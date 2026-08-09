##' Build Univariate Episodes Table
##'
##' Runs the univariate episodes SQL pipeline and materializes
##' D3_UNIVARIATE_EPISODES in the provided DuckDB connection, using a
##' window-function ("gaps and islands") interval merge instead of the
##' self-join + NOT EXISTS merge [univariate_episodes_pipeline()] uses, and
##' filtering concept_dedup down to the concept_ids actually in use before
##' deduplicating rather than after.
##'
##' @section Batching:
##' A variable flagged via `batch_column` is always processed in person_id
##' batches. Variables not flagged that way are still batched once the
##' cohort exceeds `batch_size` persons (mirroring
##' [multivariate_episodes_pipeline()]'s `n_persons > batch_size` safety
##' net) and processed in a single pass otherwise.
##'
##' @param study_variables Data frame with variable metadata including
##' concept_id, variable_id, and a Boolean batching column.
##' @param con DBI connection used to execute SQL pipeline steps.
##' @param person_ids Optional vector of person_ids to use for batching.
##' If NULL, all distinct person_ids from concepts_table will be used.
##' If provided, must be a character or numeric vector of person_ids.
##' @param concepts_table Character scalar naming the concepts source
##' table/view. If different from D3_CONCEPTS, it is aliased as D3_CONCEPTS.
##' @param sql_dir Directory containing create_univariate_episodes_*.sql
##' pipeline scripts.
##' @param start_study_date Study period start date.
##' @param end_date_missing_inclusion Study period end date.
##' @param output_hive_path Directory path where parquet hive output
##' partitions will be written after each full 5-step pipeline execution.
##' @param batch_size Maximum number of persons per batch. Cohorts larger than
##' this are split into batches, even for variables not flagged via
##' batch_column; smaller cohorts run as a single pass. Defaults to 5000.
##' @param batch_column Name of Boolean column in study_variables
##' indicating whether a variable should always be processed in batches.
##' batch_size is otherwise the driver: a cohort larger than batch_size is
##' batched regardless of this flag.
##' @param missing_col Optional name of a column in study_variables whose
##' values are used as the fill value for periods with no recorded concept
##' (gap fills in step 2 and missing-person rows in step 3). The column is
##' exposed to the SQL pipeline as \code{missing_set_to}.
##' If \code{NULL} (default) the fill value is set to \code{NA} (NULL in
##' DuckDB) for every variable. If a column name is given and that column
##' is not present in \code{study_variables} an error is raised.
##'
##' @return Invisibly returns NULL; creates/replaces
##' D3_UNIVARIATE_EPISODES in con and writes parquet output to output_hive_path.
#'
#' @import data.table
#' @export
create_univariate_episodes <- function(
    study_variables,
    con,
    person_ids = NULL,
    concepts_table = "D3_CONCEPTS",
    sql_dir,
    start_study_date,
    end_date_missing_inclusion,
    output_hive_path,
    batch_size = 5000L,
    batch_column = "batch",
    missing_col = NULL) {
  if (missing(output_hive_path) || !nzchar(output_hive_path)) {
    stop("output_hive_path must be provided and non-empty.")
  }
  dir.create(output_hive_path, recursive = TRUE, showWarnings = FALSE)

  if (!(batch_column %in% names(study_variables))) {
    stop(sprintf(
      "study_variables must include a Boolean '%s' column to control batching per variable.",
      batch_column
    ))
  }

  # Resolve missing_set_to column.
  # If missing_col is NULL, fill with NA (becomes NULL in DuckDB).
  # If missing_col is provided, copy that column to missing_set_to so the
  # SQL pipeline always finds a column with that exact name.
  if (is.null(missing_col)) {
    study_variables$missing_set_to <- NA
    logger::log_info(
      "No missing_col provided - gap-filled periods will have NULL value in output."
    )
  } else {
    if (!(missing_col %in% names(study_variables))) {
      stop(sprintf(
        "Column '%s' specified by missing_col was not found in study_variables.",
        missing_col
      ))
    }
    study_variables$missing_set_to <- study_variables[[missing_col]]
  }
  if (concepts_table != "D3_CONCEPTS") {
    concepts_table_sql <- as.character(DBI::dbQuoteIdentifier(
      con,
      concepts_table
    ))
    DBI::dbExecute(
      con,
      sprintf(
        "CREATE OR REPLACE VIEW D3_CONCEPTS AS SELECT * FROM %s",
        concepts_table_sql
      )
    )
  }
  batch_values <- study_variables[[batch_column]]
  if (is.logical(batch_values)) {
    use_batch <- batch_values
  } else {
    normalized <- tolower(trimws(as.character(batch_values)))
    use_batch <- normalized %in% c("true", "t", "1", "yes", "y")
    invalid_batch_values <- !(normalized %in%
      c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n", "", "na"))
    if (any(invalid_batch_values, na.rm = TRUE)) {
      stop(sprintf(
        "Column '%s' must contain only Boolean-like values (TRUE/FALSE, 1/0, yes/no).",
        batch_column
      ))
    }
  }
  use_batch[is.na(use_batch)] <- FALSE

  sv_non_batch <- study_variables[!use_batch, , drop = FALSE]
  sv_batch <- study_variables[use_batch, , drop = FALSE]

  # Resolve person_ids up front so cohort size alone can force batching,
  # mirroring multivariate_episodes_pipeline()'s
  # `is_batched_run <- do_batch || n_persons > batch_size`: a cohort larger
  # than batch_size is batched even when no variable requests it via
  # batch_column.
  if (is.null(person_ids)) {
    person_ids <- DBI::dbGetQuery(
      con,
      sprintf("SELECT DISTINCT person_id FROM %s", concepts_table)
    )$person_id
    message("person_ids derived from ", concepts_table)
  }
  total_persons <- length(person_ids)
  force_batch_by_size <- total_persons > batch_size

  params_common <- list(
    start_study_date = sprintf("'%s'", as.character(start_study_date)),
    end_study_date = sprintf("'%s'", as.character(end_date_missing_inclusion))
  )
  run_univariate_pipeline <- function(sv_subset,
                                      person_filter_query,
                                      output_hive_path) {
    if (nrow(sv_subset) == 0) {
      return()
    }

    concept_ids <- unique(sv_subset$concept_id)
    concept_ids <- concept_ids[!is.na(concept_ids)]
    if (length(concept_ids) == 0) {
      return()
    }

    DBI::dbWriteTable(con, "study_variables", sv_subset, overwrite = TRUE)
    DBI::dbWriteTable(
      con,
      "list_sv",
      data.frame(variable_id = unique(sv_subset$variable_id)),
      overwrite = TRUE
    )

    DBI::dbExecute(
      con,
      sprintf("CREATE OR REPLACE VIEW all_persons AS %s", person_filter_query)
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "create_univariate_episodes_1_generate_initial_spells.sql"),
        params = c(
          list(
            concept_id_list = paste(
              sprintf("'%s'", concept_ids),
              collapse = ", "
            )
          ),
          params_common
        )
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "create_univariate_episodes_2_fill_gap_spells.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "create_univariate_episodes_3_add_missing_persons.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "create_univariate_episodes_4_trim_to_study_period.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "create_univariate_episodes_5_chain_merge_episodes.sql")
      ),
      conn = con
    )

    DBI::dbExecute(
      con,
      sprintf(
        "COPY D3_UNIVARIATE_EPISODES TO '%s'
        (FORMAT PARQUET, PARTITION_BY (variable_id), APPEND TRUE);",
        output_hive_path
      )
    )
  }

  # Run sv_subset either as one pass over all persons, or split into
  # batch_size person chunks when do_batch_subset is TRUE. Shared by both
  # the non-batch-flagged and batch-flagged variable subsets so cohort-size
  # batching applies uniformly instead of only to explicitly flagged
  # variables.
  run_variable_subset <- function(sv_subset, do_batch_subset) {
    if (nrow(sv_subset) == 0 || total_persons == 0) {
      return(invisible(NULL))
    }

    if (!do_batch_subset) {
      table_person_ids <- data.table::data.table(person_id = person_ids)
      DBI::dbWriteTable(
        con,
        "table_person_ids",
        table_person_ids,
        overwrite = TRUE
      )
      run_univariate_pipeline(
        sv_subset,
        person_filter_query = "SELECT DISTINCT person_id FROM table_person_ids",
        output_hive_path
      )
      return(invisible(NULL))
    }

    batch_ids <- split(person_ids, ceiling(seq_along(person_ids) / batch_size))
    for (i_batch in seq_along(batch_ids)) {
      logger::log_info(
        "Processing batch number {i_batch} of {length(batch_ids)}"
      )
      ids <- batch_ids[[i_batch]]
      ids_df <- data.frame(person_id = ids, stringsAsFactors = FALSE)
      DBI::dbWriteTable(con, "batch_person_ids", ids_df, overwrite = TRUE)
      run_univariate_pipeline(
        sv_subset = sv_subset,
        person_filter_query = "SELECT person_id FROM batch_person_ids",
        output_hive_path
      )
    }
    invisible(NULL)
  }

  # Variables not flagged via batch_column only batch when the cohort size
  # forces it; flagged variables (sv_batch) always batch, as before.
  run_variable_subset(sv_non_batch, force_batch_by_size)
  run_variable_subset(sv_batch, TRUE)
}
