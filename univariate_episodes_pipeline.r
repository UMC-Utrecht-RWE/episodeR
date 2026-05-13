##' Build Univariate Episodes Table
##'
##' Runs the univariate episodes SQL pipeline and materializes
##' D3_UNIVARIATE_EPISODES in the provided DuckDB connection.
##' Variables flagged for batching in study_variables are processed
##' in person_id batches; remaining variables are processed in one pass.
##'
##' @param study_variables Data frame with variable metadata including
##' concept_id, variable_id, and a Boolean batching column.
##' @param con DBI connection used to execute SQL pipeline steps.
##' @param person_ids Optional vector of person_ids to use for batching.
##' If NULL, all distinct person_ids from concepts_table will be used.
##' If provided, must be a character or numeric vector of person_ids.
##' @param concepts_table Character scalar naming the concepts source
##' table or view containing person_id. Defaults to D3_CONCEPTS.
##' @param concepts_table Character scalar naming the concepts source
##' table/view. If different from D3_CONCEPTS, it is aliased as D3_CONCEPTS.
##' @param sql_dir Directory containing uni_epi_*.sql pipeline scripts.
##' @param start_study_date Study period start date.
##' @param end_date_missing_inclusion Study period end date.
##' @param output_hive_path Directory path where parquet hive output
##' partitions will be written after each full 5-step pipeline execution.
##' @param batch_size Optional numeric batch size for person-level batching.
##' If NULL or invalid, defaults to 50000.
##' @param batch_column Name of Boolean column in study_variables
##' indicating whether each variable should be processed in batches.
##'
##' @return Invisibly returns NULL; creates/replaces
##' D3_UNIVARIATE_EPISODES in con and writes parquet output to output_hive_path.
univariate_episodes_pipeline <- function(
    study_variables,
    con,
    person_ids = NULL,
    concepts_table = "D3_CONCEPTS",
    sql_dir,
    start_study_date,
    end_date_missing_inclusion,
    output_hive_path,
    batch_size = 5000L,
    batch_column = "batch") {
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
  if (concepts_table != "D3_CONCEPTS") {
    concepts_table_sql <- as.character(DBI::dbQuoteIdentifier(con, concepts_table))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW D3_CONCEPTS AS SELECT * FROM %s",
      concepts_table_sql
    ))
  }
  batch_values <- study_variables[[batch_column]]
  if (is.logical(batch_values)) {
    use_batch <- batch_values
  } else {
    normalized <- tolower(trimws(as.character(batch_values)))
    use_batch <- normalized %in% c("true", "t", "1", "yes", "y")
    invalid_batch_values <- !(normalized %in% c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n", "", "na"))
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
    DBI::dbWriteTable(con, "list_sv", data.frame(variable_id = unique(sv_subset$variable_id)), overwrite = TRUE)

    DBI::dbExecute(con, sprintf("CREATE OR REPLACE VIEW all_persons AS %s", person_filter_query))

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "uni_epi_1_generate_initial_spells.sql"),
        params = c(
          list(concept_id_list = paste(sprintf("'%s'", concept_ids), collapse = ", ")),
          params_common
        )
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "uni_epi_2_fill_gap_spells.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "uni_epi_3_add_missing_persons.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "uni_epi_4_trim_to_study_period.sql"),
        params = params_common
      ),
      conn = con
    )

    picard::execute_sql_file(
      sql = picard::load_sql_query(
        file.path(sql_dir, "uni_epi_5_chain_merge_episodes.sql")
      ),
      conn = con
    )

    DBI::dbExecute(con, sprintf(
      "COPY D3_UNIVARIATE_EPISODES TO '%s'
      (FORMAT PARQUET, PARTITION_BY (variable_id), APPEND TRUE);",
      output_hive_path
    ))
  }


  if (!is.null(person_ids)) {
    table_person_ids <- data.table::data.table(person_id = person_ids)
    DBI::dbWriteTable(con, "table_person_ids", table_person_ids, overwrite = TRUE)
    person_filter_query <- sprintf("SELECT DISTINCT person_id FROM table_person_ids")
  } else {
    person_filter_query <- sprintf("SELECT DISTINCT person_id FROM %s", concepts_table)
  }

  run_univariate_pipeline(
    sv_non_batch,
    person_filter_query = person_filter_query,
    output_hive_path
  )

  if (nrow(sv_batch) > 0) {
    total_persons <- length(person_ids)

    if (total_persons > 0) {
      if (is.null(person_ids)) {
        all_persons_query <- sprintf("SELECT DISTINCT person_id FROM %s", concepts_table)
        person_ids <- DBI::dbGetQuery(con, all_persons_query)$person_id
      }
      batch_ids <- split(person_ids, ceiling(seq_along(person_ids) / batch_size))
      for (i_batch in seq_along(batch_ids)) {
        logger::log_info("Processing batch number {i_batch} of {length(batch_ids)}")
        ids <- batch_ids[[i_batch]]
        ids_df <- data.frame(person_id = ids, stringsAsFactors = FALSE)
        DBI::dbWriteTable(con, "batch_person_ids", ids_df, overwrite = TRUE)
        run_univariate_pipeline(
          sv_subset = sv_batch,
          person_filter_query = "SELECT person_id FROM batch_person_ids",
          output_hive_path
        )
      }
    }
  }
}
