##' Build Multivariate Episodes Table
##'
##' Processes D3_UNIVARIATE_EPISODES hive-partitioned parquet into a wide
##' multivariate combination table (D3_MULTIVARIATE_EPISODES), processing
##' persons in batches of batch_size and appending each batch into a single
##' accumulator table before writing the final table to parquet once.
##'
##' @param study_variables Data frame with variable metadata including
##' variable_id and a Boolean batching column.
##' @param con DBI connection used to execute SQL pipeline steps.
##' @param d3_univariate_episodes_path Directory path to the
##' D3_UNIVARIATE_EPISODES hive-partitioned parquet folder.
##' @param sql_dir Directory containing multi_epi_*.sql pipeline scripts.
##' @param output_path Full file path for the output parquet file.
##' @param person_ids Optional vector of person_ids. If NULL, derived from
##' distinct person_ids in the univariate episodes input.
##' @param batch_size Maximum number of persons per batch. Cohorts larger than
##' this are split into batches; smaller cohorts run as a single batch.
##' Defaults to 7000.
##' @param batch_column Name of a Boolean column in study_variables. Required
##' (its presence is validated); when any value is TRUE batching is forced even
##' for a small cohort. batch_size is otherwise the driver.
##' @param data_type_col Name of the column in study_variables that declares
##' the target data type for each variable (e.g. BOOL, NUM, INT, CHAR, DATE).
##' Defaults to "data_type". Set to NULL to skip type conversion.
##'
##' @return Invisibly returns NULL; writes D3_MULTIVARIATE_EPISODES parquet
##' to output_path.
#'
#' @import data.table
#' @export
multivariate_episodes_pipeline <- function(
  study_variables,
  con,
  d3_univariate_episodes_path,
  sql_dir,
  output_path,
  person_ids = NULL,
  batch_size = 7000L,
  batch_column = "batch",
  data_type_col = "data_type"
) {
  if (missing(output_path) || !nzchar(output_path)) {
    stop("output_path must be provided and non-empty.")
  }

  if (dir.exists(output_path)) {
    unlink(output_path, recursive = TRUE, force = TRUE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  if (!(batch_column %in% names(study_variables))) {
    stop(sprintf(
      "study_variables must include a Boolean '%s' column to control batching per variable.",
      batch_column
    ))
  }

  if (!("variable_id" %in% names(study_variables))) {
    stop("study_variables must include a 'variable_id' column.")
  }

  target_variable_ids <- unique(study_variables$variable_id)
  target_variable_ids <- target_variable_ids[!is.na(target_variable_ids)]
  if (length(target_variable_ids) == 0) {
    stop("study_variables has no non-missing variable_id values to process.")
  }
  logger::log_info(
    "Processing only variable_id(s) present in study_variables: {paste(sort(target_variable_ids), collapse = ', ')}"
  )
  DBI::dbWriteTable(
    con,
    "list_sv",
    data.frame(variable_id = target_variable_ids),
    overwrite = TRUE
  )

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
  do_batch <- any(use_batch)

  # Build date dimension from full univariate episodes range
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE OR REPLACE TABLE dim_date AS
     SELECT unnest(generate_series(
       MIN(start_episode)::DATE,
       MAX(end_episode)::DATE,
       INTERVAL '1 day'
     )) AS dates
     FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
      d3_univariate_episodes_path
    )
  )

  # Load SQL scripts once before batching
  uni_epi_param <- sprintf(
    "'%s/**/*.parquet', hive_partitioning = TRUE",
    d3_univariate_episodes_path
  )
  sql_explosion <- picard::load_sql_query(
    file.path(sql_dir, "multi_epi_1_explosion.sql"),
    params = list(d3_univariate_episodes_path = uni_epi_param)
  )
  sql_combine <- picard::load_sql_query(file.path(
    sql_dir,
    "multi_epi_2_combine.sql"
  ))
  sql_mergestatus <- picard::load_sql_query(file.path(
    sql_dir,
    "multi_epi_3_mergestatus.sql"
  ))

  # Resolve person_ids
  if (is.null(person_ids)) {
    person_ids <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT person_id FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        d3_univariate_episodes_path
      )
    )$person_id
    message("person_ids derived from D3_UNIVARIATE_EPISODES")
  }

  # Process batch_size persons at a time
  n_persons <- length(person_ids)
  is_batched_run <- do_batch || n_persons > batch_size
  step <- if (is_batched_run) batch_size else n_persons
  batch_starts <- seq.int(1L, n_persons, by = step)
  logger::log_info(paste("Number of batches:", length(batch_starts)))

  if (is_batched_run) {
    batch_output_dir <- file.path(
      dirname(output_path),
      "D3_MULTIVARIATE_EPISODES"
    )
    if (dir.exists(batch_output_dir)) {
      unlink(batch_output_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(batch_output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Attach each batch into a single table, then write it once
  for (i_batch in seq_along(batch_starts)) {
    logger::log_info(sprintf(
      "Processing batch %d of %d",
      i_batch,
      length(batch_starts)
    ))
    from <- batch_starts[i_batch]
    to <- min(from + step - 1L, n_persons)
    batch_episodes <- run_batch(
      ids_subset = person_ids[from:to],
      connection = con,
      sql_explosion = sql_explosion,
      sql_combine = sql_combine,
      sql_mergestatus = sql_mergestatus,
      study_variables = study_variables,
      data_type_col = data_type_col
    )

    if (is_batched_run) {
      DBI::dbWriteTable(
        con,
        "i_batch_multivariate_episodes",
        batch_episodes,
        overwrite = TRUE
      )

      batch_file <- file.path(
        batch_output_dir,
        sprintf("batch_%05d.parquet", i_batch)
      )
      batch_file_sql <- gsub("'", "''", batch_file, fixed = TRUE)

      DBI::dbExecute(
        con,
        sprintf(
          "COPY (
              SELECT
                person_id,
                CAST(start_episode AS DATE) AS start_episode,
                CAST(end_episode AS DATE) AS end_episode,
                * EXCLUDE (person_id, start_episode, end_episode)
             FROM i_batch_multivariate_episodes
             )
           TO '%s' (FORMAT 'parquet',
            ROW_GROUP_SIZE 122880)",
          batch_file_sql
        )
      )
      DBI::dbExecute(con, "DROP TABLE IF EXISTS i_batch_multivariate_episodes")
    } else {
      DBI::dbWriteTable(
        con,
        "D3_MULTIVARIATE_EPISODES",
        batch_episodes,
        append = TRUE
      )
    }
    rm(batch_episodes)
  }

  logger::log_info("Batch processing complete")

  if (!is_batched_run) {
    DBI::dbExecute(
      con,
      sprintf(
        "COPY (
            SELECT
              person_id,
              CAST(start_episode AS DATE) AS start_episode,
              CAST(end_episode AS DATE) AS end_episode,
              * EXCLUDE (person_id, start_episode, end_episode)
           FROM D3_MULTIVARIATE_EPISODES
           )
         TO '%s' (FORMAT 'parquet')",
        output_path
      )
    )
  }
}
