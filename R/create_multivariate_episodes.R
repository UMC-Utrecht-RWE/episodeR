##' Build Multivariate Episodes Table
##'
##' Processes D3_UNIVARIATE_EPISODES hive-partitioned parquet into a wide
##' multivariate combination table (D3_MULTIVARIATE_EPISODES) in a single
##' fast SQL pass over the whole cohort.
##'
##' @section Batching is a safety net, not the default path:
##' The single-pass SQL below holds the whole cohort's working set in
##' memory, which is fast but can exhaust memory for very large cohorts
##' (see the scaling notes on [multivariate_episodes_pipeline()]). Batching
##' exists only as a fallback for that case: it activates when a
##' `study_variables[[batch_column]]` flag is TRUE or when the cohort
##' exceeds `batch_size` persons, and is slower than the single-pass path
##' because it re-reads the input per batch. Prefer sizing `batch_size` up
##' (or not batching at all) whenever the cohort fits in memory.
##'
##' @param study_variables Data frame with variable metadata including
##' variable_id and a Boolean batching column.
##' @param con DBI connection used to execute SQL pipeline steps.
##' @param d3_univariate_episodes_path Directory path to the
##' D3_UNIVARIATE_EPISODES hive-partitioned parquet folder.
##' @param output_path Directory path for the output. Created (any existing
##' contents cleared first) and populated with one parquet file per batch
##' (batch_00001.parquet, ...) -- a single file when the batching safety net
##' didn't trigger, several when it did.
##' @param person_ids Optional vector of person_ids. If NULL, derived from
##' distinct person_ids in the univariate episodes input.
##' @param batch_size Maximum number of persons per batch before the
##' batching safety net kicks in. Defaults to 5000 -- benchmarking on a
##' 100K-person/10-variable cohort found this the fastest value tested:
##' smaller batch sizes pay a large per-batch input re-scan cost, while
##' larger ones (50000+) push the wide combination table into memory
##' pressure and can fail outright on large cohorts.
##' @param batch_column Name of a Boolean column in study_variables. Required
##' (its presence is validated); when any value is TRUE batching is forced even
##' for a small cohort. batch_size is otherwise the driver.
##' @param data_type_col Name of the column in study_variables that declares
##' the target data type for each variable (e.g. BOOL, NUM, INT, CHAR, DATE).
##' Currently unused by this function; accepted for interface parity with
##' [multivariate_episodes_pipeline()].
##'
##' @return Invisibly returns NULL; writes D3_MULTIVARIATE_EPISODES parquet
##' file(s) into the output_path directory.
#'
#' @import data.table
#' @export
create_multivariate_episodes <- function(
  study_variables,
  con,
  d3_univariate_episodes_path,
  output_path,
  person_ids = NULL,
  batch_size = 5000L,
  batch_column = "batch",
  data_type_col = "data_type"
) {
  if (missing(output_path) || !nzchar(output_path)) {
    stop("output_path must be provided and non-empty.")
  }
  validate_connection(con)
  validate_data_frame(study_variables, "study_variables")
  validate_batch_size(batch_size)
  validate_required_columns(study_variables, "variable_id")
  use_batch <- validate_batch_column(study_variables, batch_column)

  if (dir.exists(output_path)) {
    unlink(output_path, recursive = TRUE, force = TRUE)
  }

  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

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

  uni_epi_param <- sprintf(
    "'%s/**/*.parquet', hive_partitioning = TRUE",
    d3_univariate_episodes_path
  )

  sql_build_episodes <- read_sql("create_multivariate_episodes_2_combine_and_pivot.sql")

  # Batching is a safety net for cohorts too large to fit in memory in one
  # pass -- not the default path. It only activates when a
  # study_variables[[batch_column]] flag is TRUE or the cohort exceeds
  # batch_size persons; otherwise the whole cohort runs through the single
  # fast SQL pass below, unchanged from before batching support existed.
  do_batch <- any(use_batch)

  if (is.null(person_ids)) {
    person_ids <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT person_id FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        d3_univariate_episodes_path
      )
    )$person_id
  }
  n_persons <- length(person_ids)
  if (n_persons == 0) {
    logger::log_info("No persons to process; skipping.")
    return(invisible(NULL))
  }
  is_batched_run <- do_batch || n_persons > batch_size

  if (is_batched_run) {
    logger::log_warn(sprintf(
      paste(
        "create_multivariate_episodes(): %d persons exceeds batch_size (%d)",
        "or batching was requested via '%s' -- falling back to the batched",
        "safety net. This re-reads the input per batch and is slower than",
        "the single-pass path; prefer raising batch_size when the cohort",
        "fits in memory."
      ),
      n_persons, batch_size, batch_column
    ))
  }

  sql_initial <- glue::glue(
    read_sql("create_multivariate_episodes_1_encode_variables.sql"),
    d3_univariate_episodes_path = uni_epi_param
  )

  # An unbatched run is just a single batch covering every person: step
  # spans the whole cohort in one go, so the loop below runs once.
  step <- if (is_batched_run) batch_size else n_persons
  batch_starts <- seq.int(1L, n_persons, by = step)

  for (i_batch in seq_along(batch_starts)) {
    if (is_batched_run) {
      logger::log_info(sprintf("Processing batch %d of %d", i_batch, length(batch_starts)))
    }
    from <- batch_starts[i_batch]
    to <- min(from + step - 1L, n_persons)

    DBI::dbWriteTable(
      con,
      "i_batch_persons",
      data.frame(person_id = person_ids[from:to], stringsAsFactors = FALSE),
      overwrite = TRUE
    )

    DBI::dbExecute(con, sql_initial)
    DBI::dbExecute(con, sql_build_episodes)
    write_batch_output(con, output_path, i_batch)
  }

  logger::log_info("Batch processing complete")
}
