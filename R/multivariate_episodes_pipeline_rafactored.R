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
multivariate_episodes_pipeline_2 <- function(
  study_variables,
  con,
  d3_univariate_episodes_path,
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

  # Load SQL scripts once before batching
  uni_epi_param <- sprintf(
    "'%s/**/*.parquet', hive_partitioning = TRUE",
    d3_univariate_episodes_path
  )

  sql_initial <- picard::load_sql_query(
    file.path(sql_dir, "multi_initial.sql"),
    params = list(d3_univariate_episodes_path = uni_epi_param)
  )

  sql_build_episodes <- picard::load_sql_query(
    file.path(sql_dir, "create_multivariate_episodes.sql")
  )

  DBI::dbExecute(con, sql_initial)
  DBI::dbExecute(con, sql_build_episodes)

  logger::log_info("Batch processing complete")

  DBI::dbExecute(
    con,
    sprintf(
      "COPY (
          SELECT
            person_id,
            TRY_CAST(start_episode AS DATE) AS start_episode,
            TRY_CAST(end_episode AS DATE) AS end_episode,
            * EXCLUDE (person_id, start_episode, end_episode)
         FROM multivariate_episode_wide
         )
       TO '%s' (FORMAT 'parquet')",
      output_path
    )
  )
}
