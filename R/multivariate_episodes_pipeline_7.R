#' Run the multivariate episodes pipeline, version 7.
#'
#' @param connection A connection to a database where the multivariate episodes pipeline will be executed.
#' @param input_directory Directory of the univariate episodes parquet files.
#' @param output_directory Directory where the output parquet files will be saved.
#' @returns NULL
#'
#' @export
multivariate_episodes_pipeline_7 <- function(
  connection,
  input_directory,
  output_directory
) {
  DBI::dbExecute(
    connection,
    glue::glue(
      "CREATE OR REPLACE TABLE dim_date AS
          SELECT
            person_id,
            int_var_id,
            start_episode::DATE AS start_episode,
            end_episode::DATE AS end_episode
          FROM read_parquet(['{input_directory}/**/*.parquet'],
            hive_partitioning = TRUE, 
            union_by_name = TRUE )"
    )
  )

  # -----------------------------------------------------------------
  # Step 1: explode intervals to daily grain, collapse to one row
  # per person/day with a bitmask combination (var_rank, EXPLODED,
  # COMBINATIONS tables).
  # -----------------------------------------------------------------
  explode <- picard::load_sql_query(
    file_path = system.file(
      package = "episodeR",
      "sql",
      "branch_explode.sql"
    )
  )
  picard::execute_sql_file(sql = explode, conn = connection)

  # -----------------------------------------------------------------
  # Step 2: collapse COMBINATIONS into multivariate_episode, with
  # gap-aware episode boundaries.
  # -----------------------------------------------------------------
  step2 <- picard::load_sql_query(
    file_path = system.file(
      package = "episodeR",
      "sql",
      "step2_collapse.sql"
    )
  )
  picard::execute_sql_file(sql = step2, conn = connection)

  # -----------------------------------------------------------------
  # Step 3: merge touching/overlapping episodes of the same
  # combination into multivariate_episode_merged.
  # -----------------------------------------------------------------
  step3 <- picard::load_sql_query(
    file_path = system.file(
      package = "episodeR",
      "sql",
      "step3_merge.sql"
    )
  )
  picard::execute_sql_file(sql = step3, conn = connection)

  # -----------------------------------------------------------------
  # Sanity check: this table should always be empty. Non-empty means
  # step 2's gap-filter missed a case -- fail loudly rather than
  # silently shipping wrong episode boundaries.
  # -----------------------------------------------------------------
  n_gap_violations <- DBI::dbGetQuery(
    connection,
    "SELECT COUNT(*) AS n FROM multivariate_episode_gap_check"
  )$n

  if (n_gap_violations > 0) {
    DBI::dbExecute(
      connection,
      glue::glue(
        "[multivariate_episode_pipeline] {n_gap_violations} gap-check 
        violation(s) found -- episodes were merged across a real gap."
      )
    )
  }

  DBI::dbExecute(
    connection,
    glue::glue(
      "COPY (
        SELECT
          person_id,
          CAST(start_episode AS DATE) AS start_episode,
          CAST(end_episode AS DATE) AS end_episode,
          * EXCLUDE (person_id, start_episode, end_episode)
        FROM D3_MULTIVARIATE_EPISODES
      ) TO '{output_directory}' (FORMAT 'parquet', OVERWRITE_OR_IGNORE TRUE)"
    )
  )
}
