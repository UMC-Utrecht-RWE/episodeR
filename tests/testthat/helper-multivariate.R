# Test helpers for the multivariate episode explosion/combine/merge SQL steps.
#
# multi_epi_1_explosion.sql reads a real hive-partitioned parquet dataset and
# builds dim_var/new_variables_ids/EXPLODED itself, so it is tested against
# small on-disk parquet fixtures (write_uni_epi_fixture() + build_dim_date()).
#
# multi_epi_2_combine.sql and multi_epi_3_mergestatus.sql are pure
# table-to-table SQL transforms, so their tests seed the expected *upstream*
# table directly (EXPLODED / multivariate_episode_coded) and skip the
# parquet + explosion machinery entirely - mirroring how
# tests/testthat/helper-univariate.R seeds each uni_epi_*.sql step in
# isolation rather than always running the full pipeline.
#
# run_step()/.load_sql()/new_test_con() come from helper-univariate.R
# (auto-loaded first by testthat, alphabetically before this file) - they
# are generic over which SQL file/params they run, so are reused as-is here.

#' Write a small hive-partitioned parquet fixture mimicking
#' D3_UNIVARIATE_EPISODES, and return the "'<dir>/**/*.parquet',
#' hive_partitioning = TRUE" fragment multi_epi_1_explosion.sql expects for
#' its {d3_univariate_episodes_path} parameter.
#'
#' @param con A DuckDB connection (used only to run the COPY).
#' @param episodes data.frame(person_id, variable_id, value, start_episode,
#'   end_episode) - start_episode/end_episode must already be Date.
#' @param dir Directory to write the hive-partitioned parquet into. Removed
#'   and recreated.
write_uni_epi_fixture <- function(con, episodes, dir) {
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "uni_epi_fixture_input", episodes, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_fixture_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      dir
    )
  )
  DBI::dbExecute(con, "DROP TABLE uni_epi_fixture_input")
  sprintf("'%s/**/*.parquet', hive_partitioning = TRUE", dir)
}

#' Build dim_date the same way multivariate_episodes_pipeline() does: one
#' row per day from MIN(start_episode) to MAX(end_episode) across the
#' *entire* univariate episodes input (computed once, before any batching).
#'
#' @param uni_epi_path_param The path fragment returned by
#'   write_uni_epi_fixture().
build_dim_date <- function(con, uni_epi_path_param) {
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE OR REPLACE TABLE dim_date AS
       SELECT unnest(generate_series(
         MIN(start_episode)::DATE, MAX(end_episode)::DATE, INTERVAL '1 day'
       )) AS dates
       FROM read_parquet(%s)",
      uni_epi_path_param
    )
  )
}

#' Seed EXPLODED (day-level person x variable rows) for testing
#' multi_epi_2_combine.sql directly, bypassing step 1.
#'
#' @param con A DuckDB connection.
#' @param rows A list of c(person_id, int_var_id, date) triples, e.g.
#'   c("p1", 1L, "2024-01-01"). Within a (person_id, date) group, list rows
#'   in ascending int_var_id order - this mirrors the guaranteed output
#'   order of multi_epi_1_explosion.sql's EXPLODED table (`ORDER BY
#'   person_id, dates, int_var_id`), which multi_epi_2_combine.sql's
#'   string_agg(int_var_id, ';') (no explicit ORDER BY) relies on for a
#'   stable, ascending combination string like "1;2".
seed_exploded <- function(con, rows) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS EXPLODED")
  values_sql <- vapply(
    rows,
    function(r) {
      sprintf("('%s', %s, DATE '%s')", r[[1]], as.integer(r[[2]]), r[[3]])
    },
    character(1)
  )
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE TABLE EXPLODED AS SELECT * FROM (VALUES %s) AS t(person_id, int_var_id, dates)",
      paste(values_sql, collapse = ",")
    )
  )
}

#' Run multi_epi_2_combine.sql against the EXPLODED table already present
#' on `con`, and return the resulting multivariate_episode table.
run_combine_sql <- function(con) {
  run_step(con, "multi_epi_2_combine.sql")
  DBI::dbGetQuery(
    con,
    "SELECT person_id, combination, start_episode, end_episode
     FROM multivariate_episode
     ORDER BY person_id, start_episode"
  )
}

#' Seed multivariate_episode_coded for testing multi_epi_3_mergestatus.sql
#' directly, bypassing steps 1-2 and the R-side dictionary encoding.
#'
#' @param con A DuckDB connection.
#' @param df data.frame(person_id, dic_index, start_episode, end_episode).
seed_episode_coded <- function(con, df) {
  DBI::dbWriteTable(con, "multivariate_episode_coded", df, overwrite = TRUE)
}

#' Run multi_epi_3_mergestatus.sql (a bare SELECT, no CREATE TABLE) against
#' multivariate_episode_coded already present on `con`, and return the
#' result as a data.table sorted for stable comparisons.
run_mergestatus_sql <- function(con) {
  sql_text <- .load_sql("multi_epi_3_mergestatus.sql")
  out <- DBI::dbGetQuery(con, sql_text)
  data.table::as.data.table(out)
}
