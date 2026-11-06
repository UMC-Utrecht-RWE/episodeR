library(testthat)
library(yaml)
library(data.table)

config_test <- yaml::read_yaml(testthat::test_path("config_test.yaml"))
config_t3 <- yaml::read_yaml(testthat::test_path("config_T3.yaml"))

testthat::test_that("Multivariate episodes pipeline produces expected output", {
  # data_dir <- config_test$multivariate_episodes$data_dir
  data_dir <- testthat::test_path("data", "multivariate_episodes")
  sql_dir <- system.file(package = "episodeR", "sql/")

  testthat::expect_true(file.exists(file.path(
    data_dir,
    "D3_UNIVARIATE_EPISODES.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    data_dir,
    "D3_MULTIVARIATE_EPISODES.csv"
  )))
  testthat::expect_true(file.exists(file.path(data_dir, "study_variables.csv")))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_test_uni_hive")
  output_parquet <- file.path(
    tempdir(),
    "multi_test_D3_MULTIVARIATE_EPISODES.parquet"
  )
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  unlink(output_parquet, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(output_parquet, force = TRUE), add = TRUE)

  # Write univariate episodes CSV as hive-partitioned parquet
  uni_epi <- data.table::fread(file.path(
    data_dir,
    "D3_UNIVARIATE_EPISODES.csv"
  ))
  uni_epi[, `:=`(start_episode, as.Date(start_episode))]
  uni_epi[, `:=`(end_episode, as.Date(end_episode))]
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta[, `:=`(batch = FALSE)]

  person_ids <- unique(uni_epi$person_id)

  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_parquet,
    person_ids = person_ids,
    batch_size = 100,
    batch_column = "batch"
  )

  # check date schema, throw error if timestamp
  output_schema <- arrow::read_parquet(
    output_parquet,
    as_data_frame = FALSE
  )$schema
  testthat::expect_equal(
    output_schema$GetFieldByName("start_episode")$type$ToString(),
    "date32[day]"
  )
  testthat::expect_equal(
    output_schema$GetFieldByName("end_episode")$type$ToString(),
    "date32[day]"
  )

  # check values match expected
  actual <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM read_parquet('%s')", output_parquet)
    )
  )
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- data.table::fread(file.path(
    data_dir,
    "D3_MULTIVARIATE_EPISODES.csv"
  ))
  expected[, start_episode := as.Date(start_episode)]
  expected[, end_episode := as.Date(end_episode)]
  data.table::setorder(expected, person_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("batched run matches the single-batch expected output", {
  data_dir <- testthat::test_path("data", "multivariate_episodes")
  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_test_uni_hive_batched")
  output_parquet <- file.path(tempdir(), "multi_test_batched.parquet")
  # When batching, output_path is a directory that receives one parquet file
  # per batch instead of a single file.
  batch_output_dir <- output_parquet
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  unlink(output_parquet, recursive = TRUE, force = TRUE)
  # Simulate a file left at output_path by an earlier unbatched run; the
  # batched run must replace it with a directory.
  writeLines("stale unbatched output", output_parquet)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(output_parquet, force = TRUE), add = TRUE)
  on.exit(unlink(batch_output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  uni_epi <- data.table::fread(file.path(
    data_dir,
    "D3_UNIVARIATE_EPISODES.csv"
  ))
  uni_epi[, start_episode := as.Date(start_episode)]
  uni_epi[, end_episode := as.Date(end_episode)]
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta[, batch := FALSE]
  person_ids <- unique(uni_epi$person_id)

  # batch_size = 1 forces one batch per person, exercising the streamed union
  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_parquet,
    person_ids = person_ids,
    batch_size = 1,
    batch_column = "batch"
  )

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet('%s/*.parquet')",
      batch_output_dir
    )
  ))
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- data.table::fread(file.path(
    data_dir,
    "D3_MULTIVARIATE_EPISODES.csv"
  ))
  expected[, start_episode := as.Date(start_episode)]
  expected[, end_episode := as.Date(end_episode)]
  data.table::setorder(expected, person_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)

  # The batched run must also leave the combined result in DuckDB, as the
  # unbatched run does.
  testthat::expect_true(DBI::dbExistsTable(con, "D3_MULTIVARIATE_EPISODES"))
  actual_tbl <- data.table::as.data.table(DBI::dbReadTable(
    con,
    "D3_MULTIVARIATE_EPISODES"
  ))
  actual_tbl[, start_episode := as.Date(start_episode)]
  actual_tbl[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual_tbl, person_id, start_episode)
  data.table::setcolorder(actual_tbl, names(expected))
  testthat::expect_equal(actual_tbl, expected)
})

testthat::test_that("batched run matches unbatched run when persons have different variables", {
  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_test_uni_hive_disjoint")
  unbatched_output <- file.path(tempdir(), "multi_test_disjoint_unbatched.parquet")
  batched_output <- file.path(tempdir(), "multi_test_disjoint_batched")
  unlink(c(uni_hive_dir, unbatched_output, batched_output), recursive = TRUE, force = TRUE)
  on.exit(
    unlink(c(uni_hive_dir, unbatched_output, batched_output), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  # P1 only has VAR_A and P2 only has VAR_B, so one-person batches each lack
  # one of the variable columns. Both are Boolean so the FALSE fill converts
  # cleanly (a "FALSE" fill on a NUM variable would warn when coerced).
  uni_epi <- data.table::data.table(
    person_id = c("P1", "P1", "P2", "P2"),
    variable_id = c("VAR_A", "VAR_A", "VAR_B", "VAR_B"),
    value = c(TRUE, FALSE, TRUE, FALSE),
    start_episode = as.Date(c("2024-01-01", "2024-03-01", "2024-01-01", "2024-02-01")),
    end_episode = as.Date(c("2024-02-29", "2024-04-30", "2024-01-31", "2024-04-30"))
  )
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::data.table(
    variable_id = c("VAR_A", "VAR_B"),
    data_type = c("BOOL", "BOOL"),
    batch = FALSE
  )

  run_pipeline <- function(output_path, batch_size) {
    multivariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con,
      d3_univariate_episodes_path = uni_hive_dir,
      sql_dir = sql_dir,
      output_path = output_path,
      person_ids = c("P1", "P2"),
      batch_size = batch_size,
      batch_column = "batch"
    )
    result <- data.table::as.data.table(DBI::dbReadTable(
      con,
      "D3_MULTIVARIATE_EPISODES"
    ))
    result[, start_episode := as.Date(start_episode)]
    result[, end_episode := as.Date(end_episode)]
    data.table::setorder(result, person_id, start_episode)
    result
  }

  unbatched <- run_pipeline(unbatched_output, batch_size = 100)
  batched <- run_pipeline(batched_output, batch_size = 1)

  testthat::expect_setequal(names(batched), names(unbatched))
  data.table::setcolorder(batched, names(unbatched))
  testthat::expect_equal(batched, unbatched)

  # The per-batch parquet files carry the same columns as well
  batched_files <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet('%s/*.parquet')", batched_output)
  ))
  testthat::expect_setequal(names(batched_files), names(unbatched))
})

testthat::test_that("multivariate_episodes_pipeline errors when batch column is missing", {
  # uni_data_dir <- config_test$univariate_episodes$data_dir
  uni_data_dir <- testthat::test_path("data", "univariate_episodes")
  # sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)
  sql_dir <- system.file(package = "episodeR", "sql/")

  sv_meta <- data.table::fread(file.path(uni_data_dir, "study_variables.csv"))
  # No batch column added - should trigger error

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # A previous result at output_path must survive a failed run
  output_path <- file.path(tempdir(), "dummy_multi.parquet")
  unlink(output_path, recursive = TRUE, force = TRUE)
  writeLines("previous result", output_path)
  on.exit(unlink(output_path, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::expect_error(
    multivariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con,
      d3_univariate_episodes_path = file.path(tempdir(), "dummy_uni_hive"),
      sql_dir = sql_dir,
      output_path = output_path
    ),
    "must include a Boolean"
  )
  testthat::expect_equal(readLines(output_path), "previous result")
})
