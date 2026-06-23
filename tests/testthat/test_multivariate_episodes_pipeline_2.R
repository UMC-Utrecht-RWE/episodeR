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

  multivariate_episodes_pipeline_2(
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

testthat::test_that("multivariate_episodes_pipeline errors when batch column is missing", {
  # uni_data_dir <- config_test$univariate_episodes$data_dir
  uni_data_dir <- testthat::test_path("data", "univariate_episodes")
  # sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)
  sql_dir <- system.file(package = "episodeR", "sql/")

  sv_meta <- data.table::fread(file.path(uni_data_dir, "study_variables.csv"))
  # No batch column added - should trigger error

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  testthat::expect_error(
    multivariate_episodes_pipeline_2(
      study_variables = sv_meta,
      con = con,
      d3_univariate_episodes_path = file.path(tempdir(), "dummy_uni_hive"),
      sql_dir = sql_dir,
      output_path = file.path(tempdir(), "dummy_multi.parquet")
    ),
    "must include a Boolean"
  )
})
