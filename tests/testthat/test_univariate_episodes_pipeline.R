library(testthat)
library(yaml)
library(data.table)

config_test <- yaml::read_yaml(testthat::test_path("config_test.yaml"))
config_t3 <- yaml::read_yaml(testthat::test_path("config_T3.yaml"))

testthat::test_that("Univariate episodes pipeline produces expected output", {
  # data_dir <- config_test$univariate_episodes$data_dir
  data_dir <- testthat::test_path("data", "univariate_episodes")
  start_study_date <- config_test$univariate_episodes$start_study_date
  end_study_date <- config_test$univariate_episodes$end_study_date
  end_date_missing_inclusion <- end_study_date

  testthat::expect_true(file.exists(file.path(data_dir, "D3_CONCEPTS.csv")))
  testthat::expect_true(file.exists(file.path(data_dir, "study_variables.csv")))

  sql_dir <- system.file(package = "episodeR", "sql/")

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))
  sv_meta[, `:=`(batch, FALSE)]

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  hive_dir <- file.path(tempdir(), "univariate_episodes_hive")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  D3_SPELLS <- data.table::fread(file.path(data_dir, "D3_SPELLS.csv"))
  DBI::dbWriteTable(
    con,
    "D3_CONCEPTS",
    data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv")),
    overwrite = TRUE
  )

  univariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    person_ids = unique(D3_SPELLS$person_id),
    sql_dir = sql_dir,
    start_study_date = start_study_date,
    end_date_missing_inclusion = end_date_missing_inclusion,
    output_hive_path = hive_dir,
    batch_size = 100,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )

  # Retrieve and compare to expected output
  actual <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT person_id, variable_id, value, start_episode, end_episode FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        hive_dir
      )
    )
  )
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, variable_id, start_episode)

  expected <- data.table::fread(file.path(
    data_dir,
    "D3_UNIVARIATE_EPISODES.csv"
  ))
  expected[, start_episode := as.Date(start_episode)]
  expected[, end_episode := as.Date(end_episode)]
  data.table::setorder(expected, person_id, variable_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Univariate episodes pipeline trims concept timestamps to dates in step 1", {
  data_dir <- testthat::test_path("data", "univariate_episodes")
  start_study_date <- config_test$univariate_episodes$start_study_date
  end_study_date <- config_test$univariate_episodes$end_study_date
  end_date_missing_inclusion <- end_study_date

  sql_dir <- system.file(package = "episodeR", "sql/")

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))
  sv_meta[, `:=`(batch, FALSE)]

  concepts <- data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv"))
  concepts[, date := paste0(date, " 12:34:56")]

  expected <- data.table::fread(file.path(
    data_dir,
    "D3_UNIVARIATE_EPISODES.csv"
  ))
  expected[, start_episode := as.Date(start_episode)]
  expected[, end_episode := as.Date(end_episode)]
  data.table::setorder(expected, person_id, variable_id, start_episode)

  D3_SPELLS <- data.table::fread(file.path(data_dir, "D3_SPELLS.csv"))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  hive_dir <- file.path(data_dir, "univariate_episodes_hive_timestamps")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  DBI::dbWriteTable(
    con,
    "D3_CONCEPTS",
    concepts,
    overwrite = TRUE
  )

  episodeR::univariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    person_ids = unique(D3_SPELLS$person_id),
    sql_dir = sql_dir,
    start_study_date = start_study_date,
    end_date_missing_inclusion = end_date_missing_inclusion,
    output_hive_path = hive_dir,
    batch_size = 100,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )

  parquet_files <- list.files(
    hive_dir,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  start_types <- vapply(
    parquet_files,
    function(path) {
      arrow::read_parquet(path, as_data_frame = FALSE)$schema$GetFieldByName("start_episode")$type$ToString()
    },
    character(1)
  )
  end_types <- vapply(
    parquet_files,
    function(path) {
      arrow::read_parquet(path, as_data_frame = FALSE)$schema$GetFieldByName("end_episode")$type$ToString()
    },
    character(1)
  )
  testthat::expect_true(all(start_types == "date32[day]"))
  testthat::expect_true(all(end_types == "date32[day]"))

  actual <- data.table::as.data.table(
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT person_id, variable_id, value, start_episode, end_episode FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        hive_dir
      )
    )
  )
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, variable_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("univariate_episodes_pipeline errors when batch column is missing", {
  # data_dir <- file.path(config_test$univariate_episodes$data_dir)
  data_dir <- testthat::test_path("data", "univariate_episodes")
  sql_dir <- system.file(package = "episodeR", "sql/")

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(
    con,
    "D3_CONCEPTS",
    data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv")),
    overwrite = TRUE
  )

  testthat::expect_error(
    univariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con,
      sql_dir = sql_dir,
      start_study_date = config_test$univariate_episodes$start_study_date,
      end_date_missing_inclusion = config_test$univariate_episodes$end_study_date,
      output_hive_path = file.path(
        tempdir(),
        "univariate_episodes_hive_missing_batch"
      )
    ),
    "must include a Boolean"
  )
})
