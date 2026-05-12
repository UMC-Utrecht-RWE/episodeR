library(testthat)
library(yaml)
config_test <- read_yaml(file.path("configuration", "config_test.yaml"))
config_t3 <- read_yaml(file.path("configuration", "config_T3.yaml"))
source(file.path("univariate_episodes_pipeline.r"))

testthat::test_that("Univariate episodes pipeline produces expected output", {
  data_dir <- config_test$univariate_episodes$data_dir
  start_study_date <- config_test$univariate_episodes$start_study_date
  end_study_date <- config_test$univariate_episodes$end_study_date
  end_date_missing_inclusion <- end_study_date

  testthat::expect_true(file.exists(file.path(data_dir, "D3_CONCEPTS.csv")))
  testthat::expect_true(file.exists(file.path(data_dir, "study_variables.csv")))

  sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))
  sv_meta[, batch := FALSE]

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  hive_dir <- file.path(tempdir(), "univariate_episodes_hive")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "D3_SPELLS", data.table::fread(file.path(data_dir, "D3_SPELLS.csv")), overwrite = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv")), overwrite = TRUE)

  univariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    sql_dir = sql_dir,
    spells_table = "D3_SPELLS",
    start_study_date = start_study_date,
    end_date_missing_inclusion = end_date_missing_inclusion,
    output_hive_path = hive_dir,
    batch_size = 100,
    batch_column = "batch"
  )

  # Retrieve and compare to expected output
  actual <- data.table::as.data.table(
    DBI::dbGetQuery(con, sprintf("SELECT person_id, variable_id, value, spell_start, spell_end FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)", hive_dir))
  )
  actual[, spell_start := as.Date(spell_start)]
  actual[, spell_end := as.Date(spell_end)]
  data.table::setorder(actual, person_id, variable_id, spell_start)

  expected <- data.table::fread(file.path(data_dir, "D3_UNIVARIATE_EPISODES.csv"))
  expected[, spell_start := as.Date(spell_start)]
  expected[, spell_end := as.Date(spell_end)]
  expected[, value := as.character(value)] # Ensure value is character for comparison, adjust if expected is numeric
  data.table::setorder(expected, person_id, variable_id, spell_start)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)
})

testthat::test_that("univariate_episodes_pipeline errors when batch column is missing", {
  data_dir <- file.path(config_test$univariate_episodes$data_dir)
  sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv")), overwrite = TRUE)

  testthat::expect_error(
    univariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con,
      sql_dir = sql_dir,
      start_study_date = config_test$univariate_episodes$start_study_date,
      end_date_missing_inclusion = config_test$univariate_episodes$end_study_date,
      output_hive_path = file.path(tempdir(), "univariate_episodes_hive_missing_batch")
    ),
    "must include a Boolean"
  )
})
