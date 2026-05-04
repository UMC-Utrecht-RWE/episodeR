picard::load_config()
source(file.path("tests", "helper.R"))

testthat::test_that("Univariate episodes pipeline produces expected output", {
  data_dir <- config_test$univariate_episodes$data_dir
  start_study_date <- config_test$univariate_episodes$start_study_date
  end_study_date <- config_test$univariate_episodes$end_study_date
  end_date_missing_inclusion <- end_study_date

  testthat::expect_true(file.exists(file.path(data_dir, "D3_CONCEPTS.csv")))
  testthat::expect_true(file.exists(file.path(data_dir, "D3_SPELLS.csv")))
  testthat::expect_true(file.exists(file.path(data_dir, "study_variables.csv")))

  sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)

  sv_meta <- data.table::fread(file.path(data_dir, "study_variables.csv"))
  sv_meta$start_look_back <- abs(as.integer(sv_meta$start_look_back))
  sv_meta$end_look_back <- abs(as.integer(sv_meta$end_look_back))
  concept_ids <- unique(sv_meta$concept_id)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "D3_CONCEPTS", data.table::fread(file.path(data_dir, "D3_CONCEPTS.csv")), overwrite = TRUE)
  DBI::dbWriteTable(con, "D3_SPELLS", data.table::fread(file.path(data_dir, "D3_SPELLS.csv")), overwrite = TRUE)
  DBI::dbWriteTable(con, "study_variables", as.data.frame(sv_meta), overwrite = TRUE)
  DBI::dbExecute(con, "CREATE VIEW all_persons AS SELECT DISTINCT person_id FROM D3_SPELLS")

  # Step 1: Generate initial spells
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(sql_dir, "uni_epi_1_generate_initial_spells.sql"),
      params = list(
        concept_id_list  = paste(sprintf("'%s'", concept_ids), collapse = ", "),
        start_study_date = sprintf("'%s'", start_study_date),
        end_study_date   = sprintf("'%s'", end_date_missing_inclusion)
      )
    ),
    conn = con
  )

  # Step 2: Fill gaps between/before/after known spells
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(sql_dir, "uni_epi_2_fill_gap_spells.sql"),
      params = list(
        start_study_date = sprintf("'%s'", start_study_date),
        end_study_date   = sprintf("'%s'", end_date_missing_inclusion)
      )
    ),
    conn = con
  )

  # Step 3: Add full-period missing spells for persons with no concept data
  list_sv <- unique(sv_meta$variable_id)
  DBI::dbWriteTable(con, "list_sv", data.frame(variable_id = list_sv), overwrite = TRUE)
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(sql_dir, "uni_epi_3_add_missing_persons.sql"),
      params = list(
        start_study_date = sprintf("'%s'", start_study_date),
        end_study_date   = sprintf("'%s'", end_date_missing_inclusion)
      )
    ),
    conn = con
  )

  # Step 4: Clip spells to study period
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(sql_dir, "uni_epi_4_trim_to_study_period.sql"),
      params = list(
        start_study_date = sprintf("'%s'", start_study_date),
        end_study_date   = sprintf("'%s'", end_date_missing_inclusion)
      )
    ),
    conn = con
  )

  # Step 5: Chain-merge same-value adjacent/overlapping intervals
  picard::execute_sql_file(
    sql = picard::load_sql_query(
      file.path(sql_dir, "uni_epi_5_chain_merge_episodes.sql")
    ),
    conn = con
  )

  # Retrieve and compare to expected output
  actual <- data.table::as.data.table(
    DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  )
  actual[, spell_start := as.Date(spell_start)]
  actual[, spell_end := as.Date(spell_end)]
  data.table::setorder(actual, person_id, variable_id, spell_start)

  expected <- data.table::fread(file.path(data_dir, "D3_UNIVARIATE_EPISODES.csv"))
  expected[, spell_start := as.Date(spell_start)]
  expected[, spell_end := as.Date(spell_end)]
  data.table::setorder(expected, person_id, variable_id, spell_start)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(actual, expected)
})
