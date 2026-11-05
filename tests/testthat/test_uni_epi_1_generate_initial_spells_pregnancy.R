library(testthat)
library(data.table)

testthat::test_that("uni_epi_1_generate_initial_spells_pregnancy carries PRIOR variables into later pregnancy windows", {
# testing logic with study_variables / excluding_pregnancies = NA selects 
# standard mode, and is_prior = TRUE enables the prior-variable carry-forward logic.
#
# P1's concept on 2024-10-01 falls inside the first active pregnancy
# window, so it initially creates a TRUE episode from the concept date
# through the pregnancy end date: 2024-10-01 to 2024-12-31.
#
# Because this is the first TRUE episode for an is_prior variable, its
# value is changed to FALSE while its dates remain unchanged.
#
# The is_prior carry-forward logic then creates a TRUE episode covering
# P1's later active pregnancy window: 2025-02-01 to 2025-05-31.
#
# P1's concept on 2025-01-15 is between the two pregnancy windows and
# therefore creates no episode. P2 creates no episode because its
# pregnancy window has value = FALSE.

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(
    con,
    "D3_CONCEPTS",
    data.frame(
      person_id = c("P1", "P1", "P2"),
      concept_id = c("CONCEPT_A", "CONCEPT_A", "CONCEPT_A"),
      date = c("2024-10-01", "2025-01-15", "2024-10-01"),
      value = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )

  DBI::dbWriteTable(
    con,
    "study_variables",
    data.frame(
      variable_id = "VAR_A",
      concept_id = "CONCEPT_A",
      is_prior = TRUE,
      excluding_pregnancies = NA, # required column for uni_epi_1_generate_initial_spells_pregnancy.sql
      start_look_back = 0L,
      end_look_back = -30L,
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )

  DBI::dbWriteTable(
    con,
    "all_persons",
    data.frame(person_id = c("P1", "P2"), stringsAsFactors = FALSE),
    overwrite = TRUE
  )

  DBI::dbWriteTable(
    con,
    "pregnancy_episode_windows",
    data.frame(
      person_id = c("P1", "P1", "P2"),
      lmp_date = as.Date(c("2024-09-01", "2025-02-01", "2024-09-01")),
      pregnancy_end_date = as.Date(c("2024-12-31", "2025-05-31", "2024-12-31")),
      value = c(TRUE, TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )

  sql_root <- system.file(package = "episodeR", "sql")
  if (!nzchar(sql_root)) {
    sql_root <- file.path(getwd(), "inst", "sql")
  }

  sql <- picard::load_sql_query(
    file.path(sql_root, "uni_epi_1_generate_initial_spells_pregnancy.sql"),
    params = list(
      concept_id_list = "'CONCEPT_A'",
      start_study_date = "'2024-01-01'",
      end_study_date = "'2025-12-31'"
    )
  )

  picard::execute_sql_file(sql = sql, conn = con)

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT person_id, variable_id, value, start_episode, end_episode FROM episodes_raw"
  ))

  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- data.table::as.data.table(data.frame(
    person_id = c("P1", "P1"),
    variable_id = c("VAR_A", "VAR_A"),
    value = c("FALSE", "TRUE"),
    start_episode = as.Date(c("2024-10-01", "2025-02-01")),
    end_episode = as.Date(c("2024-12-31", "2025-05-31")),
    stringsAsFactors = FALSE
  ))

  data.table::setorder(expected, person_id, start_episode)

  testthat::expect_equal(actual, expected)
})
