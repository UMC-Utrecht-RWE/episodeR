## Tests for uni_epi_3_add_missing_persons.sql
## Input: episodes_with_gaps, all_persons, list_sv, study_variables
## Output: episodes_complete

params <- list(start_study_date = "'2024-01-01'", end_study_date = "'2024-01-31'")

seed_step3 <- function(con, episodes_with_gaps, persons, variables, missing_values) {
  DBI::dbWriteTable(con, "episodes_with_gaps", episodes_with_gaps, overwrite = TRUE)
  DBI::dbWriteTable(
    con, "all_persons",
    data.frame(person_id = persons, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con, "list_sv",
    data.frame(variable_id = variables, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con, "study_variables",
    data.frame(
      concept_id = paste0("C_", variables), variable_id = variables,
      start_look_back = 5, end_look_back = 0,
      missing_set_to = missing_values,
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )
}

test_that("a person entirely absent from the concept data gets one full-period row", {
  con <- new_test_con()
  seed_step3(
    con,
    episodes_with_gaps = data.frame(
      person_id = "P1", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")
    ),
    persons = c("P1", "P2"),
    variables = "VAR1",
    missing_values = "MISSING"
  )
  run_step(con, "uni_epi_3_add_missing_persons.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete ORDER BY person_id")

  expect_equal(nrow(out), 2)
  p2 <- out[out$person_id == "P2", ]
  expect_equal(p2$value, "MISSING")
  expect_equal(as.Date(p2$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p2$end_episode), as.Date("2024-01-31"))
})

test_that("a person present for one variable but not another only gets filled for the missing one", {
  con <- new_test_con()
  seed_step3(
    con,
    episodes_with_gaps = data.frame(
      person_id = "P1", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")
    ),
    persons = "P1",
    variables = c("VAR1", "VAR2"),
    missing_values = c("MISSING_1", "MISSING_2")
  )
  run_step(con, "uni_epi_3_add_missing_persons.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete ORDER BY variable_id")

  expect_equal(nrow(out), 2)
  expect_equal(out$value[out$variable_id == "VAR1"], "A")
  expect_equal(out$value[out$variable_id == "VAR2"], "MISSING_2")
})

test_that("no extra row is added when every (person, variable) pair already has coverage", {
  con <- new_test_con()
  seed_step3(
    con,
    episodes_with_gaps = data.frame(
      person_id = c("P1", "P2"), variable_id = "VAR1", value = c("A", "B"),
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")
    ),
    persons = c("P1", "P2"),
    variables = "VAR1",
    missing_values = "MISSING"
  )
  run_step(con, "uni_epi_3_add_missing_persons.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 2)
})
