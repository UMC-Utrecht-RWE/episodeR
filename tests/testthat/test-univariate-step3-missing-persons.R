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

test_that("10 persons across all 4 (has VAR1, has VAR2) coverage combinations each end up with exactly one row per variable", {
  con <- new_test_con()
  full_period <- function(person_id, variable_id, value) {
    data.frame(
      person_id = person_id, variable_id = variable_id, value = value,
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")
    )
  }
  episodes_with_gaps <- rbind(
    # P1, P5, P9: coverage for both variables -> nothing filled.
    full_period("P1", "VAR1", "A1"), full_period("P1", "VAR2", "B1"),
    full_period("P5", "VAR1", "A5"), full_period("P5", "VAR2", "B5"),
    full_period("P9", "VAR1", "A9"), full_period("P9", "VAR2", "B9"),
    # P2, P6: VAR1 coverage only -> VAR2 gets filled.
    full_period("P2", "VAR1", "A2"),
    full_period("P6", "VAR1", "A6"),
    # P3, P8: VAR2 coverage only -> VAR1 gets filled.
    full_period("P3", "VAR2", "B3"),
    full_period("P8", "VAR2", "B8")
    # P4, P7, P10: no coverage at all -> both variables get filled.
  )
  seed_step3(
    con,
    episodes_with_gaps = episodes_with_gaps,
    persons = paste0("P", 1:10),
    variables = c("VAR1", "VAR2"),
    missing_values = c("MISSING_1", "MISSING_2")
  )
  run_step(con, "uni_epi_3_add_missing_persons.sql", params)
  out <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM episodes_complete ORDER BY person_id, variable_id"
  ))

  # Every one of the 10 persons ends up with exactly one row per variable.
  expect_equal(nrow(out), 20)
  counts <- out[, .N, by = .(person_id, variable_id)]
  expect_true(all(counts$N == 1))
  expect_setequal(unique(out$person_id), paste0("P", 1:10))

  # "Both" persons pass through untouched.
  expect_equal(out[person_id == "P1" & variable_id == "VAR1"]$value, "A1")
  expect_equal(out[person_id == "P1" & variable_id == "VAR2"]$value, "B1")
  expect_equal(out[person_id == "P9" & variable_id == "VAR1"]$value, "A9")

  # VAR1-only persons get VAR2 filled with the VAR2-specific missing value.
  expect_equal(out[person_id == "P2" & variable_id == "VAR1"]$value, "A2")
  expect_equal(out[person_id == "P2" & variable_id == "VAR2"]$value, "MISSING_2")
  expect_equal(out[person_id == "P6" & variable_id == "VAR2"]$value, "MISSING_2")

  # VAR2-only persons get VAR1 filled with the VAR1-specific missing value.
  expect_equal(out[person_id == "P3" & variable_id == "VAR1"]$value, "MISSING_1")
  expect_equal(out[person_id == "P3" & variable_id == "VAR2"]$value, "B3")
  expect_equal(out[person_id == "P8" & variable_id == "VAR1"]$value, "MISSING_1")

  # Fully-absent persons get both variables filled, spanning the full period.
  for (p in c("P4", "P7", "P10")) {
    rows <- out[person_id == p]
    expect_equal(nrow(rows), 2)
    expect_setequal(rows$value, c("MISSING_1", "MISSING_2"))
    expect_true(all(as.Date(rows$start_episode) == as.Date("2024-01-01")))
    expect_true(all(as.Date(rows$end_episode) == as.Date("2024-01-31")))
  }
})
