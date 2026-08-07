## Tests for uni_epi_2_fill_gap_spells.sql
## Input: episodes_raw, study_variables (missing_set_to)
## Output: episodes_with_gaps

seed_episodes_raw <- function(con, episodes_raw, missing_set_to = "MISSING") {
  DBI::dbWriteTable(con, "episodes_raw", episodes_raw, overwrite = TRUE)
  DBI::dbWriteTable(
    con, "study_variables",
    data.frame(
      concept_id = "C1", variable_id = "VAR1",
      start_look_back = 5, end_look_back = 0,
      missing_set_to = missing_set_to
    ),
    overwrite = TRUE
  )
}

params <- list(start_study_date = "'2024-01-01'", end_study_date = "'2024-01-31'")

test_that("gaps are filled before, between, and after known episodes", {
  con <- new_test_con()
  seed_episodes_raw(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15")
  ))
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_with_gaps ORDER BY start_episode")

  expect_equal(nrow(out), 3)
  expect_equal(out$value, c("MISSING", "A", "MISSING"))
  expect_equal(as.Date(out$start_episode), as.Date(c("2024-01-01", "2024-01-10", "2024-01-16")))
  expect_equal(as.Date(out$end_episode), as.Date(c("2024-01-09", "2024-01-15", "2024-01-31")))
})

test_that("no before-fill is created when the first episode already starts at study start", {
  con <- new_test_con()
  seed_episodes_raw(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-15")
  ))
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_with_gaps ORDER BY start_episode")

  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("A", "MISSING"))
})

test_that("no after-fill is created when the last episode already ends at study end", {
  con <- new_test_con()
  seed_episodes_raw(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-31")
  ))
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_with_gaps ORDER BY start_episode")

  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("MISSING", "A"))
})

test_that("no fill row is created when two episodes are already adjacent (no gap)", {
  con <- new_test_con()
  seed_episodes_raw(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = c("A", "B"),
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-31"))
  ))
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_with_gaps ORDER BY start_episode")

  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("A", "B"))
})

test_that("fill value uses the variable-specific missing_set_to, and NULL is respected when unset", {
  con <- new_test_con()
  seed_episodes_raw(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15")
  ), missing_set_to = NA)
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_with_gaps ORDER BY start_episode")

  fills <- out[out$value != "A" | is.na(out$value), ]
  expect_true(all(is.na(out$value[as.Date(out$start_episode) != as.Date("2024-01-10")])))
})

test_that("gap filling is independent per (person, variable)", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_raw", data.frame(
    person_id = c("P1", "P2"),
    variable_id = c("VAR1", "VAR1"),
    value = c("A", "B"),
    start_episode = as.Date(c("2024-01-10", "2024-01-20")),
    end_episode = as.Date(c("2024-01-15", "2024-01-25"))
  ), overwrite = TRUE)
  DBI::dbWriteTable(con, "study_variables", data.frame(
    concept_id = "C1", variable_id = "VAR1",
    start_look_back = 5, end_look_back = 0, missing_set_to = "MISSING"
  ), overwrite = TRUE)

  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- DBI::dbGetQuery(con, "
    SELECT person_id, COUNT(*) AS n
    FROM episodes_with_gaps GROUP BY person_id ORDER BY person_id
  ")
  expect_equal(out$n, c(3, 3))
})
