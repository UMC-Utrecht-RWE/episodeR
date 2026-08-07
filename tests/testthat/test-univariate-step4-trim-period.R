## Tests for uni_epi_4_trim_to_study_period.sql
## Input/output: episodes_complete (modified in place: DELETE then UPDATE)

params <- list(start_study_date = "'2024-01-01'", end_study_date = "'2024-01-31'")

test_that("an episode entirely outside the study period is deleted", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_complete", data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2023-01-01"), end_episode = as.Date("2023-06-01")
  ), overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 0)
})

test_that("an episode straddling the study start is clamped, not deleted", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_complete", data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2023-12-25"), end_episode = as.Date("2024-01-05")
  ), overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-05"))
})

test_that("an episode straddling the study end is clamped, not deleted", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_complete", data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-25"), end_episode = as.Date("2024-06-01")
  ), overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-25"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-31"))
})

test_that("an episode spanning the entire study period (and beyond) is clamped on both ends", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_complete", data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2020-01-01"), end_episode = as.Date("2030-01-01")
  ), overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-31"))
})

test_that("an episode fully inside the study period is left untouched", {
  con <- new_test_con()
  DBI::dbWriteTable(con, "episodes_complete", data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-20")
  ), overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_complete")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-10"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-20"))
})
