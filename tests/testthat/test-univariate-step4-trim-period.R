# Tests for uni_epi_4_trim_to_study_period.sql
# Input/output: episodes_complete (modified in place: DELETE then UPDATE)

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

test_that("10 persons exercise deletion, clamping, and untouched cases together, including one person with a mixed in/out multi-episode set", {
  con <- new_test_con()
  episodes_complete <- rbind(
    # P1: entirely before the study period -> deleted.
    data.frame(person_id = "P1", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2023-01-01"), end_episode = as.Date("2023-06-01")),
    # P2: entirely after the study period -> deleted.
    data.frame(person_id = "P2", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-06-01"), end_episode = as.Date("2024-12-01")),
    # P3: straddles study start -> clamped start, kept.
    data.frame(person_id = "P3", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2023-12-25"), end_episode = as.Date("2024-01-05")),
    # P4: straddles study end -> clamped end, kept.
    data.frame(person_id = "P4", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-25"), end_episode = as.Date("2024-06-01")),
    # P5: spans the whole period and beyond -> clamped on both ends.
    data.frame(person_id = "P5", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2020-01-01"), end_episode = as.Date("2030-01-01")),
    # P6: fully inside -> untouched.
    data.frame(person_id = "P6", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-20")),
    # P7: exactly matches the study boundaries -> untouched.
    data.frame(person_id = "P7", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")),
    # P8: two episodes - one entirely outside (deleted), one inside (kept).
    data.frame(person_id = "P8", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2023-01-01"), end_episode = as.Date("2023-06-01")),
    data.frame(person_id = "P8", variable_id = "VAR1", value = "B",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15")),
    # P9: straddles study start on a different variable, independence check.
    data.frame(person_id = "P9", variable_id = "VAR2", value = "C",
      start_episode = as.Date("2023-12-20"), end_episode = as.Date("2024-01-02")),
    # P10: fully inside, different value, sanity check.
    data.frame(person_id = "P10", variable_id = "VAR1", value = "D",
      start_episode = as.Date("2024-01-05"), end_episode = as.Date("2024-01-08"))
  )
  DBI::dbWriteTable(con, "episodes_complete", episodes_complete, overwrite = TRUE)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params)
  out <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM episodes_complete ORDER BY person_id, start_episode"
  ))

  # P1, P2, and P8's outside episode are gone
  expect_equal(nrow(out), 8)
  expect_setequal(unique(out$person_id), c("P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"))

  expect_equal(as.Date(out[person_id == "P3"]$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out[person_id == "P3"]$end_episode), as.Date("2024-01-05"))

  expect_equal(as.Date(out[person_id == "P4"]$start_episode), as.Date("2024-01-25"))
  expect_equal(as.Date(out[person_id == "P4"]$end_episode), as.Date("2024-01-31"))

  expect_equal(as.Date(out[person_id == "P5"]$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out[person_id == "P5"]$end_episode), as.Date("2024-01-31"))

  expect_equal(as.Date(out[person_id == "P6"]$start_episode), as.Date("2024-01-10"))
  expect_equal(as.Date(out[person_id == "P6"]$end_episode), as.Date("2024-01-20"))

  expect_equal(as.Date(out[person_id == "P7"]$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out[person_id == "P7"]$end_episode), as.Date("2024-01-31"))

  p8 <- out[person_id == "P8"]
  expect_equal(nrow(p8), 1)
  expect_equal(p8$value, "B")
  expect_equal(as.Date(p8$start_episode), as.Date("2024-01-10"))
  expect_equal(as.Date(p8$end_episode), as.Date("2024-01-15"))

  p9 <- out[person_id == "P9"]
  expect_equal(p9$variable_id, "VAR2")
  expect_equal(as.Date(p9$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p9$end_episode), as.Date("2024-01-02"))

  p10 <- out[person_id == "P10"]
  expect_equal(p10$value, "D")
  expect_equal(as.Date(p10$start_episode), as.Date("2024-01-05"))
  expect_equal(as.Date(p10$end_episode), as.Date("2024-01-08"))
})
