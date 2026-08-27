# Tests for uni_epi_2_fill_gap_spells.sql
# Input: episodes_raw, study_variables (missing_set_to)
# Output: episodes_with_gaps

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

test_that("10 persons exercise before/between/after gap-filling, no-gap coverage, and per-variable missing_set_to together", {
  con <- new_test_con()
  episodes_raw <- rbind(
    # P1: fully inside -> before + after gaps.
    data.frame(person_id = "P1", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15")),
    # P2: already starts at study start -> after gap only.
    data.frame(person_id = "P2", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-15")),
    # P3: already ends at study end -> before gap only.
    data.frame(person_id = "P3", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-31")),
    # P4: exactly covers the full period -> no gaps at all.
    data.frame(person_id = "P4", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-31")),
    # P5: two adjacent episodes together covering the full period -> no gaps.
    data.frame(person_id = "P5", variable_id = "VAR1", value = c("A", "B"),
      start_episode = as.Date(c("2024-01-01", "2024-01-16")),
      end_episode = as.Date(c("2024-01-15", "2024-01-31"))),
    # P6: two episodes with a real internal gap -> before + between + after.
    data.frame(person_id = "P6", variable_id = "VAR1", value = c("A", "B"),
      start_episode = as.Date(c("2024-01-05", "2024-01-20")),
      end_episode = as.Date(c("2024-01-10", "2024-01-25"))),
    # P7: single one-day episode mid-period -> before + after.
    data.frame(person_id = "P7", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-15"), end_episode = as.Date("2024-01-15")),
    # P8: different variable (VAR2), different missing_set_to.
    data.frame(person_id = "P8", variable_id = "VAR2", value = "A",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15")),
    # P9: mirrors P1's shape with a different value, independence sanity check.
    data.frame(person_id = "P9", variable_id = "VAR1", value = "Z",
      start_episode = as.Date("2024-01-05"), end_episode = as.Date("2024-01-20")),
    # P10: different variable (VAR3) with missing_set_to left NA/unset.
    data.frame(person_id = "P10", variable_id = "VAR3", value = "Q",
      start_episode = as.Date("2024-01-10"), end_episode = as.Date("2024-01-15"))
  )
  DBI::dbWriteTable(con, "episodes_raw", episodes_raw, overwrite = TRUE)
  DBI::dbWriteTable(con, "study_variables", data.frame(
    concept_id = c("C1", "C2", "C3"),
    variable_id = c("VAR1", "VAR2", "VAR3"),
    start_look_back = 5, end_look_back = 0,
    missing_set_to = c("MISSING", "MISSING_2", NA)
  ), overwrite = TRUE)

  run_step(con, "uni_epi_2_fill_gap_spells.sql", params)
  out <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM episodes_with_gaps ORDER BY person_id, start_episode"
  ))

  # 3+2+2+1+2+5+3+3+3+3 rows for P1..P10
  expect_equal(nrow(out), 27)
  expect_setequal(unique(out$person_id), paste0("P", 1:10))

  expect_equal(nrow(out[person_id == "P1"]), 3)
  expect_equal(out[person_id == "P1"]$value, c("MISSING", "A", "MISSING"))

  expect_equal(nrow(out[person_id == "P2"]), 2)
  expect_equal(out[person_id == "P2"]$value, c("A", "MISSING"))

  expect_equal(nrow(out[person_id == "P3"]), 2)
  expect_equal(out[person_id == "P3"]$value, c("MISSING", "A"))

  p4 <- out[person_id == "P4"]
  expect_equal(nrow(p4), 1)
  expect_equal(p4$value, "A")

  p5 <- out[person_id == "P5"]
  expect_equal(nrow(p5), 2)
  expect_equal(p5$value, c("A", "B"))

  p6 <- out[person_id == "P6"]
  expect_equal(nrow(p6), 5)
  expect_equal(p6$value, c("MISSING", "A", "MISSING", "B", "MISSING"))
  expect_equal(as.Date(p6$start_episode), as.Date(c(
    "2024-01-01", "2024-01-05", "2024-01-11", "2024-01-20", "2024-01-26"
  )))
  expect_equal(as.Date(p6$end_episode), as.Date(c(
    "2024-01-04", "2024-01-10", "2024-01-19", "2024-01-25", "2024-01-31"
  )))

  p7 <- out[person_id == "P7"]
  expect_equal(nrow(p7), 3)
  expect_equal(p7$value, c("MISSING", "A", "MISSING"))

  p8 <- out[person_id == "P8"]
  expect_equal(nrow(p8), 3)
  expect_equal(p8$value, c("MISSING_2", "A", "MISSING_2"))

  p9 <- out[person_id == "P9"]
  expect_equal(nrow(p9), 3)
  expect_equal(p9$value, c("MISSING", "Z", "MISSING"))

  p10 <- out[person_id == "P10"]
  expect_equal(nrow(p10), 3)
  expect_true(is.na(p10$value[1]))
  expect_equal(p10$value[2], "Q")
  expect_true(is.na(p10$value[3]))
})
