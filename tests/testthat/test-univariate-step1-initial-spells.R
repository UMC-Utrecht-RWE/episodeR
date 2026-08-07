## Tests for uni_epi_1_generate_initial_spells.sql
##
## Produces: concept_dedup, trimmed_episodes, episodes_raw
##
## NOTE on look-back column naming (verified against the SQL, not assumed):
##   start_episode = concept.date + end_look_back
##   end_episode   = concept.date + start_look_back
## i.e. `start_look_back` controls how far the EPISODE END reaches forward
## from the record date, and `end_look_back` controls the EPISODE START.
## This is counter-intuitive naming but is the current, verified behaviour;
## all fixtures below are written to match it explicitly.

test_that("a single concept record becomes one episode spanning its look-back window", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1", date = "2024-01-10", value = "A"
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 30, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-03-01'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_raw")
  expect_equal(nrow(out), 1)
  expect_equal(out$value, "A")
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-10"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-02-09"))
})

test_that("two same-day records with different values collapse to a single 'unknown' record", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1", date = "2024-01-05",
    value = c("A", "B")
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 5, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  dedup <- DBI::dbGetQuery(con, "SELECT * FROM concept_dedup")
  expect_equal(nrow(dedup), 1)
  expect_equal(dedup$value, "unknown")
})

test_that("two same-day records with the SAME value dedup cleanly (no 'unknown')", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1", date = "2024-01-05",
    value = c("A", "A")
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 5, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  dedup <- DBI::dbGetQuery(con, "SELECT * FROM concept_dedup")
  expect_equal(nrow(dedup), 1)
  expect_equal(dedup$value, "A")
})

test_that("a later record's episode crops the end of the preceding overlapping episode (MRR resolution)", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1",
    date = c("2024-01-01", "2024-01-10"),
    value = c("A", "B")
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 30, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-03-01'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_raw ORDER BY start_episode")
  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("A", "B"))
  # A's window would naturally run to 2024-01-31, but B starts 2024-01-10,
  # so A must be cropped to end the day before B starts.
  expect_equal(as.Date(out$end_episode[1]), as.Date("2024-01-09"))
  expect_equal(as.Date(out$start_episode[2]), as.Date("2024-01-10"))
  expect_equal(as.Date(out$end_episode[2]), as.Date("2024-02-09"))
})

test_that("a record whose window falls entirely before the study period is dropped from trimmed_episodes", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1", date = "2023-01-01", value = "A"
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 5, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM trimmed_episodes")
  expect_equal(nrow(out), 0)
})

test_that("a record whose window straddles the study start is clamped, not dropped", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1", date = "2023-12-30", value = "A"
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 5, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM trimmed_episodes")
  expect_equal(nrow(out), 1)
  # window is [2023-12-30, 2024-01-04]; clamped start should be study start
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-04"))
})

test_that("chain-merge in step 1 collapses overlapping same-value trimmed episodes into one", {
  # Two concept records, close enough together that their look-back windows
  # overlap AND (after MRR resolution) still share the same value -> should
  # already appear as one merged row in episodes_raw.
  con <- new_test_con()
  concepts <- data.frame(
    person_id = "P1", concept_id = "C1",
    date = c("2024-01-01", "2024-01-05"),
    value = c("A", "A")
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 10, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = "P1")

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_raw")
  expect_equal(nrow(out), 1)
  expect_equal(out$value, "A")
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-15"))
})

test_that("different persons and different variables never mix in dedup or MRR resolution", {
  con <- new_test_con()
  concepts <- data.frame(
    person_id = c("P1", "P2"),
    concept_id = c("C1", "C1"),
    date = c("2024-01-05", "2024-01-05"),
    value = c("A", "B")
  )
  sv <- sv_row("C1", "VAR1", start_look_back = 5, end_look_back = 0)
  seed_inputs(con, concepts, sv, persons = c("P1", "P2"))

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-01-31'"
  ))

  out <- DBI::dbGetQuery(con, "SELECT * FROM episodes_raw ORDER BY person_id")
  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("A", "B"))
})
