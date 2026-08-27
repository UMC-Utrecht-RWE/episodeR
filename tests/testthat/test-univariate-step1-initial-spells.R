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

test_that("10 persons exercise dedup, MRR cropping, drop/clamp, chain-merge, and multi-variable independence together", {
  # One combined fixture standing in for the narrow single-behavior tests
  # above, run across 10 person_ids at once - it catches cross-person
  # bleed-over (e.g. accidental grouping by date/value alone instead of by
  # person_id, variable_id) that isolated 1-2-person tests can't expose.
  # VAR1/C1 and VAR2/C2 both use a look-back of 10 days
  # (start_episode = date, end_episode = date + 10; per the header note,
  # end_look_back=0 controls the start, start_look_back=10 controls the end).
  con <- new_test_con()
  concepts <- rbind(
    # P1: single record -> one 11-day episode.
    data.frame(person_id = "P1", concept_id = "C1", date = "2024-01-10", value = "A"),
    # P2: same-day different values -> dedup collapses to 'unknown'.
    data.frame(person_id = "P2", concept_id = "C1", date = c("2024-01-05", "2024-01-05"), value = c("A", "B")),
    # P3: same-day identical values -> clean dedup, no 'unknown'.
    data.frame(person_id = "P3", concept_id = "C1", date = c("2024-01-05", "2024-01-05"), value = c("A", "A")),
    # P4: later record crops the earlier one's end (MRR resolution).
    data.frame(person_id = "P4", concept_id = "C1", date = c("2024-01-01", "2024-01-05"), value = c("A", "B")),
    # P5: window entirely before the study period -> dropped, 0 rows.
    data.frame(person_id = "P5", concept_id = "C1", date = "2023-01-01", value = "A"),
    # P6: window straddles study start -> clamped, not dropped.
    data.frame(person_id = "P6", concept_id = "C1", date = "2023-12-28", value = "A"),
    # P7: overlapping same-value records chain-merge into one episode.
    data.frame(person_id = "P7", concept_id = "C1", date = c("2024-01-01", "2024-01-05"), value = c("A", "A")),
    # P8: two different variables for the same person stay independent.
    data.frame(person_id = "P8", concept_id = "C1", date = "2024-01-01", value = "A"),
    data.frame(person_id = "P8", concept_id = "C2", date = "2024-01-01", value = "X"),
    # P9: two far-apart records, same variable/value -> no merge (gap too large).
    data.frame(person_id = "P9", concept_id = "C1", date = c("2024-01-01", "2024-02-01"), value = c("A", "A")),
    # P10: independent sanity check, distinct date/value from P1.
    data.frame(person_id = "P10", concept_id = "C1", date = "2024-02-15", value = "Z")
  )
  sv <- rbind(
    sv_row("C1", "VAR1", start_look_back = 10, end_look_back = 0),
    sv_row("C2", "VAR2", start_look_back = 10, end_look_back = 0)
  )
  persons <- paste0("P", 1:10)
  seed_inputs(con, concepts, sv, persons = persons)

  run_step(con, "uni_epi_1_generate_initial_spells.sql", list(
    concept_id_list = "'C1', 'C2'",
    start_study_date = "'2024-01-01'",
    end_study_date = "'2024-03-01'"
  ))

  out <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM episodes_raw ORDER BY person_id, variable_id, start_episode"
  ))

  # P5 is fully dropped; everyone else contributes at least one row.
  expect_setequal(unique(out$person_id), setdiff(persons, "P5"))
  expect_equal(nrow(out), 12) # 1+1+1+2+0+1+1+2+2+1 across P1..P10

  p1 <- out[person_id == "P1"]
  expect_equal(nrow(p1), 1)
  expect_equal(p1$value, "A")
  expect_equal(as.Date(p1$start_episode), as.Date("2024-01-10"))
  expect_equal(as.Date(p1$end_episode), as.Date("2024-01-20"))

  expect_equal(out[person_id == "P2"]$value, "unknown")
  expect_equal(out[person_id == "P3"]$value, "A")

  p4 <- out[person_id == "P4"]
  expect_equal(nrow(p4), 2)
  expect_equal(p4$value, c("A", "B"))
  expect_equal(as.Date(p4$end_episode[1]), as.Date("2024-01-04"))
  expect_equal(as.Date(p4$start_episode[2]), as.Date("2024-01-05"))

  expect_equal(nrow(out[person_id == "P5"]), 0)

  p6 <- out[person_id == "P6"]
  expect_equal(nrow(p6), 1)
  expect_equal(as.Date(p6$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p6$end_episode), as.Date("2024-01-07"))

  p7 <- out[person_id == "P7"]
  expect_equal(nrow(p7), 1)
  expect_equal(as.Date(p7$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p7$end_episode), as.Date("2024-01-15"))

  p8 <- out[person_id == "P8"]
  expect_equal(nrow(p8), 2)
  expect_setequal(p8$variable_id, c("VAR1", "VAR2"))
  expect_equal(p8$value[p8$variable_id == "VAR1"], "A")
  expect_equal(p8$value[p8$variable_id == "VAR2"], "X")

  p9 <- out[person_id == "P9"]
  expect_equal(nrow(p9), 2)
  expect_equal(as.Date(p9$start_episode), as.Date(c("2024-01-01", "2024-02-01")))

  p10 <- out[person_id == "P10"]
  expect_equal(nrow(p10), 1)
  expect_equal(p10$value, "Z")
  expect_equal(as.Date(p10$start_episode), as.Date("2024-02-15"))
})
