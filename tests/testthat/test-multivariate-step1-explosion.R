# Tests for multi_epi_1_explosion.sql
#
# Produces: dim_var, new_variables_ids, EXPLODED
#
# Unlike the uni_epi_*.sql steps, this step reads a real hive-partitioned
# parquet dataset (the {d3_univariate_episodes_path} param is a whole
# "'<dir>/**/*.parquet', hive_partitioning = TRUE" fragment, substituted
# verbatim into `read_parquet(...)`), so these tests write small on-disk
# parquet fixtures via write_uni_epi_fixture() rather than seeding a table
# directly. dim_date, i_batch_persons, and list_sv are also required
# inputs and are seeded per test, mirroring how
# multivariate_episodes_pipeline() builds them (see
# R/multivariate_episodes_pipeline.R).

uni_epi_row <- function(
  person_id,
  variable_id,
  value,
  start_episode,
  end_episode
) {
  data.frame(
    person_id = person_id,
    variable_id = variable_id,
    value = value,
    start_episode = as.Date(start_episode),
    end_episode = as.Date(end_episode),
    stringsAsFactors = FALSE
  )
}

#' Write `episodes` as a hive-partitioned parquet fixture, build dim_date
#' from it, seed i_batch_persons/list_sv, and run the explosion step.
#'
#' @param persons person_ids to include in i_batch_persons (batch filter).
#' @param variables variable_ids to include in list_sv (study variable filter).
run_explosion <- function(
  con,
  episodes,
  persons,
  variables,
  fixture_dir = tempfile("uni_epi_fixture_")
) {
  path_param <- write_uni_epi_fixture(con, episodes, fixture_dir)
  withr::defer(
    unlink(fixture_dir, recursive = TRUE, force = TRUE),
    envir = parent.frame()
  )
  build_dim_date(con, path_param)
  DBI::dbWriteTable(
    con,
    "i_batch_persons",
    data.frame(person_id = persons, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con,
    "list_sv",
    data.frame(variable_id = variables, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  run_step(
    con,
    "multi_epi_1_explosion.sql",
    list(d3_univariate_episodes_path = path_param)
  )
}

test_that("a single-day episode explodes to exactly one EXPLODED row", {
  con <- new_test_con()
  episodes <- uni_epi_row("P1", "VAR1", "A", "2024-01-05", "2024-01-05")
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  out <- DBI::dbGetQuery(con, "SELECT * FROM EXPLODED")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$dates), as.Date("2024-01-05"))
})

test_that("a multi-day episode explodes to one row per day, inclusive of both endpoints", {
  con <- new_test_con()
  episodes <- uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-05")
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  out <- DBI::dbGetQuery(con, "SELECT * FROM EXPLODED ORDER BY dates")
  expect_equal(nrow(out), 5)
  expect_equal(
    as.Date(out$dates),
    seq(as.Date("2024-01-01"), as.Date("2024-01-05"), by = "day")
  )
})

test_that("dim_var assigns int_var_id in ascending (variable_id, value) order", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR2", "X", "2024-01-01", "2024-01-01"),
    uni_epi_row("P1", "VAR1", "B", "2024-01-01", "2024-01-01"),
    uni_epi_row("P1", "VAR1", "A", "2024-01-02", "2024-01-02")
  )
  run_explosion(con, episodes, persons = "P1", variables = c("VAR1", "VAR2"))

  out <- DBI::dbGetQuery(con, "SELECT * FROM dim_var ORDER BY int_var_id")
  expect_equal(nrow(out), 3)
  expect_equal(out$variable_id, c("VAR1", "VAR1", "VAR2"))
  expect_equal(out$value, c("A", "B", "X"))
  expect_equal(out$int_var_id, 1:3)
})

test_that("the same (variable_id, value) pair reuses one dictionary entry across persons/episodes", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-02"),
    uni_epi_row("P2", "VAR1", "A", "2024-01-03", "2024-01-04")
  )
  run_explosion(con, episodes, persons = c("P1", "P2"), variables = "VAR1")

  dim_var <- DBI::dbGetQuery(con, "SELECT * FROM dim_var")
  expect_equal(nrow(dim_var), 1)

  exploded <- DBI::dbGetQuery(con, "SELECT DISTINCT int_var_id FROM EXPLODED")
  expect_equal(exploded$int_var_id, dim_var$int_var_id)
})

test_that("a variable_id with two distinct values gets two dictionary entries that don't cross-contaminate episodes", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-02"),
    uni_epi_row("P1", "VAR1", "B", "2024-01-05", "2024-01-06")
  )
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  dim_var <- DBI::dbGetQuery(con, "SELECT * FROM dim_var ORDER BY value")
  expect_equal(nrow(dim_var), 2)
  id_a <- dim_var$int_var_id[dim_var$value == "A"]
  id_b <- dim_var$int_var_id[dim_var$value == "B"]
  expect_false(id_a == id_b)

  exploded <- DBI::dbGetQuery(con, "SELECT * FROM EXPLODED ORDER BY dates")
  expect_true(all(exploded$int_var_id[as.Date(exploded$dates) <= "2024-01-02"] == id_a))
  expect_true(all(exploded$int_var_id[as.Date(exploded$dates) >= "2024-01-05"] == id_b))
})

test_that("a NULL value gets its own dictionary entry and joins via IS NULL, not lost", {
  con <- new_test_con()
  episodes <- uni_epi_row("P1", "VAR1", NA_character_, "2024-01-01", "2024-01-03")
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  dim_var <- DBI::dbGetQuery(con, "SELECT * FROM dim_var")
  expect_equal(nrow(dim_var), 1)
  expect_true(is.na(dim_var$value))

  exploded <- DBI::dbGetQuery(con, "SELECT * FROM EXPLODED")
  expect_equal(nrow(exploded), 3)
  expect_true(all(exploded$int_var_id == dim_var$int_var_id))
})

test_that("persons absent from i_batch_persons are excluded entirely (batch filtering)", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-02"),
    uni_epi_row("P2", "VAR1", "A", "2024-01-01", "2024-01-02")
  )
  # Only P1 is in the current batch.
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  exploded <- DBI::dbGetQuery(con, "SELECT DISTINCT person_id FROM EXPLODED")
  expect_equal(exploded$person_id, "P1")
})

test_that("variables absent from list_sv are excluded entirely (study-variable filtering)", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-02"),
    uni_epi_row("P1", "VAR2", "B", "2024-01-01", "2024-01-02")
  )
  # Only VAR1 is in the current study_variables/list_sv.
  run_explosion(con, episodes, persons = "P1", variables = "VAR1")

  dim_var <- DBI::dbGetQuery(con, "SELECT * FROM dim_var")
  expect_equal(dim_var$variable_id, "VAR1")

  exploded <- DBI::dbGetQuery(con, "SELECT DISTINCT int_var_id FROM EXPLODED")
  expect_equal(nrow(exploded), 1)
})

test_that("different persons explode independently and never mix rows", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-03"),
    uni_epi_row("P2", "VAR1", "A", "2024-01-10", "2024-01-11")
  )
  run_explosion(con, episodes, persons = c("P1", "P2"), variables = "VAR1")

  out <- DBI::dbGetQuery(con, "SELECT person_id, COUNT(*) AS n FROM EXPLODED GROUP BY person_id ORDER BY person_id")
  expect_equal(out$person_id, c("P1", "P2"))
  expect_equal(out$n, c(3, 2))
})

test_that("10 persons exercise batch/study-variable filtering, dictionary reuse, and concurrent/sequential overlap together", {
  con <- new_test_con()
  episodes <- rbind(
    # P1: single day.
    uni_epi_row("P1", "VAR1", "A", "2024-01-05", "2024-01-05"),
    # P2: 5-day run.
    uni_epi_row("P2", "VAR1", "A", "2024-01-01", "2024-01-05"),
    # P3: two variables concurrently active (overlapping days).
    uni_epi_row("P3", "VAR1", "A", "2024-01-01", "2024-01-05"),
    uni_epi_row("P3", "VAR2", "B", "2024-01-03", "2024-01-07"),
    # P4: two variables active sequentially, no overlap.
    uni_epi_row("P4", "VAR1", "A", "2024-01-01", "2024-01-03"),
    uni_epi_row("P4", "VAR2", "B", "2024-01-05", "2024-01-07"),
    # P5: NULL value.
    uni_epi_row("P5", "VAR1", NA_character_, "2024-01-01", "2024-01-03"),
    # P6: excluded via the batch filter (kept out of `persons` below).
    uni_epi_row("P6", "VAR1", "A", "2024-01-01", "2024-01-03"),
    # P7: excluded via the study-variable filter (VAR3 kept out of `variables` below).
    uni_epi_row("P7", "VAR3", "A", "2024-01-01", "2024-01-03"),
    # P8: same (variable_id, value) as P1, for dictionary reuse.
    uni_epi_row("P8", "VAR1", "A", "2024-01-10", "2024-01-10"),
    # P9: a second variable joins partway through the first's episode.
    uni_epi_row("P9", "VAR1", "A", "2024-01-01", "2024-01-10"),
    uni_epi_row("P9", "VAR2", "B", "2024-01-05", "2024-01-08"),
    # P10: long 10-day episode.
    uni_epi_row("P10", "VAR1", "A", "2024-01-01", "2024-01-10")
  )
  # P6 is deliberately left out of the batch; P7's only variable (VAR3) is
  # deliberately left out of list_sv - both should end up with zero rows
  # despite having rows in the source parquet.
  run_explosion(
    con,
    episodes,
    persons = setdiff(paste0("P", 1:10), "P6"),
    variables = c("VAR1", "VAR2")
  )

  out <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM EXPLODED"))
  dim_var <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM dim_var"))

  # 1+5+10+6+3+0+0+1+14+10 rows for P1..P10
  expect_equal(nrow(out), 50)
  expect_setequal(
    unique(out$person_id),
    c("P1", "P2", "P3", "P4", "P5", "P8", "P9", "P10")
  )
  expect_equal(nrow(out[person_id %in% c("P6", "P7")]), 0)

  # Only (VAR1, "A"), (VAR1, NA), (VAR2, "B") are ever referenced by an
  # included (person, variable) pair - VAR3 is excluded entirely via list_sv.
  expect_equal(nrow(dim_var), 3)
  expect_false("VAR3" %in% dim_var$variable_id)

  # P1 and P8 both use (VAR1, "A") and must share one dictionary entry.
  id_var1_a <- dim_var$int_var_id[dim_var$variable_id == "VAR1" & dim_var$value %in% "A"]
  p1_ids <- unique(out[person_id == "P1"]$int_var_id)
  p8_ids <- unique(out[person_id == "P8"]$int_var_id)
  expect_equal(p1_ids, id_var1_a)
  expect_equal(p8_ids, id_var1_a)

  # P9: VAR2 only overlaps VAR1 on 01-05..01-08 (both active -> 2 rows/day);
  # the remaining days of VAR1's 01-01..01-10 span have only 1 row/day.
  p9_by_date <- out[person_id == "P9", .N, by = dates][order(dates)]
  expect_equal(nrow(p9_by_date), 10)
  expect_equal(p9_by_date$N, c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1))
})

test_that("two concurrently-active variables for the same person both appear on every shared day", {
  con <- new_test_con()
  episodes <- rbind(
    uni_epi_row("P1", "VAR1", "A", "2024-01-01", "2024-01-05"),
    uni_epi_row("P1", "VAR2", "B", "2024-01-03", "2024-01-07")
  )
  run_explosion(con, episodes, persons = "P1", variables = c("VAR1", "VAR2"))

  out <- DBI::dbGetQuery(con, "
    SELECT dates, COUNT(*) AS n
    FROM EXPLODED GROUP BY dates ORDER BY dates
  ")
  # 01-01/02: VAR1 only: n=1. 01-03..05: both: n=2. 01-06/07: VAR2 only: n=1.
  expect_equal(nrow(out), 7)
  expect_equal(out$n, c(1, 1, 2, 2, 2, 1, 1))
})
