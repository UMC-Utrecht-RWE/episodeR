## Tests for multi_epi_2_combine.sql
## Input: EXPLODED (person_id, int_var_id, dates) - one row per person per
##   active variable per day, as produced by multi_epi_1_explosion.sql.
## Output: multivariate_episode (person_id, combination, start_episode,
##   end_episode), where `combination` is a ';'-joined string of the
##   int_var_id's active that day, and consecutive days with an identical
##   combination are chain-merged into one episode.
##
## seed_exploded() rows must list each (person_id, dates) group's variables
## in ascending int_var_id order - see the comment on seed_exploded() in
## helper-multivariate.R for why (string_agg has no explicit ORDER BY, and
## relies on EXPLODED already being sorted that way).

test_that("a single active variable produces one episode spanning its full date range", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"),
    c("p1", 1L, "2024-01-03")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(out$combination, "1")
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-03"))
})

test_that("two variables active on the same days combine into one 'lo;hi' episode", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"), c("p1", 2L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"), c("p1", 2L, "2024-01-02")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(out$combination, "1;2")
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-02"))
})

test_that("a variable dropping out shrinks the combination and starts a new episode", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"), c("p1", 2L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"), c("p1", 2L, "2024-01-02"),
    # VAR2 (int_var_id 2) drops out from 01-03 onward
    c("p1", 1L, "2024-01-03"),
    c("p1", 1L, "2024-01-04")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 2)
  expect_equal(out$combination, c("1;2", "1"))
  expect_equal(as.Date(out$start_episode), as.Date(c("2024-01-01", "2024-01-03")))
  expect_equal(as.Date(out$end_episode), as.Date(c("2024-01-02", "2024-01-04")))
})

test_that("a variable joining partway through starts a new episode with the widened combination", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"),
    # VAR2 (int_var_id 2) joins from 01-03 onward
    c("p1", 1L, "2024-01-03"), c("p1", 2L, "2024-01-03"),
    c("p1", 1L, "2024-01-04"), c("p1", 2L, "2024-01-04")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 2)
  expect_equal(out$combination, c("1", "1;2"))
  expect_equal(as.Date(out$start_episode), as.Date(c("2024-01-01", "2024-01-03")))
  expect_equal(as.Date(out$end_episode), as.Date(c("2024-01-02", "2024-01-04")))
})

test_that("a value change that keeps the same variable set still produces one stable combination", {
  # combination is keyed on int_var_id (dictionary entries), so a variable
  # switching between two *different* dictionary-coded values (e.g. int_var_id
  # 1 -> int_var_id 3, both under VAR1) is a real combination change, but here
  # we hold int_var_id fixed across all days to confirm no spurious splitting.
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"),
    c("p1", 1L, "2024-01-03"),
    c("p1", 1L, "2024-01-04"),
    c("p1", 1L, "2024-01-05")
  ))
  out <- run_combine_sql(con)
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$end_episode) - as.Date(out$start_episode), as.difftime(4, units = "days"))
})

test_that("a real value change (different int_var_id) splits into two episodes with no merge", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"), # VAR1=A
    c("p1", 1L, "2024-01-02"),
    c("p1", 2L, "2024-01-03"), # VAR1=B (different dictionary entry)
    c("p1", 2L, "2024-01-04")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 2)
  expect_equal(out$combination, c("1", "2"))
})

test_that("a chain of 5+ consecutive days with an identical combination collapses to one episode", {
  con <- new_test_con()
  rows <- lapply(1:9, function(d) {
    c("p1", 1L, sprintf("2024-01-%02d", d))
  })
  seed_exploded(con, rows)
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-09"))
})

test_that("different persons combine independently and never mix rows", {
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"), c("p1", 1L, "2024-01-02"),
    c("p2", 2L, "2024-01-01"), c("p2", 2L, "2024-01-02"), c("p2", 2L, "2024-01-03")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 2)
  p1 <- out[out$person_id == "p1", ]
  p2 <- out[out$person_id == "p2", ]
  expect_equal(p1$combination, "1")
  expect_equal(p2$combination, "2")
  expect_equal(as.Date(p2$end_episode) - as.Date(p2$start_episode), as.difftime(2, units = "days"))
})

test_that("KNOWN BEHAVIOUR: a genuine coverage gap (no EXPLODED row at all) with matching combination on both sides is silently bridged into one episode", {
  # This documents actual production behaviour, not a requirement: the SQL
  # detects "combination changed" only by comparing a row to the *previous
  # existing* row (LAG ... ORDER BY dates), with no day-arithmetic gap check.
  # In practice this shouldn't arise from well-formed univariate output,
  # since uni_epi_2_fill_gap_spells.sql guarantees every day has some
  # (possibly missing_set_to) value and therefore some EXPLODED row - but if
  # EXPLODED ever has a true hole for a person/day, this step will merge
  # across it rather than splitting the episode at the hole.
  con <- new_test_con()
  seed_exploded(con, list(
    c("p1", 1L, "2024-01-01"),
    c("p1", 1L, "2024-01-02"),
    # gap: no EXPLODED rows at all for 2024-01-03 / 2024-01-04
    c("p1", 1L, "2024-01-05"),
    c("p1", 1L, "2024-01-06")
  ))
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-06"))
})
