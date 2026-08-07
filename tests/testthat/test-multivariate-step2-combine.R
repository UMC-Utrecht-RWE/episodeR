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

test_that("10 persons exercise joining/dropping variables, real value changes, long chains, and multi-segment splits together", {
  con <- new_test_con()

  # Build EXPLODED rows for `person` on each day in `day_range` (1-based,
  # within 2024-01) with all of `var_ids` active that day, in ascending
  # order (see seed_exploded()'s ordering requirement).
  day_rows <- function(person, day_range, var_ids) {
    unlist(lapply(day_range, function(d) {
      lapply(sort(var_ids), function(v) c(person, v, sprintf("2024-01-%02d", d)))
    }), recursive = FALSE)
  }

  rows <- c(
    day_rows("p1", 1:5, 1), # single variable throughout
    day_rows("p2", 1:5, c(1, 2)), # two variables combined throughout
    day_rows("p3", 1:3, c(1, 2)), day_rows("p3", 4:6, 1), # variable drops out
    day_rows("p4", 1:3, 1), day_rows("p4", 4:6, c(1, 2)), # variable joins
    day_rows("p5", 1:3, 1), day_rows("p5", 4:6, 3), # real value change (1 -> 3)
    day_rows("p6", 1:9, 1), # long 9-day chain, one combination
    day_rows("p7", 1:3, 1), day_rows("p7", 4:6, 2), day_rows("p7", 7:9, 1), # A-B-A
    day_rows("p8", 1:5, 4), # mirrors p1's shape, independence sanity check
    day_rows("p9", 1, 1), # single day only
    day_rows("p10", 1:2, 1), day_rows("p10", 3:5, c(1, 2)), day_rows("p10", 6:7, 2) # 3-segment ramp up/down
  )
  seed_exploded(con, rows)
  out <- run_combine_sql(con)

  expect_equal(nrow(out), 17) # 1+1+2+2+2+1+3+1+1+3 across p1..p10
  expect_setequal(unique(out$person_id), paste0("p", 1:10))

  p1 <- out[out$person_id == "p1", ]
  expect_equal(nrow(p1), 1)
  expect_equal(p1$combination, "1")
  expect_equal(as.Date(p1$end_episode), as.Date("2024-01-05"))

  p2 <- out[out$person_id == "p2", ]
  expect_equal(nrow(p2), 1)
  expect_equal(p2$combination, "1;2")

  # run_combine_sql() already orders its result by (person_id, start_episode).
  p3 <- out[out$person_id == "p3", ]
  expect_equal(nrow(p3), 2)
  expect_equal(p3$combination, c("1;2", "1"))
  expect_equal(as.Date(p3$start_episode), as.Date(c("2024-01-01", "2024-01-04")))

  p4 <- out[out$person_id == "p4", ]
  expect_equal(p4$combination, c("1", "1;2"))

  p5 <- out[out$person_id == "p5", ]
  expect_equal(nrow(p5), 2)
  expect_equal(p5$combination, c("1", "3"))

  p6 <- out[out$person_id == "p6", ]
  expect_equal(nrow(p6), 1)
  expect_equal(as.Date(p6$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p6$end_episode), as.Date("2024-01-09"))

  p7 <- out[out$person_id == "p7", ]
  expect_equal(nrow(p7), 3)
  expect_equal(p7$combination, c("1", "2", "1"))

  p8 <- out[out$person_id == "p8", ]
  expect_equal(nrow(p8), 1)
  expect_equal(p8$combination, "4")

  p9 <- out[out$person_id == "p9", ]
  expect_equal(nrow(p9), 1)
  expect_equal(as.Date(p9$start_episode), as.Date(p9$end_episode))

  p10 <- out[out$person_id == "p10", ]
  expect_equal(nrow(p10), 3)
  expect_equal(p10$combination, c("1", "1;2", "2"))
})
