## Tests for multi_epi_3_mergestatus.sql
## Input: multivariate_episode_coded (person_id, dic_index, start_episode,
##   end_episode) - the dictionary-encoded combination episodes produced by
##   the R-side pivot/encode step between multi_epi_2 and multi_epi_3 (see
##   R/multivariate_episodes_pipeline.R's run_batch()).
## Output: same columns, with contiguous/overlapping same-dic_index
##   intervals chain-merged - the same algorithm as
##   uni_epi_5_chain_merge_episodes.sql, keyed on dic_index instead of value
##   and using INTERVAL '1 day' arithmetic instead of integer +/-1 (dates
##   here are already DATE-typed, unlike the univariate step's VARCHAR
##   pass-through columns).
##
## seed_episode_coded()/run_mergestatus_sql() come from helper-multivariate.R.

test_that("contiguous (adjacent, no gap) same-dic_index episodes merge into one", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = 1L,
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  out <- run_mergestatus_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-20"))
})

test_that("overlapping same-dic_index episodes merge into one covering the union", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = 1L,
    start_episode = as.Date(c("2024-01-01", "2024-01-10")),
    end_episode = as.Date(c("2024-01-15", "2024-01-25"))
  ))
  out <- run_mergestatus_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-25"))
})

test_that("adjacent episodes with DIFFERENT dic_index do not merge", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = c(1L, 2L),
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  out <- run_mergestatus_sql(con)
  out <- out[order(out$start_episode), ]

  expect_equal(nrow(out), 2)
  expect_equal(out$dic_index, c(1L, 2L))
})

test_that("a genuine one-day gap between same-dic_index episodes prevents merging", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = 1L,
    start_episode = as.Date(c("2024-01-01", "2024-01-12")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  out <- run_mergestatus_sql(con)
  expect_equal(nrow(out), 2)
})

test_that("a chain of 3+ overlapping same-dic_index episodes collapses to one, regardless of input order", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = 1L,
    # deliberately out of chronological order, to catch ordering assumptions
    start_episode = as.Date(c("2024-01-12", "2024-01-01", "2024-01-20")),
    end_episode = as.Date(c("2024-01-25", "2024-01-15", "2024-01-31"))
  ))
  out <- run_mergestatus_sql(con)

  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-31"))
})

test_that("chain-merge does not cross person or dic_index boundaries", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = c("P1", "P1", "P2"),
    dic_index = c(1L, 2L, 1L),
    start_episode = as.Date(rep("2024-01-01", 3)),
    end_episode = as.Date(rep("2024-01-10", 3))
  ))
  out <- run_mergestatus_sql(con)
  expect_equal(nrow(out), 3)
})

test_that("a recurring identical dic_index separated by a different one in between stays as two episodes", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = "P1", dic_index = c(1L, 2L, 1L),
    start_episode = as.Date(c("2024-01-01", "2024-01-11", "2024-01-21")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20", "2024-01-31"))
  ))
  out <- run_mergestatus_sql(con)
  out <- out[order(out$start_episode), ]

  expect_equal(nrow(out), 3)
  expect_equal(out$dic_index, c(1L, 2L, 1L))
})

test_that("10 persons with many dic_index values all merge/split correctly in one pass", {
  con <- new_test_con()
  seed_episode_coded(con, data.frame(
    person_id = c(
      "P1", "P1", "P1", "P2", "P2",
      "P3", "P4", "P4", "P5", "P5",
      "P6", "P6", "P7", "P7", "P8", "P8", "P8",
      "P9", "P10", "P10", "P10"
    ),
    dic_index = c(
      1L, 1L, 2L, 3L, 3L,
      1L, 2L, 4L, 5L, 5L,
      6L, 6L, 7L, 7L, 8L, 8L, 8L,
      9L, 10L, 10L, 11L
    ),
    start_episode = as.Date(c(
      "2024-01-01", "2024-01-06", "2024-01-11",
      "2024-01-01", "2024-01-05",
      "2024-01-01",
      "2024-01-01", "2024-01-15",
      "2024-01-01", "2024-01-02",
      "2024-01-01", "2024-01-10", # P6: overlapping -> merge to union
      "2024-01-01", "2024-01-12", # P7: genuine 1-day gap -> stays 2
      "2024-01-12", "2024-01-01", "2024-01-20", # P8: 3+ chain, out of order -> merge to 1
      "2024-02-01", # P9: single episode, independence sanity check
      "2024-01-01", "2024-01-06", "2024-01-15" # P10: merge dic=10, dic=11 stays separate
    )),
    end_episode = as.Date(c(
      "2024-01-05", "2024-01-10", "2024-01-20",
      "2024-01-04", "2024-01-09",
      "2024-01-31",
      "2024-01-14", "2024-01-25",
      "2024-01-01", "2024-01-03",
      "2024-01-15", "2024-01-25",
      "2024-01-10", "2024-01-20",
      "2024-01-25", "2024-01-15", "2024-01-31",
      "2024-02-05",
      "2024-01-05", "2024-01-10", "2024-01-20"
    ))
  ))
  out <- run_mergestatus_sql(con)
  out <- out[order(out$person_id, out$start_episode), ]

  # P1: [1-5]+[6-10] same dic_index 1 -> merge to 1-10; then dic_index 2 stays separate.
  p1 <- out[out$person_id == "P1", ]
  expect_equal(nrow(p1), 2)
  expect_equal(as.Date(p1$start_episode), as.Date(c("2024-01-01", "2024-01-11")))
  expect_equal(as.Date(p1$end_episode), as.Date(c("2024-01-10", "2024-01-20")))

  # P2: [1-4]+[5-9] same dic_index 3, adjacent -> merge to 1-9.
  p2 <- out[out$person_id == "P2", ]
  expect_equal(nrow(p2), 1)
  expect_equal(as.Date(p2$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p2$end_episode), as.Date("2024-01-09"))

  # P3: single episode, untouched.
  p3 <- out[out$person_id == "P3", ]
  expect_equal(nrow(p3), 1)

  # P4: two different dic_index values -> stay separate.
  p4 <- out[out$person_id == "P4", ]
  expect_equal(nrow(p4), 2)

  # P5: [01-01, 01-01]+[01-02, 01-03] same dic_index, adjacent -> merge to 01-01..01-03.
  p5 <- out[out$person_id == "P5", ]
  expect_equal(nrow(p5), 1)
  expect_equal(as.Date(p5$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p5$end_episode), as.Date("2024-01-03"))

  # P6: overlapping same dic_index episodes -> merge to the union.
  p6 <- out[out$person_id == "P6", ]
  expect_equal(nrow(p6), 1)
  expect_equal(as.Date(p6$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p6$end_episode), as.Date("2024-01-25"))

  # P7: a genuine one-day gap prevents merging.
  p7 <- out[out$person_id == "P7", ]
  expect_equal(nrow(p7), 2)

  # P8: chain of 3 overlapping same dic_index episodes, out of order -> merge to one.
  p8 <- out[out$person_id == "P8", ]
  expect_equal(nrow(p8), 1)
  expect_equal(as.Date(p8$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p8$end_episode), as.Date("2024-01-31"))

  # P9: single episode, independence sanity check (different month from P3).
  p9 <- out[out$person_id == "P9", ]
  expect_equal(nrow(p9), 1)
  expect_equal(as.Date(p9$start_episode), as.Date("2024-02-01"))

  # P10: dic_index 10's two adjacent episodes merge; dic_index 11 stays separate.
  p10 <- out[out$person_id == "P10", ]
  expect_equal(nrow(p10), 2)
  expect_equal(p10$dic_index, c(10L, 11L))
  expect_equal(as.Date(p10$start_episode), as.Date(c("2024-01-01", "2024-01-15")))
  expect_equal(as.Date(p10$end_episode), as.Date(c("2024-01-10", "2024-01-20")))

  expect_setequal(unique(out$person_id), paste0("P", 1:10))
})
