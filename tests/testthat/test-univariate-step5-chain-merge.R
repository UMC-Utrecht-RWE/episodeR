## Tests for uni_epi_5_chain_merge_episodes.sql
## Input: episodes_complete
## Output: D3_UNIVARIATE_EPISODES

seed_ec <- function(con, df) {
  DBI::dbWriteTable(con, "episodes_complete", df, overwrite = TRUE)
}

test_that("contiguous (adjacent, no gap) same-value episodes merge into one", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-20"))
})

test_that("overlapping same-value episodes merge into one covering the union", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date(c("2024-01-01", "2024-01-10")),
    end_episode = as.Date(c("2024-01-15", "2024-01-25"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-25"))
})

test_that("adjacent episodes with DIFFERENT values do not merge", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = c("A", "B"),
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES ORDER BY start_episode")
  expect_equal(nrow(out), 2)
  expect_equal(out$value, c("A", "B"))
})

test_that("a genuine one-day gap between same-value episodes prevents merging", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    start_episode = as.Date(c("2024-01-01", "2024-01-12")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES ORDER BY start_episode")
  expect_equal(nrow(out), 2)
})

test_that("NULL values are treated as equal to NULL for merge purposes (explicit IS NULL handling)", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = NA_character_,
    start_episode = as.Date(c("2024-01-01", "2024-01-11")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$value))
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-20"))
})

test_that("a chain of 3+ overlapping same-value episodes collapses to one, regardless of input order", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = "A",
    # deliberately out of chronological order, to catch ordering assumptions
    start_episode = as.Date(c("2024-01-12", "2024-01-01", "2024-01-20")),
    end_episode = as.Date(c("2024-01-25", "2024-01-15", "2024-01-31"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  expect_equal(nrow(out), 1)
  expect_equal(as.Date(out$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(out$end_episode), as.Date("2024-01-31"))
})

test_that("chain-merge does not cross person or variable boundaries", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = c("P1", "P1", "P2"),
    variable_id = c("VAR1", "VAR2", "VAR1"),
    value = "A",
    start_episode = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    end_episode = as.Date(c("2024-01-10", "2024-01-10", "2024-01-10"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES")
  expect_equal(nrow(out), 3)
})

test_that("a recurring identical value separated by a different value in between stays as two episodes", {
  con <- new_test_con()
  seed_ec(con, data.frame(
    person_id = "P1", variable_id = "VAR1", value = c("A", "B", "A"),
    start_episode = as.Date(c("2024-01-01", "2024-01-11", "2024-01-21")),
    end_episode = as.Date(c("2024-01-10", "2024-01-20", "2024-01-31"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- DBI::dbGetQuery(con, "SELECT * FROM D3_UNIVARIATE_EPISODES ORDER BY start_episode")
  expect_equal(nrow(out), 3)
  expect_equal(out$value, c("A", "B", "A"))
})

test_that("10 persons exercise contiguous/overlapping merge, no-merge, NULL-equality, chains, and multi-variable independence together", {
  con <- new_test_con()
  seed_ec(con, rbind(
    # P1: contiguous same-value episodes -> merge into one.
    data.frame(person_id = "P1", variable_id = "VAR1", value = "A",
      start_episode = as.Date(c("2024-01-01", "2024-01-11")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20"))),
    # P2: overlapping same-value episodes -> merge to the union.
    data.frame(person_id = "P2", variable_id = "VAR1", value = "A",
      start_episode = as.Date(c("2024-01-01", "2024-01-10")),
      end_episode = as.Date(c("2024-01-15", "2024-01-25"))),
    # P3: adjacent DIFFERENT values -> stay separate.
    data.frame(person_id = "P3", variable_id = "VAR1", value = c("A", "B"),
      start_episode = as.Date(c("2024-01-01", "2024-01-11")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20"))),
    # P4: a genuine one-day gap prevents merging.
    data.frame(person_id = "P4", variable_id = "VAR1", value = "A",
      start_episode = as.Date(c("2024-01-01", "2024-01-12")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20"))),
    # P5: NULL values treated as equal for merge purposes.
    data.frame(person_id = "P5", variable_id = "VAR1", value = NA_character_,
      start_episode = as.Date(c("2024-01-01", "2024-01-11")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20"))),
    # P6: chain of 3 overlapping same-value episodes, out of order -> merge to one.
    data.frame(person_id = "P6", variable_id = "VAR1", value = "A",
      start_episode = as.Date(c("2024-01-12", "2024-01-01", "2024-01-20")),
      end_episode = as.Date(c("2024-01-25", "2024-01-15", "2024-01-31"))),
    # P7: recurring value separated by a different one -> stays 3 episodes.
    data.frame(person_id = "P7", variable_id = "VAR1", value = c("A", "B", "A"),
      start_episode = as.Date(c("2024-01-01", "2024-01-11", "2024-01-21")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20", "2024-01-31"))),
    # P8: two different variables, same dates/value -> never merge across variable_id.
    data.frame(person_id = "P8", variable_id = c("VAR1", "VAR2"), value = "A",
      start_episode = as.Date("2024-01-01"), end_episode = as.Date("2024-01-10")),
    # P9: mirrors P1's shape with a different value, independence sanity check.
    data.frame(person_id = "P9", variable_id = "VAR1", value = "Z",
      start_episode = as.Date(c("2024-01-01", "2024-01-11")),
      end_episode = as.Date(c("2024-01-10", "2024-01-20"))),
    # P10: a single episode with no merge candidates -> untouched.
    data.frame(person_id = "P10", variable_id = "VAR1", value = "A",
      start_episode = as.Date("2024-01-05"), end_episode = as.Date("2024-01-10"))
  ))
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")
  out <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM D3_UNIVARIATE_EPISODES ORDER BY person_id, variable_id, start_episode"
  ))

  expect_equal(nrow(out), 15) # 1+1+2+2+1+1+3+2+1+1 across P1..P10
  expect_setequal(unique(out$person_id), paste0("P", 1:10))

  p1 <- out[person_id == "P1"]
  expect_equal(nrow(p1), 1)
  expect_equal(as.Date(p1$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p1$end_episode), as.Date("2024-01-20"))

  p2 <- out[person_id == "P2"]
  expect_equal(nrow(p2), 1)
  expect_equal(as.Date(p2$end_episode), as.Date("2024-01-25"))

  p3 <- out[person_id == "P3"]
  expect_equal(nrow(p3), 2)
  expect_equal(p3$value, c("A", "B"))

  expect_equal(nrow(out[person_id == "P4"]), 2)

  p5 <- out[person_id == "P5"]
  expect_equal(nrow(p5), 1)
  expect_true(is.na(p5$value))
  expect_equal(as.Date(p5$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p5$end_episode), as.Date("2024-01-20"))

  p6 <- out[person_id == "P6"]
  expect_equal(nrow(p6), 1)
  expect_equal(as.Date(p6$start_episode), as.Date("2024-01-01"))
  expect_equal(as.Date(p6$end_episode), as.Date("2024-01-31"))

  p7 <- out[person_id == "P7"]
  expect_equal(nrow(p7), 3)
  expect_equal(p7$value, c("A", "B", "A"))

  p8 <- out[person_id == "P8"]
  expect_equal(nrow(p8), 2)
  expect_setequal(p8$variable_id, c("VAR1", "VAR2"))

  p9 <- out[person_id == "P9"]
  expect_equal(nrow(p9), 1)
  expect_equal(p9$value, "Z")
  expect_equal(as.Date(p9$end_episode), as.Date("2024-01-20"))

  p10 <- out[person_id == "P10"]
  expect_equal(nrow(p10), 1)
  expect_equal(as.Date(p10$start_episode), as.Date("2024-01-05"))
  expect_equal(as.Date(p10$end_episode), as.Date("2024-01-10"))
})
