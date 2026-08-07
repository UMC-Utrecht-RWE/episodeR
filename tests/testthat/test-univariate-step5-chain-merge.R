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
