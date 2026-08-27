# Large-scale black-box test: univariate_episodes_pipeline() +
# multivariate_episodes_pipeline() end-to-end, across a cohort/variable
# count an order of magnitude past the existing hand-verified fixtures
# (which top out around 10 persons / 10 variables). Purpose is to catch
# integration-level breakage that only shows up at width/scale -- schema
# drift, batching correctness, look-back/MRR cropping, gap-fill defaults --
# that small fixtures wouldn't exercise.
#
# At this scale a hand-computed expected table isn't practical, so this
# test uses two complementary strategies instead of one exact oracle:
#   1. Structural sanity checks on the univariate output (full, gap-free,
#      non-overlapping coverage of the study period for every person x
#      variable pair; correct person/variable universe; dates within
#      bounds). Detailed correctness of the univariate transform itself
#      (dedup, MRR cropping, chain-merge, gap-fill) is already covered by
#      the small hand-verified fixtures in test-univariate-pipeline-blackbox.R
#      and the per-step test-univariate-step*.R files.
#   2. reference_combine() (helper-blackbox.R) as an independent oracle for
#      the multivariate combination step, applied to the pipeline's own
#      real univariate output -- same "round-trip" pattern already used by
#      the smaller "round-trips correctly on real univariate_episodes_pipeline()
#      output" test in test-multivariate-pipeline-blackbox.R, just scaled up.

testthat::test_that("univariate_episodes_pipeline + multivariate_episodes_pipeline end-to-end across 20 persons / 30 variables", {
  set.seed(20240101)

  n_persons <- 20L
  n_vars <- 30L
  persons <- sprintf("P%02d", seq_len(n_persons))
  vars <- sprintf("VAR%02d", seq_len(n_vars))
  concept_ids <- sprintf("C%02d", seq_len(n_vars))

  start_study_date <- as.Date("2024-01-01")
  end_study_date <- as.Date("2024-12-31")
  value_pool <- c("A", "B", "C")

  # Raw concept-level events per (person, concept): most pairs get 1-3
  # dated events (some before start_study_date, to exercise look-back/MRR
  # cropping; some NA values, to exercise NULL handling); ~10% of pairs are
  # skipped entirely, to exercise the missing-person gap-fill default.
  candidate_days <- seq(as.Date("2023-09-01"), as.Date("2024-11-30"), by = "day")
  concept_rows <- vector("list", n_persons * n_vars)
  idx <- 0L
  for (p in persons) {
    for (k in seq_len(n_vars)) {
      idx <- idx + 1L
      if (runif(1) < 0.10) next

      n_events <- sample(1:3, 1, prob = c(0.5, 0.3, 0.2))
      event_dates <- sort(sample(candidate_days, n_events))
      event_values <- sample(c(value_pool, NA_character_), n_events,
        replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)
      )

      concept_rows[[idx]] <- data.frame(
        person_id = p, concept_id = concept_ids[k],
        date = event_dates, value = event_values,
        stringsAsFactors = FALSE
      )
    }
  }
  concepts <- do.call(rbind, concept_rows)

  # Every third variable is flagged for batching, so both the
  # batch-flagged and non-batch-flagged code paths run within the same
  # test (univariate_episodes_pipeline() batches per-variable-subset;
  # multivariate_episodes_pipeline() batches the whole run once any
  # variable requests it).
  sv_meta <- data.frame(
    concept_id = concept_ids,
    variable_id = vars,
    start_look_back = 5,
    end_look_back = 0,
    missing_set_to = "MISSING",
    batch = (seq_len(n_vars) %% 3 == 0),
    data_type = "CHAR",
    stringsAsFactors = FALSE
  )

  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)

  uni_hive_dir <- file.path(tempdir(), "large_e2e_uni_hive")
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  univariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    person_ids = persons,
    sql_dir = sql_dir,
    start_study_date = as.character(start_study_date),
    end_date_missing_inclusion = as.character(end_study_date),
    output_hive_path = uni_hive_dir,
    batch_size = 6,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )

  actual_uni <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT person_id, variable_id, value, start_episode, end_episode
       FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
      uni_hive_dir
    )
  ))
  actual_uni[, start_episode := as.Date(start_episode)]
  actual_uni[, end_episode := as.Date(end_episode)]

  # -- Structural sanity checks on the univariate output --
  testthat::expect_setequal(unique(actual_uni$person_id), persons)
  testthat::expect_setequal(unique(actual_uni$variable_id), vars)
  testthat::expect_true(all(actual_uni$start_episode >= start_study_date))
  testthat::expect_true(all(actual_uni$end_episode <= end_study_date))
  testthat::expect_true(all(actual_uni$start_episode <= actual_uni$end_episode))

  data.table::setorder(actual_uni, person_id, variable_id, start_episode)
  coverage <- actual_uni[, .(
    n_days = sum(as.integer(end_episode - start_episode) + 1L),
    min_start = min(start_episode),
    max_end = max(end_episode),
    n_segments = .N,
    # every segment after the first must start exactly one day after the
    # previous segment's end -- full, gap-free, non-overlapping coverage
    n_gap_or_overlap_violations = sum(start_episode[-1] != end_episode[-.N] + 1)
  ), by = .(person_id, variable_id)]

  expected_days <- as.integer(end_study_date - start_study_date) + 1L
  testthat::expect_equal(nrow(coverage), n_persons * n_vars)
  testthat::expect_true(all(coverage$n_days == expected_days))
  testthat::expect_true(all(coverage$min_start == start_study_date))
  testthat::expect_true(all(coverage$max_end == end_study_date))
  testthat::expect_true(all(coverage$n_gap_or_overlap_violations == 0))

  # -- Multivariate step, validated against reference_combine() applied to
  # -- this test's own real univariate output --
  multi_output_dir <- file.path(tempdir(), "large_e2e_multi_output")
  unlink(multi_output_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(multi_output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = multi_output_dir,
    person_ids = persons,
    batch_size = 6,
    batch_column = "batch",
    data_type_col = "data_type"
  )

  read_glob <- if (dir.exists(multi_output_dir)) {
    file.path(multi_output_dir, "*.parquet")
  } else {
    multi_output_dir
  }
  actual_multi <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet('%s')", read_glob)
  ))
  actual_multi[, start_episode := as.Date(start_episode)]
  actual_multi[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual_multi, person_id, start_episode)

  expected_multi <- reference_combine(actual_uni)
  data.table::setcolorder(actual_multi, names(expected_multi))
  data.table::setorder(expected_multi, person_id, start_episode)

  testthat::expect_equal(nrow(actual_multi), nrow(expected_multi))
  testthat::expect_equal(actual_multi, expected_multi)

  # every person's combined episodes must also fully cover the study
  # period with no gaps
  multi_coverage <- actual_multi[, .(
    n_days = sum(as.integer(end_episode - start_episode) + 1L)
  ), by = person_id]
  testthat::expect_equal(nrow(multi_coverage), n_persons)
  testthat::expect_true(all(multi_coverage$n_days == expected_days))
})
