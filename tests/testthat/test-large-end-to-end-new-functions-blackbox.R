## Large-scale black-box parity test: create_univariate_episodes() +
## create_multivariate_episodes() (new) vs univariate_episodes_pipeline() +
## multivariate_episodes_pipeline() (old, deprecated) end-to-end, across a
## cohort/variable count an order of magnitude past the existing
## hand-verified fixtures (which top out around 10 persons / 10 variables).
##
## Same fixture-generation strategy as test-large-end-to-end-blackbox.R
## (which validates the old functions alone against reference_combine()),
## but here both old and new pipelines are run on the *same* synthetic
## input in the *same* test, and their outputs are compared directly for
## exact equality. That's a stronger check than either function's small
## dedicated blackbox tests give: it confirms the new SQL rewrites (sweep-
## line multivariate combine, window-function univariate chain-merge) agree
## with the old self-join/explode implementations at width/scale, not just
## on hand-picked small cases.

testthat::test_that("create_univariate_episodes + create_multivariate_episodes produce identical output to the old functions across 20 persons / 30 variables", {
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
  # test for both old and new functions.
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

  read_uni <- function(con, hive_dir) {
    out <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT person_id, variable_id, value, start_episode, end_episode
         FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        hive_dir
      )
    ))
    out[, start_episode := as.Date(start_episode)]
    out[, end_episode := as.Date(end_episode)]
    data.table::setorder(out, person_id, variable_id, start_episode)
    out[]
  }

  read_multi <- function(con, output_dir) {
    read_glob <- if (dir.exists(output_dir)) file.path(output_dir, "*.parquet") else output_dir
    out <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM read_parquet('%s')", read_glob)
    ))
    out[, start_episode := as.Date(start_episode)]
    out[, end_episode := as.Date(end_episode)]
    data.table::setorder(out, person_id, start_episode)
    out[]
  }

  # -- Old pipeline (oracle) --
  con_old <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con_old, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con_old, "D3_CONCEPTS", concepts, overwrite = TRUE)

  uni_old_dir <- file.path(tempdir(), "large_e2e_new_vs_old_uni_old")
  unlink(uni_old_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_old_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::expect_warning(
    univariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con_old,
      person_ids = persons,
      sql_dir = sql_dir,
      start_study_date = as.character(start_study_date),
      end_date_missing_inclusion = as.character(end_study_date),
      output_hive_path = uni_old_dir,
      batch_size = 6,
      batch_column = "batch",
      missing_col = "missing_set_to"
    ),
    "will be deprecated"
  )
  expected_uni <- read_uni(con_old, uni_old_dir)

  multi_old_dir <- file.path(tempdir(), "large_e2e_new_vs_old_multi_old")
  unlink(multi_old_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(multi_old_dir, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::expect_warning(
    multivariate_episodes_pipeline(
      study_variables = sv_meta,
      con = con_old,
      d3_univariate_episodes_path = uni_old_dir,
      sql_dir = sql_dir,
      output_path = multi_old_dir,
      person_ids = persons,
      batch_size = 6,
      batch_column = "batch",
      data_type_col = "data_type"
    ),
    "will be deprecated"
  )
  expected_multi <- read_multi(con_old, multi_old_dir)

  # -- New pipeline (under test) --
  con_new <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con_new, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con_new, "D3_CONCEPTS", concepts, overwrite = TRUE)

  uni_new_dir <- file.path(tempdir(), "large_e2e_new_vs_old_uni_new")
  unlink(uni_new_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_new_dir, recursive = TRUE, force = TRUE), add = TRUE)

  create_univariate_episodes(
    study_variables = sv_meta,
    con = con_new,
    person_ids = persons,
    sql_dir = sql_dir,
    start_study_date = as.character(start_study_date),
    end_date_missing_inclusion = as.character(end_study_date),
    output_hive_path = uni_new_dir,
    batch_size = 6,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )
  actual_uni <- read_uni(con_new, uni_new_dir)

  multi_new_dir <- file.path(tempdir(), "large_e2e_new_vs_old_multi_new")
  unlink(multi_new_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(multi_new_dir, recursive = TRUE, force = TRUE), add = TRUE)

  create_multivariate_episodes(
    study_variables = sv_meta,
    con = con_new,
    d3_univariate_episodes_path = uni_new_dir,
    output_path = multi_new_dir,
    person_ids = persons,
    batch_size = 6,
    batch_column = "batch",
    data_type_col = "data_type"
  )
  actual_multi <- read_multi(con_new, multi_new_dir)

  # -- Parity assertions --
  testthat::expect_equal(nrow(actual_uni), nrow(expected_uni))
  testthat::expect_equal(actual_uni, expected_uni)

  data.table::setcolorder(actual_multi, names(expected_multi))
  testthat::expect_equal(nrow(actual_multi), nrow(expected_multi))
  testthat::expect_equal(actual_multi, expected_multi)
})
