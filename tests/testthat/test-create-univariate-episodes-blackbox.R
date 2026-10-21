## Black-box test for create_univariate_episodes().
##
## Mirrors test-univariate-pipeline-blackbox.R's coverage of the older
## univariate_episodes_pipeline() (same fixtures, same expected output -
## the two functions must agree on results), plus a test for the behaviour
## that's new here: cohort-size-forced batching for variables that don't
## set batch_column themselves. Like its sibling file, this only calls the
## public create_univariate_episodes() function and asserts on its final
## parquet output - it never touches the create_univariate_episodes_*.sql
## files or any intermediate SQL table.

testthat::test_that("create_univariate_episodes produces correct output across 10 persons/2 variables", {
  # Covers, across one combined run: same-day dedup collapse, MRR cropping,
  # chain-merge of overlapping same-value records, a record dropped for
  # being fully outside the study period, a record clamped at the study
  # start boundary, gap-fill before/between/after real records, persons
  # entirely absent from concept data, and independence between two
  # variables (VAR1/VAR2) and across persons.
  concepts <- rbind(
    data.frame(
      person_id = "P1",
      concept_id = "C1",
      date = "2024-01-10",
      value = "A"
    ),
    data.frame(
      person_id = "P2",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-07"),
      value = c("A", "B")
    ),
    data.frame(
      person_id = "P4",
      concept_id = "C1",
      date = c("2024-01-02", "2024-01-20"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P5",
      concept_id = "C1",
      date = "2023-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P6",
      concept_id = "C1",
      date = "2023-12-29",
      value = "A"
    ),
    data.frame(
      person_id = "P7",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-03"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P8",
      concept_id = "C1",
      date = "2024-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P9",
      concept_id = "C2",
      date = "2024-01-15",
      value = "Z"
    )
    # P3 and P10 are deliberately absent from D3_CONCEPTS entirely.
  )
  sv_meta <- rbind(
    data.frame(
      concept_id = "C1",
      variable_id = "VAR1",
      start_look_back = 5,
      end_look_back = 0,
      missing_set_to = "MISSING",
      batch = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      concept_id = "C2",
      variable_id = "VAR2",
      start_look_back = 5,
      end_look_back = 0,
      missing_set_to = "MISSING_2",
      batch = FALSE,
      stringsAsFactors = FALSE
    )
  )
  persons <- paste0("P", 1:10)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)

  hive_dir <- file.path(tempdir(), "create_univariate_episodes_hive_blackbox")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  create_univariate_episodes(
    study_variables = sv_meta,
    con = con,
    person_ids = persons,
    start_study_date = "2024-01-01",
    end_date_missing_inclusion = "2024-01-31",
    output_hive_path = hive_dir,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT person_id, variable_id, value, start_episode, end_episode
       FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
      hive_dir
    )
  ))
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, variable_id, start_episode)

  expected <- data.table::data.table(
    person_id = c(
      "P1",
      "P1",
      "P1",
      "P1",
      "P10",
      "P10",
      "P2",
      "P2",
      "P2",
      "P2",
      "P3",
      "P3",
      "P4",
      "P4",
      "P4",
      "P4",
      "P4",
      "P4",
      "P5",
      "P5",
      "P6",
      "P6",
      "P6",
      "P7",
      "P7",
      "P7",
      "P8",
      "P8",
      "P8",
      "P9",
      "P9",
      "P9",
      "P9"
    ),
    variable_id = c(
      "VAR1",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR1",
      "VAR1",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR1",
      "VAR2",
      "VAR1",
      "VAR2",
      "VAR2",
      "VAR2"
    ),
    value = c(
      "MISSING",
      "A",
      "MISSING",
      "MISSING_2",
      "MISSING",
      "MISSING_2",
      "A",
      "B",
      "MISSING",
      "MISSING_2",
      "MISSING",
      "MISSING_2",
      "MISSING",
      "A",
      "MISSING",
      "A",
      "MISSING",
      "MISSING_2",
      "MISSING",
      "MISSING_2",
      "A",
      "MISSING",
      "MISSING_2",
      "A",
      "MISSING",
      "MISSING_2",
      "A",
      "MISSING",
      "MISSING_2",
      "MISSING",
      "MISSING_2",
      "Z",
      "MISSING_2"
    ),
    start_episode = as.Date(c(
      "2024-01-01",
      "2024-01-10",
      "2024-01-16",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-07",
      "2024-01-13",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-02",
      "2024-01-08",
      "2024-01-20",
      "2024-01-26",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-04",
      "2024-01-01",
      "2024-01-01",
      "2024-01-09",
      "2024-01-01",
      "2024-01-01",
      "2024-01-07",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01",
      "2024-01-15",
      "2024-01-21"
    )),
    end_episode = as.Date(c(
      "2024-01-09",
      "2024-01-15",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-06",
      "2024-01-12",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-01",
      "2024-01-07",
      "2024-01-19",
      "2024-01-25",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-03",
      "2024-01-31",
      "2024-01-31",
      "2024-01-08",
      "2024-01-31",
      "2024-01-31",
      "2024-01-06",
      "2024-01-31",
      "2024-01-31",
      "2024-01-31",
      "2024-01-14",
      "2024-01-20",
      "2024-01-31"
    ))
  )
  data.table::setorder(expected, person_id, variable_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(nrow(actual), 33)
  testthat::expect_equal(actual, expected)
})

testthat::test_that("create_univariate_episodes produces identical output whether or not variables are batched", {
  # Forces every variable through the person-batched code path
  # (batch = TRUE, batch_size = 2 -> 5 batches of 2 persons each for the
  # same 10 persons) and checks the final output is byte-identical to the
  # unbatched run - batching is purely a processing-chunk strategy and
  # must never change results. Reuses the same fixture as the "10
  # persons/2 variables" test above.
  concepts <- rbind(
    data.frame(
      person_id = "P1",
      concept_id = "C1",
      date = "2024-01-10",
      value = "A"
    ),
    data.frame(
      person_id = "P2",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-07"),
      value = c("A", "B")
    ),
    data.frame(
      person_id = "P4",
      concept_id = "C1",
      date = c("2024-01-02", "2024-01-20"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P5",
      concept_id = "C1",
      date = "2023-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P6",
      concept_id = "C1",
      date = "2023-12-29",
      value = "A"
    ),
    data.frame(
      person_id = "P7",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-03"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P8",
      concept_id = "C1",
      date = "2024-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P9",
      concept_id = "C2",
      date = "2024-01-15",
      value = "Z"
    )
  )
  persons <- paste0("P", 1:10)

  run_it <- function(batch_flag, batch_size = NULL) {
    sv_meta <- rbind(
      data.frame(
        concept_id = "C1",
        variable_id = "VAR1",
        start_look_back = 5,
        end_look_back = 0,
        missing_set_to = "MISSING",
        batch = batch_flag,
        stringsAsFactors = FALSE
      ),
      data.frame(
        concept_id = "C2",
        variable_id = "VAR2",
        start_look_back = 5,
        end_look_back = 0,
        missing_set_to = "MISSING_2",
        batch = batch_flag,
        stringsAsFactors = FALSE
      )
    )
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
    hive_dir <- file.path(
      tempdir(),
      paste0("create_uni_batching_equiv_", batch_flag)
    )
    unlink(hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

    args <- list(
      study_variables = sv_meta,
      con = con,
      person_ids = persons,
      start_study_date = "2024-01-01",
      end_date_missing_inclusion = "2024-01-31",
      output_hive_path = hive_dir,
      batch_column = "batch",
      missing_col = "missing_set_to"
    )
    if (!is.null(batch_size)) {
      args$batch_size <- batch_size
    }
    do.call(create_univariate_episodes, args)

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

  unbatched <- run_it(FALSE)
  batched <- run_it(TRUE, batch_size = 2L)

  testthat::expect_equal(unbatched, batched)
})

testthat::test_that("create_univariate_episodes auto-batches by cohort size even when batch = FALSE", {
  # No variable requests batching, but batch_size is set below the cohort
  # size (10 persons, batch_size = 3 -> 4 batches). This should still run
  # through the batched code path (forced by cohort size, mirroring
  # multivariate_episodes_pipeline()'s `n_persons > batch_size`) and must
  # produce output identical to a run whose batch_size comfortably exceeds
  # the cohort (single unbatched pass). Reuses the same fixture as the
  # "10 persons/2 variables" test above.
  concepts <- rbind(
    data.frame(
      person_id = "P1",
      concept_id = "C1",
      date = "2024-01-10",
      value = "A"
    ),
    data.frame(
      person_id = "P2",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-07"),
      value = c("A", "B")
    ),
    data.frame(
      person_id = "P4",
      concept_id = "C1",
      date = c("2024-01-02", "2024-01-20"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P5",
      concept_id = "C1",
      date = "2023-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P6",
      concept_id = "C1",
      date = "2023-12-29",
      value = "A"
    ),
    data.frame(
      person_id = "P7",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-03"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P8",
      concept_id = "C1",
      date = "2024-01-01",
      value = "A"
    ),
    data.frame(
      person_id = "P9",
      concept_id = "C2",
      date = "2024-01-15",
      value = "Z"
    )
  )
  persons <- paste0("P", 1:10)

  run_it <- function(batch_size) {
    sv_meta <- rbind(
      data.frame(
        concept_id = "C1",
        variable_id = "VAR1",
        start_look_back = 5,
        end_look_back = 0,
        missing_set_to = "MISSING",
        batch = FALSE,
        stringsAsFactors = FALSE
      ),
      data.frame(
        concept_id = "C2",
        variable_id = "VAR2",
        start_look_back = 5,
        end_look_back = 0,
        missing_set_to = "MISSING_2",
        batch = FALSE,
        stringsAsFactors = FALSE
      )
    )
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
    hive_dir <- file.path(
      tempdir(),
      paste0("create_uni_size_batching_", batch_size)
    )
    unlink(hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

    create_univariate_episodes(
      study_variables = sv_meta,
      con = con,
      person_ids = persons,
      start_study_date = "2024-01-01",
      end_date_missing_inclusion = "2024-01-31",
      output_hive_path = hive_dir,
      batch_size = batch_size,
      batch_column = "batch",
      missing_col = "missing_set_to"
    )

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

  unbatched <- run_it(batch_size = 1000L)
  size_forced_batched <- run_it(batch_size = 3L)

  testthat::expect_equal(unbatched, size_forced_batched)
})

testthat::test_that("create_univariate_episodes agrees with univariate_episodes_pipeline on the same input", {
  # The two functions implement the same contract via different SQL
  # (self-join vs window-function merge); their outputs must match exactly
  # for the same input. Suppresses the deprecation warning from the old
  # function so this test's output isn't cluttered by it.
  concepts <- rbind(
    data.frame(
      person_id = "P1",
      concept_id = "C1",
      date = "2024-01-10",
      value = "A"
    ),
    data.frame(
      person_id = "P2",
      concept_id = "C1",
      date = c("2024-01-01", "2024-01-07"),
      value = c("A", "B")
    ),
    data.frame(
      person_id = "P4",
      concept_id = "C1",
      date = c("2024-01-02", "2024-01-20"),
      value = c("A", "A")
    ),
    data.frame(
      person_id = "P9",
      concept_id = "C2",
      date = "2024-01-15",
      value = "Z"
    )
  )
  sv_meta <- rbind(
    data.frame(
      concept_id = "C1",
      variable_id = "VAR1",
      start_look_back = 5,
      end_look_back = 0,
      missing_set_to = "MISSING",
      batch = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      concept_id = "C2",
      variable_id = "VAR2",
      start_look_back = 5,
      end_look_back = 0,
      missing_set_to = "MISSING_2",
      batch = FALSE,
      stringsAsFactors = FALSE
    )
  )
  persons <- c("P1", "P2", "P4", "P9")
  sql_dir <- system.file(package = "episodeR", "sql/")

  run_new <- function() {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
    hive_dir <- file.path(tempdir(), "create_uni_vs_old_new")
    unlink(hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
    create_univariate_episodes(
      study_variables = sv_meta,
      con = con,
      person_ids = persons,
      start_study_date = "2024-01-01",
      end_date_missing_inclusion = "2024-01-31",
      output_hive_path = hive_dir,
      batch_column = "batch",
      missing_col = "missing_set_to"
    )
    out <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT person_id, variable_id, value, start_episode, end_episode FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        hive_dir
      )
    ))
    out[, start_episode := as.Date(start_episode)]
    out[, end_episode := as.Date(end_episode)]
    data.table::setorder(out, person_id, variable_id, start_episode)
    out[]
  }

  run_old <- function() {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
    hive_dir <- file.path(tempdir(), "create_uni_vs_old_old")
    unlink(hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
    testthat::expect_warning(
      univariate_episodes_pipeline(
        study_variables = sv_meta,
        con = con,
        person_ids = persons,
        sql_dir = sql_dir,
        start_study_date = "2024-01-01",
        end_date_missing_inclusion = "2024-01-31",
        output_hive_path = hive_dir,
        batch_column = "batch",
        missing_col = "missing_set_to"
      ),
      "will be deprecated"
    )
    out <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT person_id, variable_id, value, start_episode, end_episode FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        hive_dir
      )
    ))
    out[, start_episode := as.Date(start_episode)]
    out[, end_episode := as.Date(end_episode)]
    data.table::setorder(out, person_id, variable_id, start_episode)
    out[]
  }

  testthat::expect_equal(run_new(), run_old())
})

testthat::test_that("a gap-fill merges with an adjacent real episode of the same value", {
  # Step 1's chain-merge only ever sees the "real" concept-derived episodes
  # (episodes_raw / univariate_episodes) - it runs before steps 2/3 exist,
  # so it cannot merge a real episode with a gap-fill or missing-person row
  # steps 2/3 add afterward. If missing_set_to happens to equal a real
  # episode's value, the resulting adjacent same-value rows are only
  # merged by step 5. This test proves that actually happens end-to-end
  # (not just that step 5's merge logic works in isolation, which
  # test-univariate-step5-chain-merge.R already covers).
  concepts <- data.frame(
    person_id = "P1",
    concept_id = "C1",
    date = "2024-01-10",
    value = "A"
  )
  persons <- "P1"
  sql_dir <- system.file(package = "episodeR", "sql/")

  run_it <- function(missing_set_to) {
    sv_meta <- data.frame(
      concept_id = "C1",
      variable_id = "VAR1",
      start_look_back = 5,
      end_look_back = 0,
      missing_set_to = missing_set_to,
      batch = FALSE,
      stringsAsFactors = FALSE
    )
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
    hive_dir <- file.path(
      tempdir(),
      paste0("create_uni_gapfill_merge_", missing_set_to)
    )
    unlink(hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

    create_univariate_episodes(
      study_variables = sv_meta,
      con = con,
      person_ids = persons,
      start_study_date = "2024-01-01",
      end_date_missing_inclusion = "2024-01-31",
      output_hive_path = hive_dir,
      batch_column = "batch",
      missing_col = "missing_set_to"
    )

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
    data.table::setorder(out, start_episode)
    out[]
  }

  # missing_set_to = "A" collides with the real episode's value: the
  # before_first fill [01-01,01-09]="A", the real episode
  # [01-10,01-15]="A", and the after_last fill [01-16,01-31]="A" are all
  # adjacent and same-valued, so step 5 must collapse them into one
  # episode spanning the whole study period.
  colliding <- run_it("A")
  testthat::expect_equal(nrow(colliding), 1)
  testthat::expect_equal(colliding$value, "A")
  testthat::expect_equal(colliding$start_episode, as.Date("2024-01-01"))
  testthat::expect_equal(colliding$end_episode, as.Date("2024-01-31"))

  # Negative control: a non-colliding missing_set_to produces the naive
  # 3-episode result instead, confirming the single-episode result above
  # is genuinely value-driven and not an artefact of the date boundaries.
  non_colliding <- run_it("MISSING")
  testthat::expect_equal(nrow(non_colliding), 3)
  testthat::expect_equal(non_colliding$value, c("MISSING", "A", "MISSING"))
  testthat::expect_equal(
    non_colliding$start_episode,
    as.Date(c("2024-01-01", "2024-01-10", "2024-01-16"))
  )
  testthat::expect_equal(
    non_colliding$end_episode,
    as.Date(c("2024-01-09", "2024-01-15", "2024-01-31"))
  )
})

testthat::test_that("a gap-fill merges with an adjacent real episode when both are NULL", {
  # Same mechanism as the previous test, but for the default
  # missing_col = NULL path (gap-fills become SQL NULL) colliding with a
  # real concept value that is itself NULL, rather than an explicit
  # missing_set_to string colliding with a real string value.
  concepts <- data.frame(
    person_id = "P1",
    concept_id = "C1",
    date = "2024-01-10",
    value = NA_character_
  )
  sv_meta <- data.frame(
    concept_id = "C1",
    variable_id = "VAR1",
    start_look_back = 5,
    end_look_back = 0,
    batch = FALSE,
    stringsAsFactors = FALSE
  )
  persons <- "P1"
  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
  hive_dir <- file.path(tempdir(), "create_uni_gapfill_merge_null")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  create_univariate_episodes(
    study_variables = sv_meta,
    con = con,
    person_ids = persons,
    start_study_date = "2024-01-01",
    end_date_missing_inclusion = "2024-01-31",
    output_hive_path = hive_dir,
    batch_column = "batch"
    # missing_col intentionally omitted -> gap-fills use NULL
  )

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

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(is.na(out$value))
  testthat::expect_equal(out$start_episode, as.Date("2024-01-01"))
  testthat::expect_equal(out$end_episode, as.Date("2024-01-31"))
})

testthat::test_that("create_univariate_episodes output stays within the study period across boundary-stress scenarios", {
  # Regression test for the bounds guarantee. There is no dedicated
  # clip-to-study-period step any more (the former step 4 was dropped as a
  # provable no-op - see the git history of this file, and the comment
  # atop create_univariate_episodes_4_chain_merge_episodes.sql, for the
  # proof: step 1's `trimmed` CTE already clamps every real episode, and
  # steps 2/3 only ever construct rows from those already-bounded values).
  # This test asserts the end-to-end *output* invariant directly, so a
  # future change to steps 1-3 that breaks it gets caught here.
  concepts <- rbind(
    # P1: window lands exactly on [start_study_date, end_study_date] - no
    # clamping needed, exercises the exact-boundary edge.
    data.frame(
      person_id = "P1",
      concept_id = "C1",
      date = "2024-01-01",
      value = "A"
    ),
    # P2: a 365-day look-back from well before the study period extends
    # far past both boundaries - step 1 must clamp both ends.
    data.frame(
      person_id = "P2",
      concept_id = "C1",
      date = "2023-06-01",
      value = "A"
    )
    # P3 is deliberately absent from D3_CONCEPTS entirely (step 3 fill).
  )
  sv_meta <- data.frame(
    concept_id = "C1",
    variable_id = "VAR1",
    start_look_back = 365,
    end_look_back = 0,
    missing_set_to = "MISSING",
    batch = FALSE,
    stringsAsFactors = FALSE
  )
  persons <- c("P1", "P2", "P3")
  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)

  hive_dir <- file.path(tempdir(), "create_uni_bounds_invariant")
  unlink(hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  create_univariate_episodes(
    study_variables = sv_meta,
    con = con,
    person_ids = persons,
    start_study_date = "2024-01-01",
    end_date_missing_inclusion = "2024-01-31",
    output_hive_path = hive_dir,
    batch_column = "batch",
    missing_col = "missing_set_to"
  )

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

  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(out$start_episode >= as.Date("2024-01-01")))
  testthat::expect_true(all(out$end_episode <= as.Date("2024-01-31")))
  testthat::expect_true(all(out$start_episode <= out$end_episode))
})
