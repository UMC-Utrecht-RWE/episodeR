## Black-box test for multivariate_episodes_pipeline().
##
## Unlike test_multivariate_episodes_pipeline.R and the
## test-multivariate-step*.R files, this test never touches multi_epi_*.sql
## or any intermediate SQL table (EXPLODED, dim_var, multivariate_episode,
## ...) - it only calls the public multivariate_episodes_pipeline()
## function and asserts on its final parquet output. It's meant to survive
## a rewrite of the SQL implementation, as long as the function's
## documented input/output contract holds; the SQL-specific test files are
## expected to be deleted once that rewrite lands.

testthat::test_that("multivariate_episodes_pipeline produces correct output across 10 persons/2 variables", {
  # The univariate input is constructed directly (not produced by running
  # univariate_episodes_pipeline()) so the expected combined intervals can
  # be computed by hand via simple interval intersection, covering:
  # constant/constant, one variable changing, both changing together,
  # staggered changes, a value recurring after an interruption (must NOT
  # false-merge across the interruption), NULL values, a change at the
  # very last day, and a 4-segment staggered case.
  ep <- function(person, variable_id, value, start, end) {
    data.frame(
      person_id = person, variable_id = variable_id, value = value,
      start_episode = as.Date(start), end_episode = as.Date(end),
      stringsAsFactors = FALSE
    )
  }
  uni_epi <- rbind(
    ep("P1", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P1", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P2", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P2", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P2", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P3", "VAR1", "A", "2024-01-01", "2024-01-10"),
    ep("P3", "VAR2", "X", "2024-01-01", "2024-01-05"), ep("P3", "VAR2", "Y", "2024-01-06", "2024-01-10"),
    ep("P4", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P4", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P4", "VAR2", "X", "2024-01-01", "2024-01-04"), ep("P4", "VAR2", "Y", "2024-01-05", "2024-01-10"),
    ep("P5", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P5", "VAR1", "B", "2024-01-04", "2024-01-10"),
    ep("P5", "VAR2", "X", "2024-01-01", "2024-01-06"), ep("P5", "VAR2", "Y", "2024-01-07", "2024-01-10"),
    ep("P6", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P6", "VAR1", "B", "2024-01-04", "2024-01-06"), ep("P6", "VAR1", "A", "2024-01-07", "2024-01-10"),
    ep("P6", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P7", "VAR1", NA_character_, "2024-01-01", "2024-01-10"), ep("P7", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P8", "VAR1", "Q", "2024-01-01", "2024-01-10"), ep("P8", "VAR2", "R", "2024-01-01", "2024-01-10"),
    ep("P9", "VAR1", "A", "2024-01-01", "2024-01-09"), ep("P9", "VAR1", "B", "2024-01-10", "2024-01-10"),
    ep("P9", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P10", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P10", "VAR1", "B", "2024-01-04", "2024-01-07"), ep("P10", "VAR1", "A", "2024-01-08", "2024-01-10"),
    ep("P10", "VAR2", "X", "2024-01-01", "2024-01-05"), ep("P10", "VAR2", "Y", "2024-01-06", "2024-01-10")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_blackbox_uni_hive")
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::data.table(
    variable_id = c("VAR1", "VAR2"),
    batch = TRUE,
    data_type = "CHAR"
  )

  output_dir <- file.path(tempdir(), "multi_blackbox_output")
  unlink(output_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sql_dir <- system.file(package = "episodeR", "sql/")
  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_dir,
    person_ids = paste0("P", 1:10),
    batch_column = "batch",
    data_type_col = "data_type"
  )

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet('%s')",
      file.path(output_dir, "*.parquet")
    )
  ))
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- data.table::data.table(
    person_id = c(
      "P1",
      "P2", "P2",
      "P3", "P3",
      "P4", "P4",
      "P5", "P5", "P5",
      "P6", "P6", "P6",
      "P7",
      "P8",
      "P9", "P9",
      "P10", "P10", "P10", "P10"
    ),
    start_episode = as.Date(c(
      "2024-01-01",
      "2024-01-01", "2024-01-05",
      "2024-01-01", "2024-01-06",
      "2024-01-01", "2024-01-05",
      "2024-01-01", "2024-01-04", "2024-01-07",
      "2024-01-01", "2024-01-04", "2024-01-07",
      "2024-01-01",
      "2024-01-01",
      "2024-01-01", "2024-01-10",
      "2024-01-01", "2024-01-04", "2024-01-06", "2024-01-08"
    )),
    end_episode = as.Date(c(
      "2024-01-10",
      "2024-01-04", "2024-01-10",
      "2024-01-05", "2024-01-10",
      "2024-01-04", "2024-01-10",
      "2024-01-03", "2024-01-06", "2024-01-10",
      "2024-01-03", "2024-01-06", "2024-01-10",
      "2024-01-10",
      "2024-01-10",
      "2024-01-09", "2024-01-10",
      "2024-01-03", "2024-01-05", "2024-01-07", "2024-01-10"
    )),
    VAR1 = c(
      "A",
      "A", "B",
      "A", "A",
      "A", "B",
      "A", "B", "B",
      "A", "B", "A",
      NA_character_,
      "Q",
      "A", "B",
      "A", "B", "B", "A"
    ),
    VAR2 = c(
      "X",
      "X", "X",
      "X", "Y",
      "X", "Y",
      "X", "X", "Y",
      "X", "X", "X",
      "X",
      "R",
      "X", "X",
      "X", "X", "Y", "Y"
    )
  )
  data.table::setorder(expected, person_id, start_episode)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(nrow(actual), 21)
  testthat::expect_equal(actual, expected)
})

testthat::test_that("multivariate_episodes_pipeline round-trips correctly on real univariate_episodes_pipeline() output", {
  # Unlike the test above (which hand-constructs univariate input so the
  # expected combination can be worked out by hand), this test runs the
  # REAL univariate_episodes_pipeline() first, then feeds its actual
  # output into multivariate_episodes_pipeline(), and checks the result
  # against reference_combine() (helper-blackbox.R) applied to that same
  # real univariate output. This validates the real schema coupling
  # between the two functions - neither of the two hand-constructed
  # black-box tests exercises that boundary.
  concepts <- rbind(
    data.frame(person_id = "P1", concept_id = "C1", date = "2024-01-10", value = "A"),
    data.frame(person_id = "P2", concept_id = "C1", date = c("2024-01-01", "2024-01-07"), value = c("A", "B")),
    data.frame(person_id = "P4", concept_id = "C1", date = c("2024-01-02", "2024-01-20"), value = c("A", "A")),
    data.frame(person_id = "P5", concept_id = "C1", date = "2023-01-01", value = "A"),
    data.frame(person_id = "P6", concept_id = "C1", date = "2023-12-29", value = "A"),
    data.frame(person_id = "P7", concept_id = "C1", date = c("2024-01-01", "2024-01-03"), value = c("A", "A")),
    data.frame(person_id = "P8", concept_id = "C1", date = "2024-01-01", value = "A"),
    data.frame(person_id = "P9", concept_id = "C2", date = "2024-01-15", value = "Z")
  )
  sv_meta <- rbind(
    data.frame(concept_id = "C1", variable_id = "VAR1", start_look_back = 5, end_look_back = 0, missing_set_to = "MISSING", batch = FALSE, data_type = "CHAR", stringsAsFactors = FALSE),
    data.frame(concept_id = "C2", variable_id = "VAR2", start_look_back = 5, end_look_back = 0, missing_set_to = "MISSING_2", batch = FALSE, data_type = "CHAR", stringsAsFactors = FALSE)
  )
  persons <- paste0("P", 1:10)
  sql_dir <- system.file(package = "episodeR", "sql/")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)

  uni_hive_dir <- file.path(tempdir(), "roundtrip_uni_hive")
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  univariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    person_ids = persons,
    sql_dir = sql_dir,
    start_study_date = "2024-01-01",
    end_date_missing_inclusion = "2024-01-31",
    output_hive_path = uni_hive_dir,
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

  output_dir <- file.path(tempdir(), "roundtrip_multi_output")
  unlink(output_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sv_meta_batched <- sv_meta
  sv_meta_batched$batch <- TRUE
  multivariate_episodes_pipeline(
    study_variables = sv_meta_batched,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_dir,
    person_ids = persons,
    batch_column = "batch",
    data_type_col = "data_type"
  )

  actual_multi <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet('%s')", file.path(output_dir, "*.parquet"))
  ))
  actual_multi[, start_episode := as.Date(start_episode)]
  actual_multi[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual_multi, person_id, start_episode)

  expected_multi <- reference_combine(actual_uni)
  data.table::setcolorder(actual_multi, names(expected_multi))

  testthat::expect_equal(actual_multi, expected_multi)
})

testthat::test_that("multivariate_episodes_pipeline produces identical output whether or not it's batched", {
  # Forces the batched code path (batch = TRUE, batch_size = 2 -> 5
  # batches of 2 persons each for the same 10 persons, output_path used
  # as a directory) and checks the result is identical to the unbatched
  # single-file run. Reuses the same fixture as the "10 persons/2
  # variables" test above.
  ep <- function(person, variable_id, value, start, end) {
    data.frame(
      person_id = person, variable_id = variable_id, value = value,
      start_episode = as.Date(start), end_episode = as.Date(end),
      stringsAsFactors = FALSE
    )
  }
  uni_epi <- rbind(
    ep("P1", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P1", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P2", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P2", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P2", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P3", "VAR1", "A", "2024-01-01", "2024-01-10"),
    ep("P3", "VAR2", "X", "2024-01-01", "2024-01-05"), ep("P3", "VAR2", "Y", "2024-01-06", "2024-01-10"),
    ep("P4", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P4", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P4", "VAR2", "X", "2024-01-01", "2024-01-04"), ep("P4", "VAR2", "Y", "2024-01-05", "2024-01-10"),
    ep("P5", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P5", "VAR1", "B", "2024-01-04", "2024-01-10"),
    ep("P5", "VAR2", "X", "2024-01-01", "2024-01-06"), ep("P5", "VAR2", "Y", "2024-01-07", "2024-01-10"),
    ep("P6", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P6", "VAR1", "B", "2024-01-04", "2024-01-06"), ep("P6", "VAR1", "A", "2024-01-07", "2024-01-10"),
    ep("P6", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P7", "VAR1", NA_character_, "2024-01-01", "2024-01-10"), ep("P7", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P8", "VAR1", "Q", "2024-01-01", "2024-01-10"), ep("P8", "VAR2", "R", "2024-01-01", "2024-01-10"),
    ep("P9", "VAR1", "A", "2024-01-01", "2024-01-09"), ep("P9", "VAR1", "B", "2024-01-10", "2024-01-10"),
    ep("P9", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P10", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P10", "VAR1", "B", "2024-01-04", "2024-01-07"), ep("P10", "VAR1", "A", "2024-01-08", "2024-01-10"),
    ep("P10", "VAR2", "X", "2024-01-01", "2024-01-05"), ep("P10", "VAR2", "Y", "2024-01-06", "2024-01-10")
  )
  sql_dir <- system.file(package = "episodeR", "sql/")

  run_it <- function(batch_flag, batch_size = NULL) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    uni_hive_dir <- file.path(tempdir(), paste0("multi_batching_equiv_uni_", batch_flag))
    unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
    on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
    dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
    DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
    DBI::dbExecute(
      con,
      sprintf(
        "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
        uni_hive_dir
      )
    )

    sv_meta <- data.table::data.table(variable_id = c("VAR1", "VAR2"), batch = batch_flag, data_type = "CHAR")
    output_path <- file.path(tempdir(), paste0("multi_batching_equiv_out_", batch_flag))
    unlink(output_path, recursive = TRUE, force = TRUE)
    on.exit(unlink(output_path, recursive = TRUE, force = TRUE), add = TRUE)

    args <- list(
      study_variables = sv_meta, con = con, d3_univariate_episodes_path = uni_hive_dir,
      sql_dir = sql_dir, output_path = output_path, person_ids = paste0("P", 1:10),
      batch_column = "batch", data_type_col = "data_type"
    )
    if (!is.null(batch_size)) args$batch_size <- batch_size
    do.call(multivariate_episodes_pipeline, args)

    read_glob <- if (dir.exists(output_path)) file.path(output_path, "*.parquet") else output_path
    out <- data.table::as.data.table(DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", read_glob)))
    out[, start_episode := as.Date(start_episode)]
    out[, end_episode := as.Date(end_episode)]
    data.table::setorder(out, person_id, start_episode)
    out[]
  }

  unbatched <- run_it(FALSE)
  batched <- run_it(TRUE, batch_size = 2L)

  testthat::expect_equal(unbatched, batched)
})

testthat::test_that("multivariate_episodes_pipeline combines 3 simultaneous variables correctly", {
  # The two tests above only ever combine 2 variables, which wouldn't
  # catch a bug specific to combining 3+ (e.g. string_agg/dictionary
  # ordering, or a self-join condition that only checks one other
  # variable). Uses reference_combine() (helper-blackbox.R) as the
  # expected-output oracle rather than a hand-computed table, since with
  # 3 variables and 10 persons manual interval intersection would be
  # error-prone - reference_combine() was already validated against a
  # hand-verified 2-variable fixture elsewhere in this file.
  ep <- function(person, variable_id, value, start, end) {
    data.frame(
      person_id = person, variable_id = variable_id, value = value,
      start_episode = as.Date(start), end_episode = as.Date(end),
      stringsAsFactors = FALSE
    )
  }
  uni_epi <- rbind(
    # P1: all 3 constant
    ep("P1", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P1", "VAR2", "X", "2024-01-01", "2024-01-10"), ep("P1", "VAR3", "M", "2024-01-01", "2024-01-10"),
    # P2: only VAR1 changes
    ep("P2", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P2", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P2", "VAR2", "X", "2024-01-01", "2024-01-10"), ep("P2", "VAR3", "M", "2024-01-01", "2024-01-10"),
    # P3: VAR1 and VAR2 change together, VAR3 constant
    ep("P3", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P3", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P3", "VAR2", "X", "2024-01-01", "2024-01-04"), ep("P3", "VAR2", "Y", "2024-01-05", "2024-01-10"),
    ep("P3", "VAR3", "M", "2024-01-01", "2024-01-10"),
    # P4: all 3 change together at the same boundary
    ep("P4", "VAR1", "A", "2024-01-01", "2024-01-04"), ep("P4", "VAR1", "B", "2024-01-05", "2024-01-10"),
    ep("P4", "VAR2", "X", "2024-01-01", "2024-01-04"), ep("P4", "VAR2", "Y", "2024-01-05", "2024-01-10"),
    ep("P4", "VAR3", "M", "2024-01-01", "2024-01-04"), ep("P4", "VAR3", "N", "2024-01-05", "2024-01-10"),
    # P5: staggered changes at day 3 (VAR1), day 5 (VAR2), day 7 (VAR3)
    ep("P5", "VAR1", "A", "2024-01-01", "2024-01-02"), ep("P5", "VAR1", "B", "2024-01-03", "2024-01-10"),
    ep("P5", "VAR2", "X", "2024-01-01", "2024-01-04"), ep("P5", "VAR2", "Y", "2024-01-05", "2024-01-10"),
    ep("P5", "VAR3", "M", "2024-01-01", "2024-01-06"), ep("P5", "VAR3", "N", "2024-01-07", "2024-01-10"),
    # P6: VAR3 recurs (M,N,M) while VAR1/VAR2 stay constant - must not false-merge
    ep("P6", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P6", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P6", "VAR3", "M", "2024-01-01", "2024-01-03"), ep("P6", "VAR3", "N", "2024-01-04", "2024-01-06"), ep("P6", "VAR3", "M", "2024-01-07", "2024-01-10"),
    # P7: VAR2 NULL constant, others constant
    ep("P7", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P7", "VAR2", NA_character_, "2024-01-01", "2024-01-10"), ep("P7", "VAR3", "M", "2024-01-01", "2024-01-10"),
    # P8: independence/sanity, distinct values
    ep("P8", "VAR1", "Q", "2024-01-01", "2024-01-10"), ep("P8", "VAR2", "R", "2024-01-01", "2024-01-10"), ep("P8", "VAR3", "S", "2024-01-01", "2024-01-10"),
    # P9: VAR3 changes only on the very last day
    ep("P9", "VAR1", "A", "2024-01-01", "2024-01-10"), ep("P9", "VAR2", "X", "2024-01-01", "2024-01-10"),
    ep("P9", "VAR3", "M", "2024-01-01", "2024-01-09"), ep("P9", "VAR3", "N", "2024-01-10", "2024-01-10"),
    # P10: complex staggered pattern with overlapping boundaries across all 3
    ep("P10", "VAR1", "A", "2024-01-01", "2024-01-03"), ep("P10", "VAR1", "B", "2024-01-04", "2024-01-06"), ep("P10", "VAR1", "A", "2024-01-07", "2024-01-10"),
    ep("P10", "VAR2", "X", "2024-01-01", "2024-01-05"), ep("P10", "VAR2", "Y", "2024-01-06", "2024-01-10"),
    ep("P10", "VAR3", "M", "2024-01-01", "2024-01-02"), ep("P10", "VAR3", "N", "2024-01-03", "2024-01-08"), ep("P10", "VAR3", "M", "2024-01-09", "2024-01-10")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_3var_uni_hive")
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::data.table(
    variable_id = c("VAR1", "VAR2", "VAR3"),
    batch = TRUE,
    data_type = "CHAR"
  )
  output_dir <- file.path(tempdir(), "multi_3var_output")
  unlink(output_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sql_dir <- system.file(package = "episodeR", "sql/")
  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_dir,
    person_ids = paste0("P", 1:10),
    batch_column = "batch",
    data_type_col = "data_type"
  )

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet('%s')", file.path(output_dir, "*.parquet"))
  ))
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- reference_combine(uni_epi)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(nrow(actual), 24)
  testthat::expect_equal(actual, expected)
})

testthat::test_that("multivariate_episodes_pipeline combines 10 simultaneous variables correctly (extreme width)", {
  # Deliberately extreme: 10 variables active at once, an order of
  # magnitude past the 2- and 3-variable tests above, to catch bugs that
  # only show up at width (e.g. string_agg/dictionary ordering with many
  # ids, a self-join condition that only checks a fixed number of
  # variables, or dcast/pivot issues with many columns). As with the
  # 3-variable test, uses reference_combine() (helper-blackbox.R) as the
  # oracle rather than a hand-computed table - manually intersecting 10
  # variables' worth of intervals would be impractical to verify by eye.
  ep <- function(person, variable_id, value, start, end) {
    data.frame(
      person_id = person, variable_id = variable_id, value = value,
      start_episode = as.Date(start), end_episode = as.Date(end),
      stringsAsFactors = FALSE
    )
  }
  vars <- paste0("VAR", 1:10)
  rows <- list()

  # P1: all 10 constant -> 1 row.
  for (k in 1:10) {
    rows[[length(rows) + 1]] <- ep("P1", vars[k], paste0("V", k), "2024-01-01", "2024-01-15")
  }

  # P2: all 10 change together at the same boundary (day 8) -> 2 rows.
  for (k in 1:10) {
    rows[[length(rows) + 1]] <- ep("P2", vars[k], paste0("A", k), "2024-01-01", "2024-01-07")
    rows[[length(rows) + 1]] <- ep("P2", vars[k], paste0("B", k), "2024-01-08", "2024-01-15")
  }

  # P3: maximally staggered - VARk switches X{k}->Y{k} starting on day
  # (k+1), so each of days 2..10 flips exactly one more variable ->
  # 11 distinct combination segments (extreme fragmentation).
  for (k in 1:10) {
    switch_day <- k + 1
    rows[[length(rows) + 1]] <- ep("P3", vars[k], paste0("X", k), "2024-01-01", sprintf("2024-01-%02d", switch_day - 1))
    rows[[length(rows) + 1]] <- ep("P3", vars[k], paste0("Y", k), sprintf("2024-01-%02d", switch_day), "2024-01-15")
  }

  # P4: 9 variables constant, VAR5 toggles away and back to its original
  # value (A5 -> B5 -> A5) -> 3 rows; the two "all-A" segments either
  # side of the interruption must NOT false-merge, even at width 10.
  for (k in 1:10) {
    if (k != 5) {
      rows[[length(rows) + 1]] <- ep("P4", vars[k], paste0("A", k), "2024-01-01", "2024-01-15")
    } else {
      rows[[length(rows) + 1]] <- ep("P4", "VAR5", "A5", "2024-01-01", "2024-01-05")
      rows[[length(rows) + 1]] <- ep("P4", "VAR5", "B5", "2024-01-06", "2024-01-08")
      rows[[length(rows) + 1]] <- ep("P4", "VAR5", "A5", "2024-01-09", "2024-01-15")
    }
  }

  # P5: 5 variables with real constant values, 5 constant NULLs -> 1 row,
  # NULL handling in a wide (10-column) combination.
  for (k in 1:5) {
    rows[[length(rows) + 1]] <- ep("P5", vars[k], paste0("R", k), "2024-01-01", "2024-01-15")
  }
  for (k in 6:10) {
    rows[[length(rows) + 1]] <- ep("P5", vars[k], NA_character_, "2024-01-01", "2024-01-15")
  }

  uni_epi <- do.call(rbind, rows)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  uni_hive_dir <- file.path(tempdir(), "multi_10var_uni_hive")
  unlink(uni_hive_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(uni_hive_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(uni_hive_dir, recursive = TRUE, showWarnings = FALSE)
  DBI::dbWriteTable(con, "uni_epi_input", uni_epi, overwrite = TRUE)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY uni_epi_input TO '%s' (FORMAT PARQUET, PARTITION_BY (variable_id))",
      uni_hive_dir
    )
  )

  sv_meta <- data.table::data.table(
    variable_id = vars,
    batch = TRUE,
    data_type = "CHAR"
  )
  output_dir <- file.path(tempdir(), "multi_10var_output")
  unlink(output_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sql_dir <- system.file(package = "episodeR", "sql/")
  multivariate_episodes_pipeline(
    study_variables = sv_meta,
    con = con,
    d3_univariate_episodes_path = uni_hive_dir,
    sql_dir = sql_dir,
    output_path = output_dir,
    person_ids = paste0("P", 1:5),
    batch_column = "batch",
    data_type_col = "data_type"
  )

  actual <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet('%s')", file.path(output_dir, "*.parquet"))
  ))
  actual[, start_episode := as.Date(start_episode)]
  actual[, end_episode := as.Date(end_episode)]
  data.table::setorder(actual, person_id, start_episode)

  expected <- reference_combine(uni_epi)
  data.table::setcolorder(actual, names(expected))

  testthat::expect_equal(nrow(actual), 18)
  testthat::expect_equal(nrow(actual[actual$person_id == "P3", ]), 11) # the fragmentation case
  testthat::expect_equal(actual, expected)
})
