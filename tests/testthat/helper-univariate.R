## Test helpers for the univariate episode pipeline.
##
## These helpers deliberately do NOT depend on the internal `picard`
## package (picard::load_sql_query / picard::execute_sql_file), so this
## suite can run in CI or any environment that only has DBI + duckdb +
## data.table available. `.load_sql()` re-implements picard's simple
## "{param}" substitution; `.exec_sql_file()` mirrors execute_sql_file()
## by running every semicolon-separated statement in a file in order.
##
## If your environment *does* have `picard` installed and you want the
## tests to exercise the exact production code path (including
## `univariate_episodes_pipeline()` itself), see
## test-univariate-integration.R, which is skipped automatically when
## `picard` is not available.

sql_dir <- system.file("sql", package = "episodeR")
if (!nzchar(sql_dir)) {
  # Fallback for running tests from a source checkout without installing
  # the package first (devtools::load_all() style layouts).
  candidate <- file.path("..", "..", "inst", "sql")
  if (dir.exists(candidate)) sql_dir <- candidate
}

#' Minimal stand-in for picard::load_sql_query().
#' Replaces every "{name}" token in the file with params[["name"]].
.load_sql <- function(filename, params = list()) {
  sql <- readLines(file.path(sql_dir, filename), warn = FALSE)
  sql <- paste(sql, collapse = "\n")
  for (nm in names(params)) {
    sql <- gsub(paste0("{", nm, "}"), params[[nm]], sql, fixed = TRUE)
  }
  sql
}

#' Minimal stand-in for picard::execute_sql_file().
#' Mirrors execute_sql_file() by handing the whole (possibly
#' multi-statement) file to a single dbExecute() call - DuckDB runs
#' semicolon-separated statements in order natively. A naive R-side split
#' on ';' was tried previously but breaks on files with a ';' inside a
#' SQL comment (e.g. uni_epi_1_generate_initial_spells.sql).
.exec_sql <- function(con, sql) {
  DBI::dbExecute(con, sql)
  invisible(NULL)
}

#' Run one uni_epi_*.sql step against `con`.
run_step <- function(con, filename, params = list()) {
  .exec_sql(con, .load_sql(filename, params))
}

#' Fresh in-memory DuckDB connection.
new_test_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = parent.frame())
  con
}

#' Write the standard set of input tables the pipeline expects:
#' D3_CONCEPTS, study_variables, all_persons, list_sv.
#'
#' @param concepts data.frame(person_id, concept_id, date, value)
#' @param study_variables data.frame(concept_id, variable_id,
#'   start_look_back, end_look_back, missing_set_to). NOTE the SQL computes
#'   start_episode = date + end_look_back
#'   end_episode   = date + start_look_back
#'   i.e. the naming is swapped relative to intuition - verified against
#'   the actual SQL. Tests below use values consistent with this.
#' @param persons character vector of all person_ids in the population
#'   (drives step-3 "missing person" fill).
seed_inputs <- function(con, concepts, study_variables, persons) {
  DBI::dbWriteTable(con, "D3_CONCEPTS", concepts, overwrite = TRUE)
  DBI::dbWriteTable(con, "study_variables", study_variables, overwrite = TRUE)
  DBI::dbWriteTable(
    con,
    "all_persons",
    data.frame(person_id = persons, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con,
    "list_sv",
    data.frame(
      variable_id = unique(study_variables$variable_id),
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )
  invisible(NULL)
}

#' Run all 5 uni_epi_*.sql steps in order and return D3_UNIVARIATE_EPISODES
#' as a data.table, sorted for stable comparisons.
run_full_pipeline <- function(
  con,
  concepts,
  study_variables,
  persons,
  start_study_date,
  end_study_date
) {
  seed_inputs(con, concepts, study_variables, persons)

  concept_ids <- unique(study_variables$concept_id)
  concept_id_list <- paste(sprintf("'%s'", concept_ids), collapse = ", ")
  params_common <- list(
    start_study_date = sprintf("'%s'", start_study_date),
    end_study_date = sprintf("'%s'", end_study_date)
  )

  run_step(
    con,
    "uni_epi_1_generate_initial_spells.sql",
    c(list(concept_id_list = concept_id_list), params_common)
  )
  run_step(con, "uni_epi_2_fill_gap_spells.sql", params_common)
  run_step(con, "uni_epi_3_add_missing_persons.sql", params_common)
  run_step(con, "uni_epi_4_trim_to_study_period.sql", params_common)
  run_step(con, "uni_epi_5_chain_merge_episodes.sql")

  out <- DBI::dbGetQuery(
    con,
    "
    SELECT person_id, variable_id, value, start_episode, end_episode
    FROM D3_UNIVARIATE_EPISODES
    ORDER BY person_id, variable_id, start_episode
  "
  )
  data.table::as.data.table(out)
}

#' Convenience: build a single-row study_variables data.frame.
sv_row <- function(
  concept_id,
  variable_id,
  start_look_back,
  end_look_back,
  missing_set_to = NA
) {
  data.frame(
    concept_id = concept_id,
    variable_id = variable_id,
    start_look_back = start_look_back,
    end_look_back = end_look_back,
    missing_set_to = missing_set_to,
    stringsAsFactors = FALSE
  )
}

#' Assert that a set of episodes is gap-free and non-overlapping within
#' each (person_id, variable_id) group, and fully covers
#' [start_study_date, end_study_date].
expect_covers_and_gapfree <- function(
  episodes,
  start_study_date,
  end_study_date
) {
  episodes <- data.table::as.data.table(episodes)
  start_study_date <- as.Date(start_study_date)
  end_study_date <- as.Date(end_study_date)
  groups <- unique(episodes[, .(person_id, variable_id)])
  for (i in seq_len(nrow(groups))) {
    sub <- episodes[
      person_id == groups$person_id[i] & variable_id == groups$variable_id[i]
    ]
    sub <- sub[order(start_episode)]
    testthat::expect_equal(
      as.Date(sub$start_episode[1]),
      start_study_date,
      info = sprintf(
        "%s/%s does not start at study start",
        groups$person_id[i],
        groups$variable_id[i]
      )
    )
    testthat::expect_equal(
      as.Date(sub$end_episode[nrow(sub)]),
      end_study_date,
      info = sprintf(
        "%s/%s does not end at study end",
        groups$person_id[i],
        groups$variable_id[i]
      )
    )
    if (nrow(sub) > 1) {
      gaps <- as.integer(
        as.Date(sub$start_episode[-1]) - as.Date(sub$end_episode[-nrow(sub)])
      )
      testthat::expect_true(
        all(gaps == 1),
        info = sprintf(
          "%s/%s has a gap or overlap between consecutive episodes",
          groups$person_id[i],
          groups$variable_id[i]
        )
      )
    }
  }
}
