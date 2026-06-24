test_that("EXPLODE and INTERVAL branches produce identical multivariate_episode_merged output", {
    library(duckdb)
    library(DBI)

    sql_dir <- system.file(package = "episodeR", "sql") # tests/testthat -> project root/sql

    # -------------------------------------------------------------
    # Fixture data covering every edge case found during development:
    #   person 1: trivial single-variable episode
    #   person 2: THE GAP-BUG CASE -- same combination reappears after
    #             a true gap; must stay as two separate episodes
    #   person 3: overlapping intervals of the SAME variable (triggers
    #             the Branch B double-counting bug found during testing)
    #   person 4: two variables concurrently active (multivariate combo)
    #   person 5: touching episodes of the same combination (tests
    #             step 3's merge, not just step 2's collapse)
    #   person 6: triple overlap of the same variable
    #   person 7: combination change that coincides with a real gap
    #   person 8: one variable's interval nested inside another's
    #   person 9: back-to-back episodes of DIFFERENT combinations
    # -------------------------------------------------------------
    fixture_sql <- "
        CREATE TABLE new_variables_ids (
            person_id INTEGER, int_var_id INTEGER,
            start_episode DATE, end_episode DATE
        );
        INSERT INTO new_variables_ids VALUES
            (1, 1, '2024-01-01', '2024-01-05'),
            (2, 1, '2024-01-03', '2024-01-03'),
            (2, 1, '2024-01-06', '2024-01-08'),
            (3, 5, '2024-01-10', '2024-01-15'),
            (3, 5, '2024-01-13', '2024-01-18'),
            (4, 2, '2024-01-01', '2024-01-10'),
            (4, 3, '2024-01-05', '2024-01-08'),
            (5, 9, '2024-01-01', '2024-01-05'),
            (5, 9, '2024-01-06', '2024-01-10'),
            (6, 1, '2024-02-01', '2024-02-10'),
            (6, 1, '2024-02-05', '2024-02-15'),
            (6, 1, '2024-02-08', '2024-02-20'),
            (7, 1, '2024-01-01', '2024-01-05'),
            (7, 2, '2024-01-08', '2024-01-12'),
            (8, 1, '2024-01-01', '2024-01-20'),
            (8, 2, '2024-01-05', '2024-01-10'),
            (9, 1, '2024-01-01', '2024-01-05'),
            (9, 2, '2024-01-06', '2024-01-10');
    "

    run_sql_file <- function(con, path) {
        sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
        statements <- Filter(
            function(s) nzchar(trimws(s)),
            strsplit(sql_text, ";\\s*(\\n|$)")[[1]]
        )
        for (stmt in statements) {
            dbExecute(con, stmt)
        }
    }

    run_branch <- function(branch_file) {
        con <- dbConnect(duckdb(), dbdir = ":memory:")
        dbExecute(con, fixture_sql)
        run_sql_file(con, file.path(sql_dir, branch_file))
        run_sql_file(con, file.path(sql_dir, "step2_collapse.sql"))
        run_sql_file(con, file.path(sql_dir, "step3_merge.sql"))

        n_violations <- dbGetQuery(
            con,
            "SELECT COUNT(*) AS n FROM multivariate_episode_gap_check"
        )$n

        result <- dbGetQuery(
            con,
            "
            SELECT person_id, combination, start_episode, end_episode
            FROM multivariate_episode_merged
            ORDER BY person_id, combination, start_episode
        "
        )
        dbDisconnect(con, shutdown = TRUE)
        list(result = result, n_violations = n_violations)
    }

    explode_out <- run_branch("branch_explode.sql")
    interval_out <- run_branch("branch_interval.sql")

    # No episode should ever be merged across a real calendar gap,
    # in either branch.
    expect_equal(explode_out$n_violations, 0)
    expect_equal(interval_out$n_violations, 0)

    # The core assertion: both branches must agree exactly.
    expect_equal(explode_out$result, interval_out$result)

    # Spot-check the gap-bug case explicitly (person 2): must be two
    # separate episodes, not one Jan3-Jan8 episode.
    person_2 <- explode_out$result[explode_out$result$person_id == 2, ]
    expect_equal(nrow(person_2), 2)
    expect_equal(
        as.character(person_2$start_episode),
        c("2024-01-03", "2024-01-06")
    )

    # Spot-check the overlap case (person 3): must be ONE episode
    # spanning the full union, not fragmented or double-counted.
    person_3 <- explode_out$result[explode_out$result$person_id == 3, ]
    expect_equal(nrow(person_3), 1)
    expect_equal(as.character(person_3$start_episode), "2024-01-10")
    expect_equal(as.character(person_3$end_episode), "2024-01-18")

    # Spot-check the multivariate case (person 4): three segments,
    # with the middle one reflecting both variables combined.
    person_4 <- explode_out$result[explode_out$result$person_id == 4, ]
    expect_equal(nrow(person_4), 3)

    # Spot-check the touching-episode merge (person 5): step 3 must
    # merge into a single Jan1-Jan10 episode.
    person_5 <- explode_out$result[explode_out$result$person_id == 5, ]
    expect_equal(nrow(person_5), 1)
    expect_equal(as.character(person_5$end_episode), "2024-01-10")
})
