##' Build Multivariate Episodes Table
##'
##' Processes D3_UNIVARIATE_EPISODES hive-partitioned parquet into a wide
##' multivariate combination table (D3_MULTIVARIATE_EPISODES), processing
##' persons in batches of batch_size and writing each batch to parquet before
##' combining them into the final table.
##'
##' @param study_variables Data frame with variable metadata including
##' variable_id and a Boolean batching column.
##' @param con DBI connection used to execute SQL pipeline steps.
##' @param d3_univariate_episodes_path Directory path to the
##' D3_UNIVARIATE_EPISODES hive-partitioned parquet folder.
##' @param sql_dir Directory containing multi_epi_*.sql pipeline scripts.
##' @param output_path Full file path for the output parquet file.
##' @param person_ids Optional vector of person_ids. If NULL, derived from
##' distinct person_ids in the univariate episodes input.
##' @param batch_size Maximum number of persons per batch. Cohorts larger than
##' this are split into batches; smaller cohorts run as a single batch.
##' Defaults to 7000.
##' @param batch_column Name of a Boolean column in study_variables. Required
##' (its presence is validated); when any value is TRUE batching is forced even
##' for a small cohort. batch_size is otherwise the driver.
##' @param data_type_col Name of the column in study_variables that declares
##' the target data type for each variable (e.g. BOOL, NUM, INT, CHAR, DATE).
##' Defaults to "data_type". Set to NULL to skip type conversion.
##'
##' @return Invisibly returns NULL; writes D3_MULTIVARIATE_EPISODES parquet
##' to output_path.
#'
#' @import data.table
#' @export
multivariate_episodes_pipeline_2 <- function(
  study_variables,
  con,
  d3_univariate_episodes_path,
  sql_dir,
  output_path,
  person_ids = NULL,
  batch_size = 7000L,
  batch_column = "batch",
  data_type_col = "data_type"
) {
  if (missing(output_path) || !nzchar(output_path)) {
    stop("output_path must be provided and non-empty.")
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  if (!(batch_column %in% names(study_variables))) {
    stop(sprintf(
      "study_variables must include a Boolean '%s' column to control batching per variable.",
      batch_column
    ))
  }

  batch_values <- study_variables[[batch_column]]
  if (is.logical(batch_values)) {
    use_batch <- batch_values
  } else {
    normalized <- tolower(trimws(as.character(batch_values)))
    use_batch <- normalized %in% c("true", "t", "1", "yes", "y")
    invalid_batch_values <- !(normalized %in%
      c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n", "", "na"))
    if (any(invalid_batch_values, na.rm = TRUE)) {
      stop(sprintf(
        "Column '%s' must contain only Boolean-like values (TRUE/FALSE, 1/0, yes/no).",
        batch_column
      ))
    }
  }
  use_batch[is.na(use_batch)] <- FALSE
  do_batch <- any(use_batch)

  # Build date dimension from full univariate episodes range
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE OR REPLACE TABLE dim_date AS
     SELECT unnest(generate_series(
       MIN(start_episode)::DATE,
       MAX(end_episode)::DATE,
       INTERVAL '1 day'
     )) AS dates
     FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
      d3_univariate_episodes_path
    )
  )

  # Load SQL scripts once before batching
  uni_epi_param <- sprintf(
    "'%s/**/*.parquet', hive_partitioning = TRUE",
    d3_univariate_episodes_path
  )
  sql_explosion <- picard::load_sql_query(
    file.path(sql_dir, "multi_epi_1_explosion.sql"),
    params = list(d3_univariate_episodes_path = uni_epi_param)
  )
  sql_combine <- picard::load_sql_query(file.path(
    sql_dir,
    "multi_epi_2_combine.sql"
  ))
  sql_mergestatus <- picard::load_sql_query(file.path(
    sql_dir,
    "multi_epi_3_mergestatus.sql"
  ))

  # Resolve person_ids
  if (is.null(person_ids)) {
    person_ids <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT person_id FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)",
        d3_univariate_episodes_path
      )
    )$person_id
    message("person_ids derived from D3_UNIVARIATE_EPISODES")
  }

  run_batch <- function(ids_subset) {
    DBI::dbWriteTable(
      con,
      "i_batch_persons",
      data.frame(person_id = ids_subset, stringsAsFactors = FALSE),
      overwrite = TRUE
    )

    # Step 1: Explode spells to one row per person per variable per day
    picard::execute_sql_file(sql = sql_explosion, conn = con)

    # Step 2: Combine daily values into multivariate status intervals
    picard::execute_sql_file(sql = sql_combine, conn = con)

    i_multivariate_episode <- data.table::as.data.table(DBI::dbReadTable(
      con,
      "multivariate_episode"
    ))
    dim_var <- data.table::as.data.table(DBI::dbReadTable(con, "dim_var"))

    # Unpack combination strings -> one row per variable per episode
    i_status_split <- i_multivariate_episode[,
      .(combination = unlist(strsplit(as.character(combination), ";"))),
      by = .(person_id, start_episode, end_episode)
    ]
    i_status_split[, combination := as.integer(combination)]
    rm(i_multivariate_episode)

    # Pivot to wide format (person x episode x variable)
    i_status_boolmat <- i_status_split[
      dim_var,
      on = .(combination = int_var_id),
      nomatch = 0
    ]
    i_status_boolmat <- data.table::dcast(
      i_status_boolmat,
      person_id + start_episode + end_episode ~ variable_id,
      value.var = "value",
      fill = FALSE
    )

    # Convert variable columns to declared data types
    if (!is.null(data_type_col) && data_type_col %in% names(study_variables)) {
      i_status_boolmat <- apply_data_types(
        i_status_boolmat,
        study_variables,
        data_type_col
      )
    }

    # Build compact combination dictionary and encode episodes by index
    variables_cols <- names(i_status_boolmat)[
      !names(i_status_boolmat) %in%
        c("person_id", "start_episode", "end_episode")
    ]
    dictionary <- unique(i_status_boolmat[, ..variables_cols])
    dictionary[, dic_index := .I]

    episodes_coded <- merge(i_status_boolmat, dictionary, by = variables_cols)[,
      !(variables_cols),
      with = FALSE
    ]
    DBI::dbWriteTable(
      con,
      "multivariate_episode_coded",
      episodes_coded,
      overwrite = TRUE
    )
    rm(i_status_boolmat, episodes_coded)

    # Step 3: Merge adjacent identical-status intervals
    merged_coded <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      sql_mergestatus
    ))
    merged_episodes <- merge(merged_coded, dictionary, by = "dic_index")[,
      !("dic_index"),
      with = FALSE
    ]
    rm(i_status_split, dim_var, merged_coded, dictionary)
    merged_episodes
  }

  # Write each batch to its own parquet part, then combine the parts at the end
  batch_dir <- file.path(
    dirname(output_path),
    sprintf("mv_episodes_batches_%s", basename(tempfile("")))
  )
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(batch_dir, recursive = TRUE), add = TRUE)

  # Process batch_size persons at a time
  n_persons <- length(person_ids)
  step <- if (do_batch || n_persons > batch_size) batch_size else n_persons
  batch_starts <- seq.int(1L, n_persons, by = step)
  logger::log_info(paste("Number of batches:", length(batch_starts)))

  for (i_batch in seq_along(batch_starts)) {
    logger::log_info(sprintf(
      "Processing batch %d of %d",
      i_batch,
      length(batch_starts)
    ))
    from <- batch_starts[i_batch]
    to <- min(from + step - 1L, n_persons)
    batch_episodes <- run_batch(person_ids[from:to])

    DBI::dbWriteTable(
      con,
      "D3_MULTIVARIATE_EPISODES",
      batch_episodes,
      append = TRUE
    )
    # DBI::dbExecute(
    #   con,
    #   sprintf(
    #     "COPY i_batch_output TO '%s' (FORMAT 'parquet')",
    #     file.path(batch_dir, sprintf("batch_%05d.parquet", i_batch))
    #   )
    # )
    rm(batch_episodes)
  }

  logger::log_info("Batch processing complete")

  # union_by_name fills variables a batch never produced (old rbindlist fill = TRUE).
  # DBI::dbExecute(
  #   con,
  #   sprintf(
  #     "CREATE OR REPLACE TABLE D3_MULTIVARIATE_EPISODES AS
  #       SELECT
  #         person_id,
  #         CAST(start_episode AS DATE) AS start_episode,
  #         CAST(end_episode AS DATE) AS end_episode,
  #         * EXCLUDE (person_id, start_episode, end_episode)
  #       FROM read_parquet('%s/*.parquet', union_by_name = TRUE)",
  #     batch_dir
  #   )
  # )
  DBI::dbExecute(
    con,
    sprintf(
      "COPY D3_MULTIVARIATE_EPISODES TO '%s' (FORMAT 'parquet')",
      output_path
    )
  )
}
