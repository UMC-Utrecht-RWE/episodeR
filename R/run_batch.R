#' Process Batches of Persons into Multivariate Episodes
#'
#' Runs the three-step multivariate episode SQL pipeline (explosion,
#' combine, merge-status) for a single batch of person_ids, pivots the
#' result to wide format, applies declared data types, and encodes
#' episodes against a compact combination dictionary to keep the merge
#' step cheap.
#'
#' Isolated from multivariate_episodes_pipeline() so it has no implicit
#' dependence on enclosing-scope state: every input it needs is an
#' explicit argument, and every DB side effect it produces (temp tables
#' it writes/drops) is documented below.
#'
#' @param ids_subset Vector of person_ids to process in this batch.
#' @param connection DBI connection used to execute SQL pipeline steps.
#' @param sql_explosion Loaded SQL (via picard::load_sql_query) for step 1.
#' @param sql_combine Loaded SQL for step 2.
#' @param sql_mergestatus Loaded SQL for step 3.
#' @param study_variables Data frame with variable metadata, used for
#' data-type conversion of the pivoted wide table.
#' @param data_type_col Name of the column in study_variables declaring
#' the target data type per variable, or NULL to skip conversion.
#'
#' @details Side effects on `connection`: writes/overwrites temp table
#' `i_batch_persons`; reads/drops tables `multivariate_episode` and
#' `dim_var` produced by the SQL steps as a byproduct; writes
#' `multivariate_episode_coded` (overwritten, left in place for
#' sql_mergestatus to read against). Caller is responsible for any
#' cleanup of `multivariate_episode_coded` across batches if desired.
#'
#' @return data.table of merged multivariate episodes for ids_subset,
#' with columns person_id, start_episode, end_episode, and one column
#' per study variable.
#'
#' @import data.table
run_batch <- function(
  ids_subset,
  connection,
  sql_explosion,
  sql_combine,
  sql_mergestatus,
  study_variables,
  data_type_col = "data_type"
) {
  DBI::dbWriteTable(
    connection,
    "i_batch_persons",
    data.frame(person_id = ids_subset, stringsAsFactors = FALSE),
    overwrite = TRUE
  )

  # Step 1: Explode spells to one row per person per variable per day
  picard::execute_sql_file(sql = sql_explosion, conn = connection)

  # Step 2: Combine daily values into multivariate status intervals
  picard::execute_sql_file(sql = sql_combine, conn = connection)

  i_multivariate_episode <- data.table::as.data.table(DBI::dbReadTable(
    connection,
    "multivariate_episode"
  ))
  dim_var <- data.table::as.data.table(DBI::dbReadTable(connection, "dim_var"))

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
    connection,
    "multivariate_episode_coded",
    episodes_coded,
    overwrite = TRUE
  )
  rm(i_status_boolmat, episodes_coded)

  # Step 3: Merge adjacent identical-status intervals
  merged_coded <- data.table::as.data.table(DBI::dbGetQuery(
    connection,
    sql_mergestatus
  ))
  merged_episodes <- merge(merged_coded, dictionary, by = "dic_index")[,
    !("dic_index"),
    with = FALSE
  ]
  rm(i_status_split, dim_var, merged_coded, dictionary)
  merged_episodes
}
