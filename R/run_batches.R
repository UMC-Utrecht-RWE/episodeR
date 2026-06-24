#' Run multivaritate episode pipeline in batches
#'
#' @param connection A database connection object
#' @param ids_subset A vector of person_ids to process in this batch
#' @param study_variables A list of study variables
#' @param sql_explosion The SQL file for exploding spells
#' @param sql_combine The SQL file for combining daily values
#' @param sql_mergestatus The SQL file for merging status intervals
#' @param data_type_col The column name for data types
#'
#' @returns NULL
#'
#' @keywords internal
.run_batch <- function(
  connection,
  ids_subset,
  study_variables = study_variables,
  sql_explosion = sql_explosion,
  sql_combine = sql_combine,
  sql_mergestatus = sql_mergestatus,
  data_type_col = data_type_col
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


#' Run multivaritate episode pipeline in batches
#'
#' @param connection A database connection object
#' @param ids_subset A vector of person_ids to process in this batch
#' @param study_variables A list of study variables
#' @param sql_explosion The SQL file for exploding spells
#' @param sql_combine The SQL file for combining daily values
#' @param sql_mergestatus The SQL file for merging status intervals
#' @param data_type_col The column name for data types
#'
#' @returns NULL
#'
#' @keywords internal
.run_batch_2 <- function(
  connection,
  ids_subset,
  study_variables = study_variables,
  sql_explosion = sql_explosion,
  sql_combine = sql_combine,
  sql_mergestatus = sql_mergestatus,
  sql_split_join = sql_split_join,
  data_type_col = data_type_col
) {
  # Stage this batch's person ids for the SQL pipeline
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

  # Step 2b: Split combination codes and resolve variable_id/value via dim_var
  # (moved from R's strsplit+join into SQL — avoids materializing the long
  # intermediate in R and avoids a data.table join over a wide table)
  picard::execute_sql_file(sql = sql_split_join, conn = con)

  # Pull the long (person, episode, variable_id, value) table back into R
  i_status_long <- data.table::as.data.table(DBI::dbReadTable(
    con,
    "i_status_long"
  ))

  # Guard against a batch with no matching variables/episodes
  if (nrow(i_status_long) == 0) {
    return(data.table::data.table())
  }

  # Pivot to wide format (person x episode x variable), FALSE for absent vars
  i_status_boolmat <- data.table::dcast(
    i_status_long,
    person_id + start_episode + end_episode ~ variable_id,
    value.var = "value",
    fill = FALSE
  )
  rm(i_status_long)

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
  rm(i_status_boolmat)

  DBI::dbWriteTable(
    con,
    "multivariate_episode_coded",
    episodes_coded,
    overwrite = TRUE
  )
  rm(episodes_coded)

  # Step 3: Merge adjacent identical-status intervals
  merged_coded <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    sql_mergestatus
  ))
  merged_episodes <- merge(merged_coded, dictionary, by = "dic_index")[,
    !("dic_index"),
    with = FALSE
  ]
  rm(merged_coded, dictionary)

  merged_episodes
}
