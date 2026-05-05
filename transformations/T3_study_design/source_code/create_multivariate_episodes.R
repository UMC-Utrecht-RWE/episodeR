## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Creating D3 Multivariate Episodes")
  lm$start_capturing_prints()
}

logger::log_info("[Multivariate Episodes] - STARTING")

picard::load_config()

sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)

# Input: D3_UNIVARIATE_EPISODES.parquet produced by create_univariate_episodes
# Expected columns: person_id, variable_id, value, variable_start_spell, variable_end_spell
d3_univariate_episodes_path <- file.path(
  config_project$outputs$dir_d3, "D3_UNIVARIATE_EPISODES.parquet"
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Build date dimension directly from parquet in one query
DBI::dbExecute(con, sprintf(
  "CREATE OR REPLACE TABLE dim_date AS
   SELECT unnest(generate_series(
     MIN(variable_start_spell)::DATE,
     MAX(variable_end_spell)::DATE,
     INTERVAL '1 day'
   )) AS dates
   FROM read_parquet('%s')",
  d3_univariate_episodes_path
))

# Partition persons into batches for memory-efficient processing
persons_in_spells <- data.table::as.data.table(
  DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT person_id FROM read_parquet('%s')",
    d3_univariate_episodes_path
  ))
)
batch_size <- 7000
persons_in_spells[, batch := rep(1:ceiling(.N / batch_size), each = batch_size)[1:.N]]
num_batches <- unique(persons_in_spells$batch)

# Load SQL scripts once before the loop
sql_explosion <- picard::load_sql_query(file.path(sql_dir, "multi_epi_1_explosion.sql"))
sql_explosion <- gsub(
  x           = sql_explosion,
  pattern     = "/\\*STARTCHANGEME\\*/.*?/\\*ENDCHANGEME\\*/",
  replacement = sprintf("read_parquet('%s')", d3_univariate_episodes_path)
)
sql_combine <- picard::load_sql_query(file.path(sql_dir, "multi_epi_2_combine.sql"))
sql_mergestatus <- picard::load_sql_query(file.path(sql_dir, "multi_epi_3_mergestatus.sql"))

# Process each batch: explode → combine → merge adjacent identical-status intervals
logger::log_info(paste("Number of batches:", length(num_batches)))
multivariate_episodes <- data.table::data.table()

for (i_batch in num_batches) {
  logger::log_info(paste0("Processing batch ", i_batch, " of ", length(num_batches)))

  i_batch_persons <- persons_in_spells[batch == i_batch, "person_id"]
  DBI::dbWriteTable(con, "i_batch_persons", i_batch_persons, overwrite = TRUE)

  # Step 1: Explode spells to one row per person per variable per day
  DBI::dbExecute(con, sql_explosion)

  # Step 2: Combine daily values into multivariate status intervals
  DBI::dbExecute(con, sql_combine)

  # Read back results from DuckDB to R
  i_matching_status <- data.table::as.data.table(DBI::dbReadTable(con, "matching_status"))
  data.table::setnames(
    i_matching_status,
    c("start_date", "end_date"),
    c("matching_status_start", "matching_status_end")
  )
  dim_var <- data.table::as.data.table(DBI::dbReadTable(con, "dim_var"))

  # Unpack combination strings → one row per variable per episode
  i_status_split <- i_matching_status[,
    .(combination = unlist(strsplit(as.character(combination), ";"))),
    by = .(person_id, matching_status_start, matching_status_end)
  ]
  i_status_split[, combination := as.integer(combination)]
  rm(i_matching_status)

  # Pivot to wide boolean matrix (person × episode × variable)
  i_status_boolmat <- i_status_split[
    dim_var,
    on = .(combination = int_var_id),
    nomatch = 0
  ]
  i_status_boolmat <- data.table::dcast(
    i_status_boolmat,
    person_id + matching_status_start + matching_status_end ~ variable_id,
    value.var = "value",
    fill      = FALSE
  )

  # Build a compact combination dictionary and encode episodes by index
  variables_cols <- names(i_status_boolmat)[
    !names(i_status_boolmat) %in% c("person_id", "matching_status_start", "matching_status_end")
  ]
  dictionary <- unique(i_status_boolmat[, ..variables_cols])
  dictionary[, dic_index := .I]

  episodes_coded <- merge(i_status_boolmat, dictionary, by = variables_cols)[,
    !(variables_cols),
    with = FALSE
  ]
  DBI::dbWriteTable(con, "matching_status_coded", episodes_coded, overwrite = TRUE)
  rm(i_status_boolmat, episodes_coded)

  # Step 3: Merge adjacent identical-status intervals
  merged_coded <- data.table::as.data.table(DBI::dbGetQuery(con, sql_mergestatus))
  merged_episodes <- merge(merged_coded, dictionary, by = "dic_index")[,
    !("dic_index"),
    with = FALSE
  ]

  multivariate_episodes <- data.table::rbindlist(
    list(multivariate_episodes, merged_episodes),
    use.names = TRUE,
    fill      = TRUE
  )
  rm(merged_episodes, i_status_split, dim_var, merged_coded, dictionary, i_batch_persons)
}

logger::log_info("Batch processing complete")

# Write output as parquet
output_parquet <- file.path(config_project$outputs$dir_d3, "D3_MULTIVARIATE_EPISODES.parquet")
DBI::dbWriteTable(con, "D3_MULTIVARIATE_EPISODES", multivariate_episodes, overwrite = TRUE)
DBI::dbExecute(con, sprintf(
  "COPY D3_MULTIVARIATE_EPISODES TO '%s' (FORMAT 'parquet')",
  output_parquet
))

DBI::dbDisconnect(con, shutdown = TRUE)

logger::log_info("[Multivariate Episodes] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
