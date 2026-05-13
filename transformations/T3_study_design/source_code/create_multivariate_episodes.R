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
function_dir <- file.path(config_t3$T3$root, config_t3$T3$functions)

source("multivariate_episodes_pipeline.r")

# Load study_variables and map list_sv to variable_id
study_variables <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "study_variables.csv"
)
# For testing
list_sv <- c(
  "COD_HIST_ABRYSVO_VACC",
  "COD_PREVENT_CARE_USE",
  "ABRYSVO_VACC"
)

sv_meta <- study_variables[study_variables$variable_id %in% list_sv, ]
sv_meta[, batch := TRUE] # for testing; in production assign based on variable_id

d3_univariate_episodes_path <- config_project$create_multivariate_episodes$d3_univariate_episodes

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

person_ids <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT person_id FROM read_parquet('%s')",
  config_project$create_multivariate_episodes$d3_spells
))$person_id

multivariate_episodes_pipeline(
  study_variables             = sv_meta,
  con                         = con,
  d3_univariate_episodes_path = d3_univariate_episodes_path,
  sql_dir                     = sql_dir,
  output_path                 = config_project$create_multivariate_episodes$d3_multivariate_episodes,
  person_ids                  = person_ids,
  batch_size                  = 1000,
  batch_column                = "batch"
)

DBI::dbDisconnect(con, shutdown = TRUE)

logger::log_info("[Multivariate Episodes] - SUCCEED")

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}
