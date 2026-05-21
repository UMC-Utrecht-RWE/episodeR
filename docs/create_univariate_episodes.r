## Script logger ----
if (
  !base::exists("lm", inherits = TRUE) ||
    !base::inherits(lm, "LoggerManager")
) {
  lm <- NULL
}
if (!base::is.null(lm)) {
  lm$start_script("Processing D3 PERSONS")
  # Start capturing print statements
  lm$start_capturing_prints()
}

logger::log_info("[Univariate Episodes] - STARTING")

picard::load_config()

sql_dir <- file.path(config_t3$T3$root, config_t3$T3$sql_dir)
function_dir <- file.path(config_t3$T3$root, config_t3$T3$functions)

start_study_date <- config_values$start_study_date
end_date_missing_inclusion <- min(
  config_values$end_study_date
)
# , config_values$end_study_date_this_report

# Load study_variables and map list_sv to concept_id
study_variables <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "study_variables.csv"
)
study_variables$start_look_back <- abs(as.integer(study_variables$start_look_back))
study_variables$end_look_back <- abs(as.integer(study_variables$end_look_back))

missing_possible <- picard::load(
  file_path = config_pipeline$common$dir_bridge,
  file_name = "matching_possible_missing.csv"
)
study_variables <- merge(study_variables, missing_possible[, .(variable_id, missing_set_to)],
  by = "variable_id", all.x = TRUE
)


# Full list_sv for production
list_sv <- c(
  "COMP_IMMUNOCOMP_POP",
  "COMP_RENALIMPAIRMENT_POP",
  "COMP_HEPIMPAIRMENT_CHILDPUGH_POP",
  "population_subgroup", # check codebook immunocompromised/renal_impairment/hepatic_impairment/none
  "COD_HIST_ABRYSVO_VACC",
  "COD_PREVENT_CARE_USE",
  "ABRYSVO_VACC",
  "CALENDARTIME_QUARTER", # on the date of ABRYSVO_VACC
  "AGE_CAT_5Y", # D3_SPELLS/birth_date
  "older60y", # D3_SPELLS/birth_date
  "enrollment_12m", # D3_SPELLS/start_spell
  "fup_1d" # D3_SPELLS/start_spell, D3_SPELLS/end_spell
)

# For testing
list_sv <- c(
  "COD_HIST_ABRYSVO_VACC",
  "COD_PREVENT_CARE_USE",
  "ABRYSVO_VACC"
)
###########
# TODO composites
# check other PR developing for age, enrolment, etc.
###########

# Map variable_id to concept_id(s)
sv_meta <- study_variables[study_variables$variable_id %in% list_sv, ]
concept_ids <- unique(sv_meta$concept_id)
sv_meta[, batch := TRUE] # for testing, assign all to one batch. In production, assign batches based on concept_id or variable_id to optimize processing
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# D3_CONCEPTS: read only required concept_id partitions from hive-partitioned parquet folder
concept_id_filter <- paste(sprintf("'%s'", concept_ids), collapse = ", ")
DBI::dbExecute(con, sprintf(
  "CREATE OR REPLACE VIEW D3_CONCEPTS AS
   SELECT * FROM read_parquet('%s/**/*.parquet', hive_partitioning = TRUE)
   WHERE concept_id IN (%s)",
  config_project$create_univariate_episodes$d3_concepts,
  concept_id_filter
))


person_ids <- DBI::dbGetQuery(con, sprintf(
  "SELECT DISTINCT person_id FROM read_parquet('%s')",
  config_project$create_univariate_episodes$d3_spells
))$person_id

univariate_episodes_pipeline(
  study_variables = sv_meta,
  con = con,
  person_ids = person_ids,
  sql_dir = sql_dir,
  start_study_date = start_study_date,
  end_date_missing_inclusion = end_date_missing_inclusion,
  output_hive_path = file.path(config_project$outputs$dir_d3, "D3_UNIVARIATE_EPISODES_HIVE"),
  batch_size = 1000, # config_project$create_univariate_episodes$batch_size,
  batch_column = "batch"
)

logger::log_info(c("[Univariate Episodes] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}


# issue with value in D3_concepts: DSP_ABRYSVO_VACC value is "ABRYSVO" instead of 0/1
# PP_OTHER_VACC is not stored in D3_concepts, value is NULL for all
# todo - composites
