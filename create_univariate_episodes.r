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

# D3_SPELLS: read directly from single parquet file
DBI::dbExecute(con, sprintf(
  "CREATE OR REPLACE VIEW D3_SPELLS AS SELECT * FROM read_parquet('%s')",
  config_project$create_univariate_episodes$d3_spells
))

# Register study_variables as DuckDB table (small, so can load)
DBI::dbWriteTable(con, "study_variables", sv_meta, overwrite = TRUE)

# all_persons: create as a view from D3_SPELLS
DBI::dbExecute(con, "CREATE VIEW all_persons AS SELECT DISTINCT person_id FROM D3_SPELLS")

# Step 1: Generate most-recent-record-resolved, trimmed, chain-merged spells (v2 firststep logic)
# Replicates v2 firststep: concept dedup → initial spell windows → most-recent-record crop
# → clamp + overlap filter to study period → chain-merge same-value intervals
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(sql_dir, "uni_epi_1_generate_initial_spells.sql"), # → spells_raw
    params = list(
      concept_id_list = paste(sprintf("'%s'", concept_ids), collapse = ", "),
      start_study_date = sprintf("'%s'", as.character(start_study_date)),
      end_study_date = sprintf("'%s'", as.character(end_date_missing_inclusion))
    )
  ),
  conn = con
)

# Step 2: Fill gaps with missing_set_to value (v2 secondstep logic)
# Inserts gap-fill rows: before first spell, between non-adjacent spells, after last spell.
# Operates on untrimmed spell boundaries so overlapping zero-lookback spells don't
# spuriously produce gap fills.
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(sql_dir, "uni_epi_2_fill_gap_spells.sql"), # → spells_with_gaps
    params = list(
      start_study_date = sprintf("'%s'", as.character(start_study_date)),
      end_study_date   = sprintf("'%s'", as.character(end_date_missing_inclusion))
    )
  ),
  conn = con
)

# Step 3: Add full-period missing_set_to spells for persons with no concept data (v2 thirdstep)
# For any (person, variable) pair absent from spells_with_gaps, inserts one row
# covering the entire study period with missing_set_to as value.
DBI::dbWriteTable(con, "list_sv", data.frame(variable_id = list_sv), overwrite = TRUE)
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(sql_dir, "uni_epi_3_add_missing_persons.sql"), # → spells_complete
    params = list(
      start_study_date = sprintf("'%s'", as.character(start_study_date)),
      end_study_date   = sprintf("'%s'", as.character(end_date_missing_inclusion))
    )
  ),
  conn = con
)

# Step 4: Clip spells_complete to [start_study_date, end_date_missing_inclusion] in place
# (v2 cleanOutsidePeriod: DELETE rows that don't overlap, UPDATE start/end of remaining rows)
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(sql_dir, "uni_epi_4_trim_to_study_period.sql"), # modifies spells_complete in place
    params = list(
      start_study_date = sprintf("'%s'", as.character(start_study_date)),
      end_study_date   = sprintf("'%s'", as.character(end_date_missing_inclusion))
    )
  ),
  conn = con
)

# Boolean conversion for dichotomous variables: 0/NULL → FALSE, anything else → TRUE
# Matches v2 behaviour for variables whose value encodes presence/absence.
list_of_boolean_variables <- c("SV_IMMUNOCOMPROMISED", "SV_PRIOR_COVID_DG")
for (bv in intersect(list_sv, list_of_boolean_variables)) {
  DBI::dbExecute(con, sprintf(
    "UPDATE spells_complete
     SET value = CASE
       WHEN value IN ('FALSE', '0') OR value IS NULL THEN 'FALSE'
       ELSE 'TRUE'
     END
     WHERE variable_id = '%s'",
    bv
  ))
}

# Step 5: Chain-merge same-value overlapping/adjacent intervals (v2 fifthstep)
# Collapses contiguous or touching runs of identical value into a single row.
# Produces D3_UNIVARIATE_EPISODES.
picard::execute_sql_file(
  sql = picard::load_sql_query(
    file.path(sql_dir, "uni_epi_5_chain_merge_episodes.sql") # → D3_UNIVARIATE_EPISODES
  ),
  conn = con
)
output_parquet <- file.path(
  config_project$outputs$dir_d3, "D3_UNIVARIATE_EPISODES.parquet"
)
DBI::dbExecute(con, sprintf(
  "COPY D3_UNIVARIATE_EPISODES TO '%s' (FORMAT 'parquet');",
  output_parquet
))

logger::log_info(c("[Univariate Episodes] - SUCCEED"))

if (!base::is.null(lm)) {
  lm$stop_capturing_prints()
  lm$end_script()
}


# issue with value in D3_concepts: DSP_ABRYSVO_VACC value is "ABRYSVO" instead of 0/1
# PP_OTHER_VACC is not stored in D3_concepts, value is NULL for all
# todo - composites
