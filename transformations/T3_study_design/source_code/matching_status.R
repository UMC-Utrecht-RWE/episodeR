## Creating matching stratus
# This script generates a matching status dataset. I


# Inpuit 1: D3_<MATCH_CONCEPT_NAME>_SPELL
# Input 1: matching_status_spells.

logr::log_print(c("[Create Matching Status] - START"))

########################################
#### Connect to the database        ####
########################################

if (exists("matching_status_conn")) {
  DBI::dbDisconnect(matching_status_conn, shutdown = TRUE)
}

if (file.exists(dir_matchings_status)) {
  result <- file.remove(dir_matchings_status)
  print(paste("Matching Status Database removal successful: ", result))
}

if (file.exists(paste0(dir_matchings_status, ".wal"))) {
  result <- file.remove(paste0(dir_matchings_status, ".wal"))
  print(paste("Matching Status .wal removal successful: ", result))
}


matching_status_conn <- duckdb::dbConnect(duckdb::duckdb(), dir_matchings_status)
dbExecute(matching_status_conn, paste0("ATTACH IF NOT EXISTS '", dir_matching_spells, "' AS matching_spells (READ_ONLY);"))

#################################
#### Create a date dimension ####
#################################


min_max_dates <- dbGetQuery(matching_status_conn, "SELECT MIN(variable_start_spell) AS min_date,
                                        MAX(variable_end_spell) AS max_date
                                 FROM matching_spells.matching_variable_spells")

# Create the date dimension
dim_dates <- data.table(dates = seq.Date(from = as.Date(min_max_dates$min_date), to = as.Date(min_max_dates$max_date), by = "day"))
dim_dates <- dim_dates[, .(dates)]
# Send the date dimension to DuckDB
dbWriteTable(matching_status_conn, SQL("dim_date"), dim_dates, overwrite = TRUE)

rm(dim_dates, min_max_dates)
gc()

#################################
#### Create person batches ####
#################################

persons_in_spells <- as.data.table(dbGetQuery(matching_status_conn, "SELECT DISTINCT person_id
                                 FROM matching_spells.matching_variable_spells"))
# Define batch size
if (testing == TRUE) {
  batch_size <- 700
} else {
  batch_size <- 7000
}

# Create batch column
persons_in_spells[, batch := rep(1:ceiling(.N / batch_size), each = batch_size)[1:.N]]
num_batches <- unique(persons_in_spells$batch)

##############################
#### Read the SQL scripts ####
##############################

create_matching_status_01_explosion <- getSQL(filepath = file.path(dir_sqlqueries_t3, "create_matching_status_01_explosion.sql"))
create_matching_status_01_explosion <- gsub(
  x = create_matching_status_01_explosion,
  pattern = "/\\*STARTCHANGEME\\*/.*?/\\*ENDCHANGEME\\*/",
  replacement = "matching_spells.matching_variable_spells"
)

create_matching_status_02_combine <- getSQL(filepath = file.path(dir_sqlqueries_t3, "create_matching_status_02_combine.sql"))
create_matching_status_03_mergestatus <- getSQL(filepath = file.path(dir_sqlqueries_t3, "create_matching_status_03_mergestatus.sql"))


###################################################################################
#### Now run smaller batches per year, this should speed stuff up considerably ####
###################################################################################

tic("[Create Matching Status] Total time")
# Loop per year
matching_status <- data.table()
log_print(paste("Number of batches: ", num_batches))
for (i_batch in num_batches) {
  # Some verbosity
  log_print(paste0("Now running on batch ", i_batch))

  # Filter to that batch we're looping for
  i_batch_persons <- persons_in_spells[batch == i_batch, "person_id"]

  # Send the date dimension to DuckDB
  dbWriteTable(matching_status_conn, SQL("i_batch_persons"), i_batch_persons, overwrite = TRUE)

  # Create the explosion
  tic("create_matching_status_01_explosion")
  dbExecute(matching_status_conn, create_matching_status_01_explosion)
  toc_log_print()

  # And combine the spells
  tic("create_matching_status_02_combine")
  dbExecute(matching_status_conn, create_matching_status_02_combine)
  toc_log_print()

  # Read the data back to R, starting with the spells
  i_matching_status <- as.data.table(dbReadTable(matching_status_conn, "matching_status"))
  setnames(i_matching_status, c("start_date", "end_date"), c("matching_status_start", "matching_status_end"))

  # We also need the dim_var table back to build the actual spells table back
  dim_var <- as.data.table(dbReadTable(matching_status_conn, "dim_var"))

  ##################################
  #### Rebuild the spells table ####
  ##################################

  # Here we're going to open the spells again to a single row for every variable per spell
  # This shouldn't explode to anything over the accepted number of rows for R
  i_matching_status_split <- i_matching_status[, .(combination = unlist(strsplit(as.character(combination), ";"))), by = .(person_id, matching_status_start, matching_status_end)]
  i_matching_status_split[, combination := as.integer(combination)]

  rm(i_matching_status)
  gc()

  # And we create the boolean matrix
  i_matching_status_boolmat <- i_matching_status_split[dim_var, on = .(combination = int_var_id), nomatch = 0]
  i_matching_status_boolmat <- dcast(i_matching_status_boolmat, person_id + matching_status_start + matching_status_end ~ variable_id, value.var = "value", fill = FALSE)


  variables_status <- names(i_matching_status_boolmat)[!names(i_matching_status_boolmat) %in% c("person_id", "matching_status_start", "matching_status_end")]
  dictionary_status <- unique(i_matching_status_boolmat[, ..variables_status])
  dictionary_status[, dic_index := .I]

  matching_status_coded <- merge(i_matching_status_boolmat, dictionary_status, by = variables_status)[, !(variables_status), with = FALSE]

  dbWriteTable(matching_status_conn, name = "matching_status_coded", value = matching_status_coded, overwrite = TRUE)
  rm(i_matching_status_boolmat, matching_status_coded)
  c()

  tic("Merge Status")
  merged_status_coded <- as.data.table(dbGetQuery(matching_status_conn, create_matching_status_03_mergestatus))
  toc_log_print()

  merged_status <- merge(merged_status_coded, dictionary_status, by = "dic_index")[, !("dic_index"), with = FALSE]

  # We create spell frames per year called i_matching_status_boolmat_YEAR
  matching_status <- rbindlist(list(matching_status, merged_status), use.names = TRUE, fill = TRUE)
  rm(merged_status, i_matching_status_split, dim_var, merged_status_coded, dictionary_status, i_batch_persons)
}

log_print("Processing match status FINISHED")
# ======================================== ADDING ADDITIONAL NON TIME-VARYING VARIABLES

# Adding year_of_birth
persons_information <- unique(as.data.table(read_fst(file.path(dir_d3, "D3_SPELLS.fst")))[, c("person_id", "year_of_birth")])
matching_status <- merge(matching_status, persons_information, by = "person_id")

Pfizer1052_expected_missing_variables <- as.data.table(fread(file.path(dir_commonconfig, "Pfizer1052_expected_missing_variables.csv"), header = TRUE))
Pfizer1052_expected_missing_variables <- Pfizer1052_expected_missing_variables[, c("variable_id", this_dap), with = F]
Pfizer1052_expected_missing_variables <- Pfizer1052_expected_missing_variables[get(this_dap) %in% "ND", variable_id]

matching_possible_missing <- as.data.table(fread(file.path(dir_config_t3, "matching_possible_missing.csv")))[exclude_if_missing == TRUE]
matching_possible_missing[variable_id %in% "SV_AGE", variable_id := "year_of_birth"] # SV_AGE is year_of_birth

matching_expected_missing_var <- Pfizer1052_expected_missing_variables[Pfizer1052_expected_missing_variables %in% matching_possible_missing$variable_id]

log_print(paste0("[create_matchstatus] Expected missing matching variables:", matching_expected_missing_var, "
                 Not included as exclusion criteria"))
matching_possible_missing <- matching_possible_missing[!variable_id %in% matching_expected_missing_var]

for (index in 1:nrow(matching_possible_missing)) {
  new_var_name <- paste0("flag_missing_", matching_possible_missing[index]$"variable_id")
  if (is.na(matching_possible_missing[index]$"missing_set_to")) {
    matching_status[, eval(new_var_name) := ifelse(is.na(get(matching_possible_missing[index]$"variable_id")), TRUE, FALSE)]
  } else {
    matching_status[, eval(new_var_name) := ifelse(get(matching_possible_missing[index]$"variable_id") == matching_possible_missing[index]$"missing_set_to", TRUE, FALSE)]
  }
}

# Identify columns using grep
flag_columns <- grep("flag_missing_", names(matching_status), value = TRUE)

# Sum the identified columns row-wise
matching_status[, complete_information := fifelse(rowSums(.SD) >= 1, 0, 1), .SDcols = flag_columns]
matching_status[, (flag_columns) := NULL]

# recoding to logical/numeric where necessary
logical_cols <- c(
  "SV_PREG_STATUS", "SV_PRIOR_COVID_DG", "SV_IMMUNOCOMPROMISED",
  "twelve_month_enrolment_or_enrolled_at_birth",
  "threshold_ba1_met", "threshold_ba45_met",
  "COMP_COMORBIDITIES", "fivemonth_since_lastvac",
  "elevenmonth_since_lastvac",
  "complete_information",
  "receives_bivalent",
  "receives_first_bivalent",
  "prior_bivalent",
  "receives_any_covidvaccine",
  "older_than_sixty",
  "dead",
  "conflicting_exposure"
)
num_cols <- "SV_HIST_COVID_VACC"

matching_status[, (logical_cols) := lapply(.SD, as.logical), .SDcols = logical_cols]
matching_status[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

write_fst(matching_status, file.path(dir_d3, "matching_status_spells.fst"))
toc_log_print()

dbExecute(matching_status_conn, paste0("DETACH matching_spells"))

# Close the connection again
duckdb::dbDisconnect(matching_status_conn, shutdown = TRUE)
rm(
  matching_status_conn, matching_status,
  create_matching_status_01_explosion, create_matching_status_02_combine, create_matching_status_03_mergestatus,
  variables_status, matching_possible_missing, persons_information, new_var_name, logical_cols, num_cols, index, flag_columns, persons_in_spells
)
gc()

logr::log_print(c("[Create Matching Status] - END"))
