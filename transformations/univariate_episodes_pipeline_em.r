# create lookback windows first for the population
# run per sv_subset
sv_subset <- read_csv("tests/testthat/data/univariate_episodes/study_variables.csv")
population <- read_csv("tests/testthat/data/univariate_episodes/D3_SPELLS.csv")
concepts <-  read_csv("tests/testthat/data/univariate_episodes/D3_CONCEPTS.csv")

for (i in uniqu(sv_subset$variable_id){
  concept <- sv_subset$concept_id[i]
  studyvariable <- sv_subset$variable_id[i]
  start_lookback <- sv_subset$start_look_back[i]
  end_lookback <- sv_subset$end_look_back[i]
  
  start_study_date <- config_values$start_study_date
  end_study_period <- config_values$
  
  intial_windows_pop <- population[
    ,
    .(
      person_id,
      lookback_start = get(anchor_date_start) + start_lookback,
      lookback_end = get(anchor_date_end) + end_lookback,
      variable_id = studyvariable
    )
  ]
}