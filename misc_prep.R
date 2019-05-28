load('synthetic.RData')


demos_anonymized = demos_anonymized %>% 
  mutate(
    award_year_start = lubridate::year(as.character(award_project_start_date)),
    award_year_end = lubridate::year(as.character(award_project_end_date)),
    award_total_log = log(award_total_amount+1),
    award_home_dept = str_trim(str_to_lower(award_home_dept))
  )


write_csv(demos_anonymized, 'data/demos_anonymized.csv')
write_csv(ids_anonymized, 'data/ids_anonymized.csv')
write_csv(model_variables_anonymized, 'data/model_variables_anonymized.csv')
