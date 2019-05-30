load('synthetic.RData')


demos_anonymized = demos_anonymized %>%
  mutate(
    # award_year_start = lubridate::year(as.character(award_project_start_date)),
    # award_year_end = lubridate::year(as.character(award_project_end_date)),
    award_total_log = log(award_total_amount+1),
    # award_home_dept = str_trim(str_to_lower(award_home_dept))
  )



N = 50000
libuser = sample(0:1, N, replace = T)
gender = sample(0:1, N, replace = T)
age = rnorm(N, mean = 40, sd = 10)
age = scales::rescale(age, to = c(28, 80))
age = round(age)
amount = -500000 + 333333*libuser + 625000*gender + 500000*age/10 + rnorm(N, sd=5e5)
# qplot(amount, geom='density')
summary(lm(amount ~ libuser))
summary(lm(amount ~ libuser + gender + age))

summary(amount)
# qplot(amount, geom='density')
amount = scales::rescale(amount, to = c(10000, 10000000))

summary(lm(amount ~ libuser + gender))
summary(lm(amount ~ libuser + gender + age))
  

model_variables_anonymized = tibble(
  libuser,
  gender,
  age,
  award_total_amount = amount
)



model_variables_anonymized = model_variables_anonymized %>% 
  mutate(libuser = factor(libuser, labels = c('no', 'yes')),
         gender = factor(gender, labels = c('female', 'male')))

write_csv(demos_anonymized, 'data/demos_anonymized.csv')
write_csv(ids_anonymized, 'data/ids_anonymized.csv')
write_csv(model_variables_anonymized, 'data/model_variables_anonymized.csv')
