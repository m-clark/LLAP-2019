load('synthetic.RData')

library(tidyverse)
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

demos_anonymized_test = demos_anonymized %>% 
  mutate(award_home_dept = sample(award_home_dept),
         award_year_start = award_year_start + sample(-2:2, nrow(.), replace = T),
         award_year_end = award_year_start + sample(0:2, nrow(.), replace = T),
         award_total_amount = scales::rescale(award_total_amount, c(0, 50000000)),
         award_total_log = ifelse(award_total_amount > 0, log(award_total_amount), 0),
         age = age + sample(-5:5, nrow(.), replace = T),
         race = ifelse(is.na(race), NA, sample(race)),
         libuser = ifelse(is.na(libuser), NA, sample(libuser)),
         )

demos_anonymized_test %>% 
  filter(award_year_start < 2020 & award_year_start > 1990) %>%  
  ggplot(aes(award_year_start, award_total_log)) + 
  geom_smooth(aes(color=factor(libuser)))

write_csv(demos_anonymized_test, 'workshop_project/data/demos_anonymized.csv')
write_csv(ids_anonymized, 'workshop_project/data/ids_anonymized.csv')
write_csv(model_variables_anonymized, 'workshop_project/data/model_variables_anonymized.csv')



N = 5000
libuser = sample(0:1, N, replace = T)
gender = sample(0:1, N, replace = T)
age = rnorm(N, mean = 40, sd = 10)
age = scales::rescale(age, to = c(28, 80))
age = round(age)
amount = -500000 + 333333*libuser + 625000*gender + 500000*plogis(scale(age/10)) + 1e6*libuser*plogis(scale(age/10)) + rnorm(N, sd=5e5)
# qplot(amount, geom='density')
amount = scales::rescale(amount, to = c(10000, 10000000))

model_variables_demo = tibble(
  libuser = factor(libuser, labels = c('no', 'yes')),
  gender = factor(gender, labels = c('female', 'male')),
  age,
  award_total_amount = amount
)

library(mgcv)
plot(gam(award_total_amount ~ libuser + gender + s(age, by = factor(libuser)), data = model_variables_demo))

summary(amount)
# qplot(amount, geom='density')

save(model_variables_demo, file = 'demo.RData')
