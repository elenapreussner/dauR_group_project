#######################
#### prerequisites ####
#######################

library(rio)
library(MatchIt)
library(car)
library(knitr)


###################################
#### check balancing condition ####
###################################


# evaluating the descriptives for our treatment and contol group
# including a t-test on mean differences indicated a significant 
# difference across groups and therefore, we check the balance condition



##### conduct balance check

balance_check <- matchit(
  school_nearby ~ living_area +  site_area + 
    rooms_n + baths_n + age_building +  cellar +
    immigrants_percents +  average_age +  disposable_income_per_capita + 
    pharmacy +  supermarket + hospital + doctors +  park,
  data = full_dataset_main_clean,
  method = NULL
)

# create an object from summary

summary(balance_check)


### applying the rule of thumb that all standard mean differences >= 0.1, 
## for some, standard mean differences exceed the threshold
## indication that CIA is violated


#### conduct matching on observable covariates
# applying nearest neighbor and matching 1:1

# estimate propensity score

ps_main <- matchit(
  school_nearby ~ living_area +  site_area + 
    rooms_n + baths_n + age_building +  cellar +
    immigrants_percents +  average_age +  disposable_income_per_capita +
    pharmacy +  supermarket + hospital + doctors +  park,
  data = full_dataset_main_clean,
  method = "nearest",
  distance = "logit",
  ratio = 1         
)


# inspection 

summary(ps_main)

# extract matched dataset

matched_data_main <- match.data(ps_main)


# check N
matched_data_main %>%
  count(school_nearby)



