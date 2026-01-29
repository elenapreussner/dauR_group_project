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
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park,
  data = full_dataset_main_clean,
  method = NULL
)

# create an object from summary

summary(balance_check)


### applying the rule of thumb that all standard mean differences are >= 0.1, we assume that the CIA is not violated for the most controls
## for some, standard mean differences exceed the thumb rule
## CIA is violated


#### conduct matching on observable covariates

# estimate propensity score

ps_baseline <- matchit(
  school_nearby ~ living_area +  site_area + 
    rooms_n + baths_n + age_building +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park,
  data = full_dataset_main_clean,
  method = "nearest",
  distance = "logit",
  ratio = 1         
)


# inspection 

summary(ps_baseline)

# exctract matched dataset

matched_data <- match.data(ps_baseline)


matched_data %>%
  count(school_nearby)

