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



# conduct balance check

balance_check <- matchit(
  school_nearby ~ living_area + rooms_n + age_building + immigrants_percents + average_age,
  data = full_dataset_main_clean,
  method = NULL
)

# create an object from summary

summary(balance_check)


### applying the rule of thumb that all standard mean differences are >= 0.1, we assume that the CIA is not violated
## therefore, matching on observables is not necessary




