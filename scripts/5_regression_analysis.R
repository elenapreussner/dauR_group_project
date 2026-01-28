#######################
#### prerequisites ####
#######################

library(olsrr)
library(lmtest)
library(sandwich)
library(zoo)


####################################################
#### Estimate main specification without buffer ####
####################################################

# define regression formular


formular_main <- formula(log(price_sqm) ~ school_nearby + living_area +  site_area + 
                           rooms_n + baths_n + age_building +  I(age_building^2)+  cellar +
                           immigrants_percents +  average_age +  pharmacy +  supermarket +
                           hospital + doctors +  park)

formular_interaction <- formula(log(price_sqm) ~ school_nearby + school_nearby * abitur_nearby + living_area +  site_area + 
                                  rooms_n + baths_n + age_building +  I(age_building^2)+  cellar +
                                  immigrants_percents +  average_age +  pharmacy +  supermarket +
                                  hospital + doctors +  park)




# estimate models for analysis- dataset 


main_specification <- lm(formular_main, data = full_dataset_main_clean)

interaction_specification <- lm(formular_interaction, data = full_dataset_main_clean)



# estimation using robust standard errors

main_coeftest <- coeftest(
  main_specification,
  vcov. = vcovHC(main_specification, type = "HC3")
)


interaction_coeftest <- coeftest(
  interaction_specification,
  vcov. = vcovHC(interaction_specification, type = "HC3")
)










