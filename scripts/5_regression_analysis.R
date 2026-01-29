#######################
#### prerequisites ####
#######################

library(olsrr)
library(lmtest)
library(sandwich)
library(modelsummary)
library(zoo)
library(fixest)

####################################################
#### Estimate main specification without buffer ####
####################################################


#### model estimation with successive addition of controls and regional FE  - Model 1


# only treatment indicator
m1 <- feols(
  log(price_sqm) ~ school_nearby | gid2019,
  data = full_dataset_main_clean,
  vcov = "HC3"
)



# treatment indicator + housing characteristics

m2 <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) | gid2019,
  data = full_dataset_main_clean,
  vcov = "HC3"
)



# treatment indicator + housing and neighborhood characteristics

m3 <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| gid2019,
  data  = full_dataset_main_clean,
  vcov = "HC3"
)


## create model summary for main specification

modelsummary(
  list(
    "Baseline" = m1,
    "Model 2"  = m2,
    "Model 3"  = m3
  ),
  coef_map = c(
    "school_nearby"        = "School nearby",
    "living_area"          = "living area",
    "site_area"            = "Site area",
    "rooms_n"              = "Number of rooms",
    "baths_n"              = "Number of bathrooms",
    "age_building"         = "Building age",
    "I(age_building^2)"    = "Building age squared",
    "cellar"               = "Cellar",
    "immigrants_percents"   = "Immigrants (%)",
    "average_age"          = "Average age",
    "pharmacy"             = "Pharmacy nearby",
    "supermaket"           = "Supermarket nearby",
    "hospital"             = "Hospital nearby",
    "doctors"              = "Doctor nearby",
    "park"                 = "Park nearby"
  ),
  add_rows = data.frame(
    term = "Region fixed effects",
    Baseline = "✔",
    `Model 2` = "✔",
    `Model 3` = "✔",
    check.names = FALSE
  ),
  gof_map = data.frame(
    raw   = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt   = c(0, 3)
  ),
  title = "Effect of secondary School Proximity on House Prices",
  stars = TRUE,
  gof_omit = "IC|Log|Adj",
  notes = "Note: Robust standard errors (HC3) in parentheses."
)



#### Estimate treatment heterogeneity model - Model 2


model_heterogeneity <- lm(
  log(price_sqm) ~ school_nearby * abitur_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2)+  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park,
  data = full_dataset_main_clean,
  vcov = "HC3"
)



modelsummary(
  model_heterogeneity,
  vcov = "HC3",
  statistic = "({std.error})",
  stars = c('*' = .10, '**' = .05, '***' = .01),
  coef_map = c(
    "school_nearby" = "School nearby",
    "abitur_nearby" = "Gymnasium nearby",
    "school_nearby:abitur_nearby" = "School × Gymnasium",
    "living_area" = "Living area",
    "site_area" = "Site area",
    "rooms_n" = "Number of rooms",
    "baths_n" = "Number of bathrooms",
    "age_building" = "Building age",
    "I(age_building^2)" = "Building age²",
    "cellar" = "Cellar",
    "immigrants_percents" = "Immigrants (%)",
    "average_age" = "Average age",
    "pharmacy" = "Pharmacy nearby",
    "supermarket" = "Supermarket nearby",
    "hospital" = "Hospital nearby",
    "doctors" = "Doctors nearby",
    "park" = "Park nearby"
  ),
  gof_map = data.frame(
    raw   = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt   = c(0, 3)
  ),
  add_rows = data.frame(
    term = "Region fixed effects",
    Baseline = "✔",
    `Model 2` = "✔",
    `Model 3` = "✔",
    check.names = FALSE
  ),
  title = "Effect of School with academic track Proximity on House Prices",
  stars = TRUE,
  gof_omit = "IC|Log|Adj",
  notes = "Note: Robust standard errors (HC3) in parentheses."
)




###############################################
####### second variant for matched data #######
###############################################


# only treatment indicator
m1_matched <- feols(
  log(price_sqm) ~ school_nearby | gid2019,
  data = matched_data,
  vcov = "HC3"
)



# treatment indicator + housing characteristics

m2_matched <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) | gid2019,
  data = matched_data,
  vcov = "HC3"
)



# treatment indicator + housing and neighborhood characteristics

m3_matched <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| gid2019,
  data  = matched_data,
  vcov = "HC3"
)


## create model summary for main specification

modelsummary(
  list(
    "Baseline" = m1_matched,
    "Model 2"  = m2_matched,
    "Model 3"  = m3_matched
  ),
  coef_map = c(
    "school_nearby"        = "School nearby",
    "living_area"          = "living area",
    "site_area"            = "Site area",
    "rooms_n"              = "Number of rooms",
    "baths_n"              = "Number of bathrooms",
    "age_building"         = "Building age",
    "I(age_building^2)"    = "Building age squared",
    "cellar"               = "Cellar",
    "immigrants_percents"   = "Immigrants (%)",
    "average_age"          = "Average age",
    "pharmacy"             = "Pharmacy nearby",
    "supermaket"           = "Supermarket nearby",
    "hospital"             = "Hospital nearby",
    "doctors"              = "Doctor nearby",
    "park"                 = "Park nearby"
  ),
  add_rows = data.frame(
    term = "Region fixed effects",
    Baseline = "✔",
    `Model 2` = "✔",
    `Model 3` = "✔",
    check.names = FALSE
  ),
  gof_map = data.frame(
    raw   = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt   = c(0, 3)
  ),
  title = "Effect of secondary School Proximity on House Prices",
  stars = TRUE,
  gof_omit = "IC|Log|Adj",
  notes = "Note: Robust standard errors (HC3) in parentheses."
)











