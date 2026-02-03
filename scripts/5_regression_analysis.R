#######################
#### prerequisites ####
#######################

library(fixest)


####################################
#### check regarding singletons ####
####################################

#### for unmatched data 

full_dataset_main_clean %>%
  count(ags) %>%
  summarise(singletons = sum(n == 1),
            obs_lost   = sum(n[n == 1]))


# dataset contains singletons at the municipality level, there will be excluded before estimating the models

full_dataset_main_clean_nos <- full_dataset_main_clean %>%
  add_count(ags, name = "n_ags") %>%
  filter(n_ags > 1) %>%
  select(-n_ags)


#### for matched data

matched_data_main %>%
  count(ags) %>%
  summarise(singletons = sum(n == 1),
            obs_lost   = sum(n[n == 1]))


# dataset contains singletons at the municipality level, there will be excluded before estimating the models

matched_data_main_nos <- matched_data_main %>%
  add_count(ags, name = "n_ags") %>%
  filter(n_ags > 1) %>%
  select(-n_ags)


####################################################
#### estimate main specification without buffer ####
####################################################


###### variant 1 -- without matching

#### model estimation with successive addition of controls and regional FE  - model 1

# only treatment indicator
m1 <- feols(
  log(price_sqm) ~ school_nearby | ags,
  data = full_dataset_main_clean_nos,
  vcov = ~ags
)


# treatment indicator + housing characteristics

m2 <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) + cellar | ags,
  data = full_dataset_main_clean_nos,
  vcov = ~ags
)


# treatment indicator + housing and neighborhood characteristics

m3 <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| ags,
  data  = full_dataset_main_clean_nos,
  vcov = ~ags
)


#### estimate treatment heterogeneity model - model 2

model_heterogeneity <- feols(
  log(price_sqm) ~ school_nearby + school_nearby:abitur_nearby + living_area +  
    site_area + rooms_n + baths_n + age_building +  I(age_building^2) +  
    cellar + immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park | ags,
  data = full_dataset_main_clean_nos,
  vcov = ~ags
)


###### variant 2 -- matched data

#### model estimation with successive addition of controls and regional FE  - model 1

## only treatment indicator
m1_matched <- feols(
  log(price_sqm) ~ school_nearby | ags,
  data = matched_data_main_nos,
  vcov = ~ags
)


## treatment indicator + housing characteristics

m2_matched <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) + cellar | ags,
  data = matched_data_main_nos,
  vcov = ~ags
)


## treatment indicator + housing and neighborhood characteristics

m3_matched <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| ags,
  data  = matched_data_main_nos,
  vcov = ~ags
)


#### estimate treatment heterogeneity model - model 2

model_heterogeneity_matched <- feols(
  log(price_sqm) ~ school_nearby + school_nearby:abitur_nearby + living_area +  
    site_area + rooms_n + baths_n + age_building +  I(age_building^2) +  
    cellar + immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park | ags,
  data = matched_data_main_nos,
  vcov = ~ags
)