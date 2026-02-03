#####################################################
#### treatment-assignment for robustness checks #####
#####################################################
# here, we exclude "buffer-cells" with the distance of 2 around the treated-cells

library(tidyverse)
library(fixest)
library(MatchIt)


#### create treated-cell dataset for secondary schools (first order treatment)

treated_cells_robustness <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1)) %>% # in this case, treatment area is smaller
      transmute(
        x = x + dx,
        y = y + dy,
        school_tag = if_else(dx == 0 & dy == 0, # keep relevant information on schools and create auxiliary variables for inspection
                             as.character(school_ID),
                             paste0(school_ID, "t")),
        school_type_tag = if_else(dx == 0 & dy == 0,
                                  as.character(school_type),
                                  paste0(school_type, ("t")))
      )
  ) %>%
  ungroup()

treated_cells_robustness <- treated_cells_robustness %>% # reassemble grid-id
  mutate(ergg_1km = paste0(x, "_", y))


### remove all multiple treated cells

treated_once_robustness <- treated_cells_robustness %>%
  group_by(ergg_1km)%>%
  filter(n() == 1) %>%
  ungroup()


#### create buffer-cells for the exclusion

buffer_cells_robustness <- schools %>%
  rowwise() %>% # get only the "circle" of cells with a distance of 2 around the school-cell
  reframe(
    tibble(
      x = c(
        x+2, x+2, x+2, x+2, x+2,
        x,   x+1, x-1, x-2,
        x-2, x-2, x-2, x-2,
        x,   x-1, x+1
      ),
      y = c(
        y,   y+1, y+2, y-1, y-2,
        y-2, y-2, y-2, y-2,
        y,   y-1, y+1, y+2,
        y+2, y+2, y+2
      )
    )
  ) %>%
  ungroup() %>%
  distinct()

buffer_cells_robustness <- buffer_cells_robustness %>% # reassemble grid-id
  mutate(ergg_1km = paste0(x, "_", y))


#### check for overlaps

any(treated_once_robustness$ergg_1km %in% buffer_cells_robustness$ergg_1km)

overlap_ids <- intersect(
  treated_cells_robustness$ergg_1km,
  buffer_cells_robustness$ergg_1km
)

length(overlap_ids)


#### create non-treated cells

control_cells_robustness <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = -4:4, dy = -4:4) %>%
      mutate(dist = pmax(abs(dx), abs(dy))) %>%
      filter(dist %in% c(3, 4)) %>% # the same as for the main treatment assignment
      transmute(
        x = x + dx,
        y = y + dy,
        distanz = dist,
        school_tag = paste0(school_ID, "c")
      )
  ) %>%
  ungroup() %>%
  distinct()

control_cells_robustness <- control_cells_robustness %>% # reassemble grid-id
  mutate(ergg_1km = paste0(x, "_", y))


#### match datasets using grid-identifyer

all_cells_robustness <- bind_rows(
  treated_once_robustness %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L, # auxiliary variables for cell-status
      buffer  = 0L,
      control = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag),
      school_type_tag = as.character(school_type_tag)
    ),
  buffer_cells_robustness %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L, # auxiliary variables for cell-status
      buffer  = 1L,
      control = 0L,
      source  = "buffer_cells",
      school_tag = NA_character_,
      school_type = NA_character_,
    ),
  control_cells_robustness %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L, # auxiliary variables for cell-status
      buffer  = 0L,
      control = 1L,
      source = "control_cells",
      school_tag = NA_character_,
    )
) %>%
  group_by(ergg_1km, x, y) %>% # organize relevant information on auxiliary variables, treatment-status and school per grid-id
  summarise(
    treated = max(treated),
    buffer  = max(buffer),
    control = max(control),
    source  = paste(sort(unique(source)), collapse = " + "),
    school_tag = if (all(is.na(school_tag))) NA_character_
    else paste(sort(unique(na.omit(school_tag))), collapse = ", "),
    school_type = if (all(is.na(school_type_tag))) NA_character_
    else paste(sort(unique(na.omit(school_type_tag))), collapse = ", "),
    .groups = "drop"
  )


#### create cell data-set without overlaps of treated/controls with buffer

all_cells_robustness_clean <- all_cells_robustness %>%
  filter(buffer != 1)


#### code final treatment-variable for robustness check

all_cells_robustness_clean <- all_cells_robustness_clean %>%
  mutate(
    school_nearby = case_when(
      treated == 1 & control == 0 ~ 1L, # unambiguous treated cells
      treated == 1 & control == 1 ~ 1L, # for overlaps between treated cells and controls, they are consiered as treated
      treated == 0 & control == 1 ~ 0L, # control cells
      TRUE ~ NA_integer_
    )
  ) 


##### code second order treatment

all_cells_robustness_clean <- all_cells_robustness_clean %>% # get school-type for every cell
  mutate(school_type_code = sub("t$", "", school_type))

all_cells_robustness_clean <- all_cells_robustness_clean %>% # if cell lies within x of a school offering academic track (1), otherwise (0)
  mutate(abitur_nearby = if_else(school_type_code %in% c(15, 20), 1L, 0L))

all_cells_robustness_clean <- all_cells_robustness_clean %>% # get school-id for every cell to import school data
  mutate(school_tag_code = sub("t$", "", school_tag))


#### match information with housing data

full_dataset_robustness <- housing_full %>% # import school data in the prepared housing data set for each property
  left_join(all_cells_robustness_clean, by = "ergg_1km")


#### exclude houses not lying in treated or control cells

full_dataset_robustness_clean <- full_dataset_robustness %>%
  filter(
    !if_all( # if a property has no information on the variables, it can be exluded since it is not located in any relevant cell
      c(
        treated,
        buffer,
        control,
        source,
        school_tag,
        school_type,
        school_nearby,
        school_type_code,
        abitur_nearby,
        school_tag_code
      ),
      is.na
    )
  )


##check N

full_dataset_robustness_clean %>%
  count(school_nearby)


#### clean dataset of unnecessary variables and ovservations with NA's on relevant variables

full_dataset_robustness_clean <- full_dataset_robustness_clean %>%
  select(-geometry, -x, -y, -treated, -control, -source, -school_tag, 
         -school_type) %>%
  drop_na(
    living_area,
    site_area,
    rooms_n,
    baths_n,
    age_building,
    cellar,
    price_sqm,
    immigrants_percents,
    average_age,
    disposable_income_per_capita,
    pharmacy,
    supermarket,
    hospital,
    doctors,
    park,
    ags,
  )


#############################
#### regression analysis ####
#############################

##### conduct matching procedure for robustness data

# estimate propensity score

ps_robustness <- matchit(
  school_nearby ~ living_area +  site_area + 
    rooms_n + baths_n + age_building +  cellar +
    immigrants_percents +  average_age +  disposable_income_per_capita +
    pharmacy +  supermarket + hospital + doctors +  park,
  data = full_dataset_robustness_clean,
  method = "nearest",
  distance = "logit",
  ratio = 1         
)


# inspection 

summary(ps_robustness)


# extract matched dataset

matched_data_robustness <- match.data(ps_robustness)


##### check for and remove singletons

#### for unmatched data 

full_dataset_robustness_clean %>%
  count(ags) %>%
  summarise(singletons = sum(n == 1),
            obs_lost   = sum(n[n == 1]))



# dataset contains singletons at the municipality level, there will be excluded before estimating the models

full_dataset_robustness_clean_nos <- full_dataset_robustness_clean %>%
  add_count(ags, name = "n_ags") %>%
  filter(n_ags > 1) %>%
  select(-n_ags)


#### for matched data

matched_data_robustness %>%
  count(ags) %>%
  summarise(singletons = sum(n == 1),
            obs_lost   = sum(n[n == 1]))


# dataset contains singletons at the municipality level, there will be excluded before estimating the models

matched_data_robustness_nos <- matched_data_robustness %>%
  add_count(ags, name = "n_ags") %>%
  filter(n_ags > 1) %>%
  select(-n_ags)


#### main specification ##### 

#### unmatched data

## treatment indicator + housing and neighborhood characteristics

model_robustness <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| ags,
  data  = full_dataset_robustness_clean_nos,
  vcov = ~ags
)


#### estimate treatment heterogeneity model - model 2

model_heterogeneity_robustness <- feols(
  log(price_sqm) ~ school_nearby + school_nearby:abitur_nearby + living_area +
    site_area + rooms_n + baths_n + age_building +  I(age_building^2) +  
    cellar + immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park | ags,
  data = full_dataset_robustness_clean_nos,
  vcov = ~ags
)


#### matched data

model_robustness_matched <- feols(
  log(price_sqm) ~ school_nearby + living_area +  site_area + 
    rooms_n + baths_n + age_building +  I(age_building^2) +  cellar +
    immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park| ags,
  data  = matched_data_robustness_nos,
  vcov = ~ags
)


#### estimate treatment heterogeneity model - model 2

model_heterogeneity_robustness_matched <- feols(
  log(price_sqm) ~ school_nearby + school_nearby:abitur_nearby + living_area +  
    site_area + rooms_n + baths_n + age_building +  I(age_building^2) +  
    cellar + immigrants_percents +  average_age +  pharmacy +  supermarket +
    hospital + doctors +  park | ags,
  data = matched_data_robustness_nos,
  vcov = ~ags
)
