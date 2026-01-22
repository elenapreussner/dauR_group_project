#########################
##### preliminaries #####
#########################

### load necessary packages

library(tidyverse)
library(readxl)

#### import data

setwd("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/Data-Analysis-Using-R-Group-Project/data")

schools <- read_xlsx("school_data.xlsx")
housing_data <- read.csv("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/CampusFile_HK_2022.csv")
housing_data <- read_csv("~/Uni/Data Analysis Using R/CampusFile_HK_2022.csv")
ssi_data <- read_csv2("2022_social_index.csv")

############################
##### prepare datasets #####
############################

#### school dataset

# rename school ID

ssi_data <- ssi_data %>%
  rename(school_ID = Schulnummer,
         ssi = Sozialindexstufe)


# get information on SSI

schools <- schools %>%
  left_join(
    ssi_data %>% 
      select(school_ID, ssi),
    by = "school_ID"
  ) 


#### filter school data regarding relevant school types (secondary schools)

schools <- schools %>%
  filter(school_type %in% c(4, 10, 14, 15, 20))


#### separate grid-id for the treatment assignent

schools <- schools %>%
  separate(ergg_1km, c("x", "y"), "_", remove = FALSE, convert = TRUE)



##### housing data


### filter for houses located in NRW


housing_data_NRW <- housing_data %>%
  filter(blid == "North Rhine-Westphalia")

## remove NA's on the grid-cell ID

housing_data_NRW <- housing_data_NRW %>%
  filter(ergg_1km != -9)


#### recode controls

