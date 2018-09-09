######################################################
##### OSSC19 Crowdsourced Replication Initiative #####
#####                                            #####
##### Danilo Freire and Robert Myles McDonnell   #####
######################################################

# Set working directory
setwd("crowdsourced-replication-iniciative")
folder <- 'CRI Shared Data Folder/'

# Install jetpack to manage packages
install.packages("jetpack")
jetpack::init()

# Install necessary packages
jetpack::add("readr")
jetpack::add("dplyr")
jetpack::add("stringr")
library(dplyr)

# Read files and standardize data:
## Read L2data
l2 <- readr::read_csv(paste0(folder, "L2data.csv"))

## Read ZA2900
za29 <- readr::read_csv(paste0(folder, "ZA2900.csv"))

## za4700
za47 <- readr::read_csv(paste0(folder, "ZA4700.csv"))

# data tidying:
## za2900, summary of changes: 
### columns renamed
### v206 chosen over v207 for employment -- v207 has many NA values
### country names standardized
### 'female' column recoded to 1 (female) and 0
### education recoded into 'primary or less'; 'secondary' and 
#### 'university or more'
### employment split into 'Part-time', 'Full-time', 'Not active', 
#### 'Active unemployed'

### questions: 
#### what are the 13 countries? 
za29 <- za29 %>% 
  select(old_age_care = v39, unemployed = v41, reduce_income_diff = v42, 
         jobs = v36, female = v200, age = v201, education = v205,
         employment = v206, country = v3) %>% 
  mutate(country = case_when(
    country == 'aus' ~ 'Australia', 
    # figured out from the table at 
    # https://www.gesis.org/issp/modules/issp-modules-by-topic/role-of-government/1996/
    country == 'usa' ~ 'United_States',
    country == 'nz' ~ 'New_Zealand',
    country == 'irl' ~ 'Ireland',
    country == 'cz' ~ 'Czech Republic',
    country == 'h' ~ 'Hungary',
    country == 'gb' ~ 'Great Britain',
    country == 'bg' ~ 'Bulgaria',
    country == 'cy' ~ 'Cyprus',
    country == 'i' ~ 'Italy',
    country == 'IL-J' ~ 'Israel - jews',
    country == 'IL-A' ~ 'Israel - arabs',
    country == 'D-W' ~ 'West Germany',
    country == 'D-E' ~ 'East Germany',
    country == 'n' ~ 'Norway',
    country == 'slo' ~ 'Slovenia',
    country == 'f' ~ 'France',
    country == 'lv' ~ 'Latvia',
    country == 'j' ~ 'Japan',
    country == 'rp' ~ 'Russia',
    country == 'e' ~ 'Spain',
    country == 'cdn' ~ 'Canada',
    country == 'pl' ~ 'Poland',
    country == 's' ~ 'Sweden',
    country == 'ch' ~ 'Switzerland'
  ),
  female = ifelse(female == 'Female', 1,
                  ifelse(female == 'Male', 0, female)),
  female = as.numeric(female),
  education = case_when(
    education %in% 
      c("University compl", "Semi-higher,Incpl uni.") ~ 
      "University or more",
    education %in% c("Incpl secondary", "Secondary compl") ~ 
      "Secondary",
    TRUE ~ "Primary or less"),
  employment = case_when(
    employment == "Full-time employed,main job" ~ 'Full-time',
    employment == "Part-time employed,main job" ~ 'Part-time',
    employment == "Unemployed" ~ 'Active unemployed',
    TRUE ~ 'Not active'),
  survey_year = '1996'
  ) %>% 
  mutate_at(1:4, function(x) case_when(
    x %in% c("Definitely should be", "Probably should be") ~  '1',
    TRUE ~ '0')) %>% 
  mutate_at(1:4, as.numeric)
  

## za4700, summary of changes:
### columns renamed
### country names standardized
### entries like '826.1' in the country column were removed
### 'female' column recoded to 1 (female) and 0
### education recoded into 'primary or less'; 'secondary' and 
#### 'university or more'
### employment split into 'Part-time', 'Full-time', 'Not active', 
#### 'Active unemployed'

### questions: 
#### what are the 13 countries? Some have no name, only a number
##### e.g. '276.2'
#### should NA in the DVs be removed? (I would)
za47 <- za47 %>% 
  select(old_age_care = V28, unemployed = V30, reduce_income_diff = V31,
         jobs = V25, female = sex, age, education = degree, 
         employment = spwrkst, country = V3) %>% 
  mutate(country = stringr::str_extract(country, '-[A-Za-z ]*'),
         country = stringr::str_remove(country, '-'),
         country = stringr::str_replace(country, ' ', 
                                        replacement = '_'),
         female = ifelse(female == 'Female', 1,
                         ifelse(female == 'Male', 0, female)),
         female = as.numeric(female),
         education = case_when(
           education %in% 
             c("Higher secondary completed", 
               "Above higher secondary level,other qualification") ~ 
             "Secondary",
           education == "University degree completed, graduate studies" 
           ~ "University or more",
           TRUE ~ "Primary or less"),
         employment = case_when(
           employment == "Full-time employed,main job" ~ 'Full-time',
           employment == "Part-time employed,main job" ~ 'Part-time',
           employment == "Unemployed" ~ 'Active unemployed',
           TRUE ~ 'Not active'),
         survey_year = '2006'
         ) %>% 
  filter(!is.na(country)) %>% 
  mutate_at(1:4, function(x) case_when(
    x %in% c("Definitely should be", "Probably should be") ~  '1',
    TRUE ~ '0')) %>% # NA question arises here
  mutate_at(1:4, as.numeric)


## join za29 & za47
za <- bind_rows(za29, za47)

readr::write_csv(za, 'combined_survey_data.csv')