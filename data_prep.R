######################################################
##### OSSC19 Crowdsourced Replication Initiative #####
#####                                            #####
##### Danilo Freire and Robert Myles McDonnell   #####
######################################################


##########################
##### Data Wrangling #####
##########################

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
library(readr)

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
    country == 'cz' ~ 'Czech_Republic',
    country == 'h' ~ 'Hungary',
    country == 'gb' ~ 'Great_Britain',
    country == 'bg' ~ 'Bulgaria',
    country == 'cy' ~ 'Cyprus',
    country == 'i' ~ 'Italy',
    country == 'IL-J' ~ 'Israel',
    country == 'IL-A' ~ 'Israel',
    country == 'D-W' ~ 'Germany',
    country == 'D-E' ~ 'Germany',
    country == 'n' ~ 'Norway',
    country == 'slo' ~ 'Slovenia',
    country == 'f' ~ 'France',
    country == 'lv' ~ 'Latvia',
    country == 'j' ~ 'Japan',
    country == 'rp' ~ 'Philippines',
    country == 'rus' ~ 'Russia',
    country == 'rch' ~ 'Chile',
    country == 'e' ~ 'Spain',
    country == 'cdn' ~ 'Canada',
    country == 'pl' ~ 'Poland',
    country == 's' ~ 'Sweden',
    country == 'ch' ~ 'Switzerland'
  ), 
    # as the ls data don't differentiate between West
    # and East Germany and Arab/Jews in Israel,
    # I've changed it to Germany and Israel
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
  year = '1996',
  year = as.numeric(year)
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
         employment = spwrkst, country = V3a) %>% 
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
         year = '2006',
         year = as.numeric(year)
         ) %>% 
  filter(!is.na(country)) %>% 
  mutate_at(1:4, function(x) case_when(
    x %in% c("Definitely should be", "Probably should be") ~  '1',
    TRUE ~ '0')) %>% # NA question arises here
  mutate_at(1:4, as.numeric)


## join za29 & za47
za <- bind_rows(za29, za47)
readr::write_csv(za, "za.csv")

## merge with the ls data. As the dataset contains a
## spelling error (Isreal instead of Israel), we will
## fix that small typo before joining the datasets.
## we will also rename the variables before merging
l2 <- l2 %>%
      mutate(country = case_when(
            country == 'Czech Republic' ~ 'Czech_Republic',
            country == 'Isreal' ~ 'Israel',
            country == 'New Zealand' ~ 'New_Zealand',
            country == 'cz' ~ 'Czech_Republic',
            country == 'United Kingdom' ~ 'Great_Britain',
            country == 'United States' ~ 'United_States',
            country == 'South Korea' ~ 'South_Korea',
            TRUE ~ country)) 

l2 <- l2 %>% select(employment_rate = emprate, immigrant_stock = foreignpct,
                     welfare_expenditures = socx, change_immigrant_stock = netmigpct,
                     year, country) %>% mutate_at(1:5, as.numeric) 

df <- left_join(l2, za, by = c("country", "year")) 

## change reference categories 
df <- within(df, education <- relevel(as.factor(education), ref = "Secondary")) 
df <- within(df, employment <- relevel(as.factor(employment), ref = "Full-time")) 
df <- df %>% select(year, country, everything()) 

## save data 
readr::write_csv(df, 'combined_data.csv') 

################## 
##### Models #####
##################
names(df)
summary(m1 <- glm(old_age_care ~ immigrant_stock +
                 female + age + age_squared + education + employment +
                 factor(year) + factor(country), 
                 data = df, family = binomial(link = "logit")))

summary(m2 <- glm(unemployed ~ immigrant_stock + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m3 <- glm(reduce_income_diff ~ immigrant_stock + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m4 <- glm(jobs ~ immigrant_stock + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))



summary(m5 <- glm(old_age_care ~ immigrant_stock + welfare_expenditures + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m6 <- glm(unemployed ~ immigrant_stock + welfare_expenditures + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m7 <- glm(reduce_income_diff ~ immigrant_stock + welfare_expenditures + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m8 <- glm(jobs ~ immigrant_stock + welfare_expenditures +
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))



summary(m9 <- glm(old_age_care ~ immigrant_stock + employment_rate + 
                  female + age + age_squared + education + employment +
                  factor(year) + factor(country),
                  data = df, family = binomial(link = "logit")))

summary(m10 <- glm(unemployed ~ immigrant_stock + employment_rate + 
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m11 <- glm(reduce_income_diff ~ immigrant_stock + employment_rate +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m12 <- glm(jobs ~ immigrant_stock + employment_rate +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))



summary(m13 <- glm(old_age_care ~ change_immigrant_stock +  
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m14 <- glm(unemployed ~ change_immigrant_stock + 
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m15 <- glm(reduce_income_diff ~ change_immigrant_stock + 
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m16 <- glm(jobs ~ change_immigrant_stock +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))



summary(m17 <- glm(old_age_care ~ change_immigrant_stock + welfare_expenditures +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m18 <- glm(unemployed ~ change_immigrant_stock + welfare_expenditures + 
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m19 <- glm(reduce_income_diff ~ change_immigrant_stock + welfare_expenditures +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m20 <- glm(jobs ~ change_immigrant_stock + welfare_expenditures +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))



summary(m21 <- glm(old_age_care ~ change_immigrant_stock + employment_rate +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m22 <- glm(unemployed ~ change_immigrant_stock + employment_rate + 
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m23 <- glm(reduce_income_diff ~ change_immigrant_stock + employment_rate +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))

summary(m24 <- glm(jobs ~ change_immigrant_stock + employment_rate +
                   female + age + age_squared + education + employment +
                   factor(year) + factor(country),
                   data = df, family = binomial(link = "logit")))