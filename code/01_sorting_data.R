# ---- Data Translation Challenge ----

# ---- Upload appropriate libraries ----

library(tidyverse)  # visualize data
library(haven)      # load data
library(ggplot2)
library(dplyr)


# ---- Loading relevant data ----

#     Reads in data for WINDOWS

survey_info_a   <- read_dta("raw_data/sec0a.dta")   # Survey info + HH location
agri_land_s8a1  <- read_dta("raw_data/sec8a1.dta")  # Agriculture files
sec6            <- read_dta("raw_data/sec6.dta")    # ID farm vs. non-farm HH
occupations     <- read_dta("raw_data/sec4a.dta")   # Form employment - past 12 months
education       <- read_dta("raw_data/sec2a.dta")   # General education survey questions
agg2            <- read_dta("raw_data/aggregates/agg2.dta") # Agricultural income & farm depreciation

#     Reads in data for OS/MAC

survey_info_a   <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec0a.dta")
agri_land_s8a1  <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec8a1.dta")  
sec6            <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec6.dta")  
occupations     <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec4a.dta")
education       <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2a.dta")
agg2            <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/aggregates/agg2.dta")


# ---- Agriculture code ----

# Create base of HH that own and/or operate farm, 
#     or keep livestock, or engage in fishing

agriculture_hh <- sec6 %>%
  select(c(nh, s6q1, clust)) %>%  # track unique HH's
  filter(s6q1 == 1) %>%           # keep only those HH own/operate farm TRUE
  left_join(survey_info_a) %>%    # add location info to remaining HH's
  select(c(nh, clust, region, district, loc2, loc3, loc5))

# Land info
# Unit conversion source: 
#   https://editorialexpress.com/cgi-bin/conference/download.cgi?db_name=CSAE2015&paper_id=708
#   1 pole = 1 acre, 9 rope = 1 acre

agri_land <- agriculture_hh %>%
  left_join(agri_land_s8a1) %>%       # join land info for HH
  filter(s8aq3 == 1 |  s8aq3 == 2 | s8aq3 == 3) %>% # keep entries with units we know
  mutate(hh_land_acres = case_when(
      s8aq3 == 3 ~ round(s8aq4 / 9),  # converts rope to acres (9:1)
      TRUE       ~ s8aq4              # keeps poles & acres (1:1)
    )
  ) %>%
  select(c(nh, clust, hh_land_acres))


# ---- Education code ----

edu_agg <- left_join(occupations, education) %>% # join tables @ individual level
  filter(s4aq3 == 1) %>% # Worked on farm


# Using case_when function to create dummy variables at 4 different levels of education

#first column gives a 1 to those with any education above none or NA. All NA values set to 0
  mutate(
    koranic_kinder_educ = case_when(
      is.na(s2aq2) ~ 0,
      s2aq2 == 01 ~ 0,
      TRUE ~ 1
      )
    ) %>% 
  
#prim_educ column gives a 1 to those only with primary education or above
  mutate(
    prim_educ = case_when(
      is.na(s2aq2) ~ 0,
      s2aq2 <= 02 ~ 0,
      s2aq2 == 17 ~ 0,
      s2aq2 == 03 ~ 1,
      TRUE ~ 1
    )
  ) %>% 
  
#sec_educ column gives a 1 to those only with secondary education or above
  mutate(
    sec_educ = case_when(
      is.na(s2aq2) ~ 0,
      s2aq2 == 17 ~ 0,
      s2aq2 <= 3 ~ 0,
      TRUE ~ 1
    )
  ) %>% 
  
#edu_level gives a 3 for secondary or above, 2 for primary or above, 1 for koranic/kinder or above
  #0 otherwise
  mutate(
    edu_level = case_when(
      sec_educ == 1 ~ 3,
      prim_educ == 1 ~ 2,
      koranic_kinder_educ == 1 ~ 1,
      koranic_kinder_educ == 0 ~ 0
    )
  )

#Grouped by household we now see the highest level of our 4 levels of education achieved for each household

hh_edu_ag <- group_by(edu_agg, clust, nh) %>% 
  summarize(education_max = max(edu_level))


# ---- Income / Expenses / Profit ----

# rename data and select only the agricultural income 2 corrected, cluster, nh, and depreciation
#     create a new column agricultural income 2 corrected less depreciation
aggrev <- select(agg2, clust, nh, agri2c, hhagdepn) %>%
  mutate(agri2c - hhagdepn)

options(scipen = 999) #take out scientific notation


# ---- Joining Agriculture / Education / Profit ----

hh_agri_edu_profit <- left_join(agri_land, hh_edu_ag) %>%   # more observations in edu than agri
  left_join(aggrev)

