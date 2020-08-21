# ---- Data Translation Challenge ----

# Upload appropriate libraries ----

library(tidyverse)  # visualize data
library(haven)      # load data
library(ggplot2)
library(dplyr)


# Organize + Read in Data ----

#     Reads in data for WINDOWS
survey_info_a         <- read_dta("raw_data/sec0a.dta")  # USED Below

# Agriculture files
agri_land_s8a1        <- read_dta("raw_data/sec8a1.dta")  # USED Below

sec6                  <- read_dta("raw_data/sec6.dta")  # USED Below

#----Loading relevant datasets----
  #employment screening questions from employment over past 12 months
occupations <- read_dta("raw_data/sec4a.dta")

  #General education survey questions
education <- read_dta("raw_data/sec2a.dta")


agg2 <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/aggregates/agg2.dta")


education <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2a.dta")


# Create base of HH that own and/or operate farm, 
#     or keep livestock, or engage in fishing
# Urban pop > 1500, semi-urban 5000 > pop < 1501, rural pop < 1500

agriculture_hh <- sec6 %>%
  select(c(nh, s6q1, clust)) %>%  # track unique HH's
  filter(s6q1 == 1) %>%           # keep only those HH own/operate farm TRUE
  left_join(survey_info_a) %>%    # add location info to remaining HH's
  select(c(nh, clust, region, district, loc2, loc3, loc5))

# Land info
#   Unit conversion source: 
#     https://editorialexpress.com/cgi-bin/conference/download.cgi?db_name=CSAE2015&paper_id=708
#     1 pole = 1 acre, 9 rope = 1 acre

agri_land <- agriculture_hh %>%
  left_join(agri_land_s8a1) %>%       # join land info for HH
  filter(s8aq3 == 1 |  s8aq3 == 2 | s8aq3 == 3) %>% # keep entries with units we know
  mutate(hh_land_acres = case_when(
      s8aq3 == 3 ~ round(s8aq4 / 9),  # converts rope to acres (9:1)
      TRUE       ~ s8aq4              # keeps poles & acres (1:1)
    )
  ) %>%
  select(c(nh, clust, hh_land_acres))





# Education

#----Joined tables at individual leve;----
edu_agg <- left_join(occupations, education)

#----Filtered occupations and education on s4aq3 which is a binary variable relating to if they worked on farm or not----
edu_agg <- edu_agg %>% 
  filter(s4aq3 == 1)


#----Using case_when function to create dummy variables at 4 different levels of education----

#first column gives a 1 to those with any education above none or NA. All NA values set to 0
edu_agg <- edu_agg %>% 
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


# Income / Expenses / Profit

#rename data and select only the aggricultural income 2 corrected, cluster, nh, and depriciation and create a new column aggricultural income 2 corrected less depriciation
aggrev<-select(agg2,clust,nh,agri2c,hhagdepn)%>%
  mutate(agri2c-hhagdepn)
#view data
view(aggrev)
#take out scientific notation
options(scipen = 999)
#view data
view(aggrev) 


