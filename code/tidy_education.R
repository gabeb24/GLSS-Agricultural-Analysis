
#----Combining education and occupation datasets to filter  to look at individuals who work in agriculture----


#----Loading relevant libraries----
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)


#----Loading relevant datasets----
  #employment screening questions from employment over past 12 months
occupations <- read_dta("raw_data/sec4a.dta")

  #General education survey questions
education <- read_dta("raw_data/sec2a.dta")


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




