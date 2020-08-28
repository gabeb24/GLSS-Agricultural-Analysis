
#----Examing community data to identify  relevant variables and tidy to be joined with household data----

#Loading relevant libraries
library(tidyverse)
library(haven)
library(dplyr)


#Loading relevant datasets
community_overview <- read_dta("raw_data/community/cs0.dta")
community_econ <- read_dta("raw_data/community/cs2.dta")
community_edu <- read_dta("raw_data/community/cs3.dta")
community_crops <- read_dta("raw_data/community/cs5a.dta")
community_agg <- read_dta("raw_data/community/cs5b.dta")


#Renaming, selecting, and editing relevant variables in each table to make dummy variables consistent throughout dataset
#grouping my region, district and eanum because enumeration area number reflects 
#and adjusting each enumeration area number to match "clust" variables used in household data.
#taking the minimum values of each column because entries of "2" mean "no" and entries of "1" mean yes


community_econ <- community_econ %>% 
  group_by(region, district, eanum) %>% 
  summarize(s2q4 = min(s2q4), s2q17 = min(s2q17), s2q19 = min(s2q19), s2q20 = min(s2q20)) %>% 
  mutate(clust = eanum + 4000) %>% 
  mutate(
    road = case_when(
      s2q4 == 2 ~ 0,
      s2q4 == 1 ~ 1
    )
  ) %>% 
  mutate(
    bank = case_when(
      s2q17 == 2 ~ 0,
      s2q17 == 1 ~ 1
    )
  ) %>% 
  mutate(
    daily_market = case_when(
      s2q19 == 2 ~ 0,
      s2q19 == 1 ~ 1
    )
  ) %>% 
  mutate(
    periodic_market = case_when(
      s2q20 == 2 ~ 0,
      is.na(s2q20) ~ 0,
      s2q20 == 1 ~ 1
    )
  ) %>% 
  select(region, district, clust, road, bank, daily_market, periodic_market)
  


community_edu <- community_edu %>% 
  group_by(region, district, eanum) %>% 
  summarize(s3q1 = min(s3q1), s3q11 = min(s3q11), s3q20 = min(s3q20)) %>% 
  mutate(clust = eanum + 4000) %>% 
  mutate(
    prim_school = case_when(
      s3q1 == 2 ~ 0,
      s3q1 == 1 ~ 1
    )
  ) %>% 
  mutate(
    jss_school = case_when(
      s3q11 == 2 ~ 0,
      s3q11 == 1 ~ 0
    )
  ) %>% 
  mutate(
    sec_tech_school = case_when(
      s3q20 == 2 ~ 0,
      s3q20 == 1 ~ 1
    )
  ) %>% 
  select(region, district, clust, prim_school, jss_school)


community_agg <- community_agg %>% 
  group_by(region, district, eanum) %>% 
  summarize(s5bq5 = min(s5bq5), s5bq10 = min(s5bq10), s5bq17 = min(s5bq17), s5bq20 = min(s5bq20), s5bq23 = min(s5bq23)) %>% 
  mutate(clust = eanum + 4000) %>% 
  mutate(
    agg_ext_center = case_when(
      s5bq5 == 2 ~ 0,
      s5bq5 == 1 ~ 1
    )
  ) %>% 
  mutate(
    community_coop = case_when(
      s5bq10 == 2 ~ 0,
      s5bq10 == 1 ~ 1 
    )
  ) %>% 
  mutate(
    irrigated_fields = case_when(
      s5bq17 == 2 ~ 0,
      s5bq17 == 1 ~ 1
    )
  ) %>% 
  mutate(
    sharecroppers = case_when(
      s5bq20 == 2 ~ 0,
      s5bq20 == 1 ~ 1
    )
  ) %>% 
  mutate(
    farm_mutual_aid = case_when(
      s5bq23 == 2 ~ 0,
      s5bq23 == 1 ~ 1
    )
  ) %>% 
  select(region, district, clust, agg_ext_center, community_coop, irrigated_fields, sharecroppers, farm_mutual_aid)


#We can now join the community variables with household data based on clust.
community_full <- community_econ %>% 
  inner_join(community_edu) %>% 
  left_join(community_agg)







