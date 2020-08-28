

#----Examing community data to find relevant variables----

#Loading relevant libraries
library(tidyverse)
library(haven)

#Loading relevant datasets
community_overview <- read_dta("raw_data/community/cs0.dta")
community_econ <- read_dta("raw_data/community/cs2.dta")
community_edu <- read_dta("raw_data/community/cs3.dta")
community_crops <- read_dta("raw_data/community/cs5a.dta")
community_agg <- read_dta("raw_data/community/cs5b.dta")

#Renaming and subsetting relevant variables in each table

community_overview <- community_overview %>% 
  rename(more_than_1_comm = s0q1) %>% 
  select(region, district, eanum, more_than_1_comm)


community_econ <- community_econ %>% 
  rename(
    road = s2q4,
    bank = s2q17,
    daily_market = s2q19,
    periodic_market = s2q20,
  ) %>% 
  select(region, district, eanum, road, bank, daily_market, periodic_market)


community_edu <- community_edu %>% 
  rename(
    prim_school = s3q1,
    jss = s3q11,
    sec_tec_school = s3q20,
  ) %>% 
  select(region, district, eanum, prim_school, jss, sec_tec_school)


community_agg <- community_agg %>% 
  rename(
    agg_center = s5bq5,
    comm_coop = s5bq10,
    irrigation = s5bq17,
    share_crop = s5bq20,
    mutual_aid = s5bq23
  ) %>% 
  select(region, district, eanum, agg_center, comm_coop, irrigation, share_crop, mutual_aid)



#We can now join community with household based on cluster and include our market dummy variable
community_market <- community_econ %>% 
  mutate(clust = eanum + 4000) %>% 
  select(region, district, clust, daily_market)





