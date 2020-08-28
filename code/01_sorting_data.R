# ---- Data Translation Challenge ----

# ---- Upload appropriate libraries ----

library(tidyverse)  # visualize data
library(haven)      # load data


# ---- Loading relevant data ----

#     Reads in data for WINDOWS

survey_info_a   <- read_dta("raw_data/sec0a.dta")   # Survey info + HH location
agri_plot_s8b   <- read_dta("raw_data/sec8b.dta")   # Agriculture - Plot Details
education       <- read_dta("raw_data/sec2a.dta")   # Education - General survey Qs
literacy        <- read_dta("raw_data/sec2c.dta")   # Education - Literacy / Apprenticeship 
agg2            <- read_dta("raw_data/aggregates/agg2.dta") # Agricultural income & farm depreciation

community_econ      <- read_dta("raw_data/community/cs2.dta")  # Economy and Infrastructure
community_edu       <- read_dta("raw_data/community/cs3.dta")  # Education
commnity_crops      <- read_dta("raw_data/community/cs5a.dta") # Agriculture (Might look into)
community_agg       <- read_dta("raw_data/community/cs5b.dta") # Agriculture

#     Reads in data for OS/MAC (local folder)

survey_info_a   <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec0a.dta")
agri_plot_s8b   <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec8b.dta") 
education       <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2a.dta")
literacy        <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2c.dta")
agg2            <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/aggregates/agg2.dta")


# ---- Agriculture code ----

# Creates base of HH that own and/or operate farm, or keep livestock, or engage in fishing
# Use Section 8B for land area

# Unit conversion source:
#   https://editorialexpress.com/cgi-bin/conference/download.cgi?db_name=CSAE2015&paper_id=708
#   1 pole = 1 acre, 9 rope = 1 acre

agri_land <- agri_plot_s8b %>%
  filter(s8bq4b == 1 |  s8bq4b == 2 | s8bq4b == 3) %>% # keep entries with units we know
  mutate(land_ropes = case_when(
      s8bq4b == 1 ~ round(s8bq4a * 9, 2),  # convert acres to rope (1:9)
      s8bq4b == 2 ~ round(s8bq4a * 9, 2),  # convert poles to rope (1:9)
      TRUE        ~ s8bq4a                  # keeps ropes (1:1)
    )
  ) %>%
  group_by(nh, clust) %>% 
  summarize(hh_land_ropes = sum(land_ropes)) %>%  # keep sum of plots for each HH
  left_join(survey_info_a)    # add location info to agriculture HH's


# ---- Education code ----

# Split the YEARS OF EDUCATION in three levels 
# Based on the Section 2A data

edu_agg <- education %>% # general education @ individual level

# Using case_when function to create dummy variables at 4 different levels of education
#   Variables: koranic_kinder_educ, prim_educ, sec_educ, educ_level

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


# LITERACY

# Selecting relevant literacy columns
literacy <- literacy %>% 
  select(1:9) %>% 
  
  # Assigning levels to reading, writing and calculation variables 
  #   to ease conversion to household level
  
  mutate(
    read_level = case_when(
      #assigns 3 if they can read english and ghanian
      #2 if they only read english
      #1 if they only read ghanian
      #0 if they don't read either
      s2cq1 == 1 & s2cq2 != 1 ~ 3,
      s2cq1 == 1 & s2cq2 == 1 ~ 2,
      s2cq1 == 2 & s2cq2 != 1 ~ 1,
      s2cq1 == 2 & s2cq2 == 1 ~ 0
    )
  ) %>% 
  
  mutate(
    write_level = case_when(
      #assigns 3 if they can write english and ghanian
      #2 if they only write english
      #1 if they only write ghanian
      #0 if they don't write either
      s2cq3 == 1 & s2cq4 != 1 ~ 3,
      s2cq3 == 1 & s2cq4 == 1 ~ 2,
      s2cq3 == 2 & s2cq4 != 1 ~ 1,
      s2cq3 == 2 & s2cq4 == 1 ~ 0,
    )
  ) %>% 
  
  mutate(
    calculation = case_when(
      s2cq5 == 1 ~ 1,
      s2cq5 == 2 ~ 0,
    )
  )

#Grouping all 3 new columns by the household member with the highest level of each:
#   Reading, Writing, Calculations

#unfortunately wasn't able to do this all by piping once
hh_read <- group_by(literacy, clust, nh) %>% 
  summarize(read_max = max(read_level))

hh_write <- group_by(literacy, clust, nh) %>% 
  summarize(write_max = max(write_level))

hh_calc <- group_by(literacy, clust, nh) %>% 
  summarize(calc_max = max(calculation))

# Combines read, write, calc
lit_levels <- hh_read %>%
  inner_join(hh_write) %>% 
  inner_join(hh_calc)


# ---- Income / Expenses / Profit ----

# rename data and select only the agricultural income 2 corrected, cluster, nh, and depreciation
#     create a new column agricultural income 2 corrected less depreciation
aggrev <- agg2 %>%
  select(clust, nh, agri1c, agri2c, hhagdepn) %>%
  filter(agri1c + agri2c + hhagdepn != 0) %>%      # filter out households w/ no agri income or expenses 
  mutate(profit = agri1c + agri2c - hhagdepn)      # income minus depreciation
  
  
options(scipen = 999) #take out scientific notation


# ---- Community ----

# Renaming, selecting, and editing relevant variables in each table to make dummy variables consistent throughout dataset
# grouping by region, district and eanum because enumeration area number reflects 
# and adjusting each enumeration area number to match "clust" variables used in household data.
# taking the minimum values of each column because entries of "2" mean "no" and entries of "1" mean yes


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
      s5bq5 == 1 ~ 1,
      is.na(s5bq5) ~ 0
    )
  ) %>% 
  mutate(
    community_coop = case_when(
      s5bq10 == 2 ~ 0,
      s5bq10 == 1 ~ 1,
      is.na(s5bq10) ~ 0
    )
  ) %>% 
  mutate(
    irrigated_fields = case_when(
      s5bq17 == 2 ~ 0,
      s5bq17 == 1 ~ 1,
      is.na(s5bq17) ~ 0
    )
  ) %>% 
  mutate(
    sharecroppers = case_when(
      s5bq20 == 2 ~ 0,
      s5bq20 == 1 ~ 1,
      is.na(s5bq20) ~ 0
    )
  ) %>% 
  mutate(
    farm_mutual_aid = case_when(
      s5bq23 == 2 ~ 0,
      s5bq23 == 1 ~ 1,
      is.na(s5bq23) ~ 0
    )
  ) %>% 
  select(region, district, clust, agg_ext_center, community_coop, irrigated_fields, sharecroppers, farm_mutual_aid)


#We can now join the community variables with household data based on clust.
community_full <- community_econ %>% 
  inner_join(community_edu) %>% 
  left_join(community_agg)

# ---- Joining Agriculture / Education / Profit ----

hh_agri_edu_profit <- aggrev %>%
  inner_join(agri_land) %>%
  left_join(hh_edu_ag) %>%   
  left_join(lit_levels) %>%
  left_join(community_full) %>%
  unite(region_district, region, district, sep = '_') %>% 
  mutate(profit_per_rope = round(profit / hh_land_ropes, 2)) %>%  # profit per area variable
  select(c(nh, clust, region_district, ez, loc2, loc5,  # Location based
           profit_per_rope,                             #Profit variable
           education_max, read_max, write_max, calc_max,   # HH Education / Literacy
           road, bank, daily_market, periodic_market,      # Community economy
           prim_school, jss_school,                        # Community education
           agg_ext_center, community_coop, irrigated_fields, sharecroppers, farm_mutual_aid # Community agriculture
           )
         )


# ---- Analysis ----

model_1 <- lm(data = hh_agri_edu_profit, profit_per_rope ~ education_max + ez)
summary(model_1)

ggplot(data = hh_agri_edu_profit, 
       mapping = aes(x = education_max, y = profit_per_rope)) +
  geom_point()

ggplot(data = hh_agri_edu_profit, 
       mapping = aes(x = profit_per_rope)) + 
  geom_histogram() +
  xlab("Profit per Rope of Land") +
  labs(title = "Profit per ropes of Land, Grouped by Household")

