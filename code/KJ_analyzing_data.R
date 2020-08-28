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
    edu_level_num = case_when(
      sec_educ == 1 ~ 3,
      prim_educ == 1 ~ 2,
      koranic_kinder_educ == 1 ~ 1,
      koranic_kinder_educ == 0 ~ 0
    )
  )

#Grouped by household we now see the highest level of our 4 levels of education achieved for each household

hh_edu_ag <- group_by(edu_agg, clust, nh) %>% 
  summarize(education_max_num = max(edu_level_num)) %>%
  
  # Converts the numeric value to categorical labels.
  #   Ordered highest to lowest: sec_educ, prim_educ, kinder_educ, non_educ
  mutate(
    education_max = case_when(
      education_max_num == 3 ~ "sec_educ",
      education_max_num == 2 ~ "prim_educ", 
      education_max_num == 1 ~ "kinder_educ",
      education_max_num == 0 ~ "non_educ"
  
    )
  )


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

lit_levels <- hh_read %>%
  inner_join(hh_write) %>% 
  inner_join(hh_calc)


# ---- Income / Expenses / Profit ----

# rename data and select only the agricultural income 2 corrected, cluster, nh, and depreciation
#     create a new column agricultural income 2 corrected less depreciation
aggrev <- agg2 %>%
  select(clust, nh, agri1c, agri2c, hhagdepn) %>%
  filter(agri1c + agri2c + hhagdepn != 0) %>%      # filter out households w/ no agri income or expenses 
  mutate(profit = agri1c + agri2c - hhagdepn)      # profit = income minus depreciation


# ---- Joining Agriculture / Education / Profit ----

hh_agri_edu_profit <- aggrev %>%
  inner_join(agri_land) %>%
  left_join(hh_edu_ag) %>%   
  left_join(lit_levels) %>%
  mutate(profit_per_rope = round(profit / hh_land_ropes, 2)) %>%  # profit per area variable
  select(c(nh, clust, profit_per_rope,
           education_max, read_max, write_max, calc_max,   # Education / Literacy
           region, ez, district, loc2, loc3, loc5,         # Location based
           )
         )

options(scipen = 999) #take out scientific notation


# ---- Analysis ----

model_all <- lm(data = hh_agri_edu_profit, 
              profit_per_rope ~ education_max + read_max + write_max + calc_max +
                                region + ez + district + loc2 + loc3 + loc5)
summary(model_all)

# Variables with the highest significance: 
#     write_max, region, district, loc5

model_reduced = lm(data = hh_agri_edu_profit,
                   profit_per_rope ~ education_max*write_max + region + district + loc5)
summary(model_reduced)

ggplot(data = hh_agri_edu_profit, 
       mapping = aes(x = read_max, y = profit_per_rope)) +
  geom_point() +
  geom_smooth()

ggplot(data = hh_agri_edu_profit,
       mapping = aes(x = education_max)) +
  geom_bar()

ggplot(data = hh_agri_edu_profit,
       mapping = aes(x = education_max, y = profit_per_rope)) +
  geom_boxplot()

ggplot(data = hh_agri_edu_profit, 
       mapping = aes(x = profit_per_rope)) + 
  geom_histogram() +
  xlab("Profit per Rope of Land") +
  labs(title = "Profit per ropes of Land, Grouped by Household")
plot(model_reduced)

