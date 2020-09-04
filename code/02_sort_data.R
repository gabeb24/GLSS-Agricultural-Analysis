# ---- Data Translation Challenge ----

# This file sorts, tidies, and joins the agricultural, education, profit, 
# and community data to be analyzed in the next file.

# Survey Info:
# Renames location variables (ez, loc2, and loc5) in Section 0A data frame to be
# more descriptive.

# Agriculture: 
# Creates base of HH that own and/or operate farm, or keep livestock, or engage 
# in fishing. Use Section 8B for land area.
# Unit conversion: 1 pole = 1 acre, 9 rope = 1 acre

# Education:
# Splits the years of education per individual in four levels. 
# Based on the Section 2A data
# Uses the case_when function to split the years of education per individual into 
# four levels: no_education, koranic_kinder_educ, prim_educ, sec_educ.
# Evaluates individuals to identify max education of a household
# Assigns a 1 value to highest level of household education and a 0 to all other levels of education.

# Literacy:
# Assigns levels to individuals based on ability to read, write and perform 
# calculations. Grouped at the household level by the member with the highest 
# level of each area.

# Income / Expenses / Profit:
# Selects the corrected agricultural income and depreciation of each household. 
# create a new agricultural profit column that accounts for depreciation of farm
# equipment.

# Community:
# Selects variables related to the agriculture, education, and economy of a 
# community. New variables are created for consistency throughout dataset. Groups
# entries by region, district and enumeration area number (related to cluster, 
# used in household data.
# Entries of "2" mean "no" and entries of "1" mean yes


# ---- Survey Info ----
survey_info_a <- survey_info_a %>% 
  mutate(
    eco_zone = case_when(
      ez == 1 ~ 'coastal',
      ez == 2 ~ 'forest',
      ez == 3 ~ 'savannah'
    )
  ) %>% 
  
  mutate(
    coastal_zone = case_when(
      ez == 1 ~ 1,
      ez != 1 ~ 0
    )
  ) %>% 
  mutate(
    forest_zone = case_when(
      ez == 2 ~ 1,
      ez != 2 ~ 0
    )
  ) %>% 
  mutate(
    savannah_zone = case_when(
      ez == 3 ~ 1,
      ez != 3 ~ 0
    )
  ) %>% 
  mutate(
    urban_rural = case_when(
      loc2 == 1 ~ 'urban',
      loc2 == 2 ~ 'rural'
    )
  ) %>% 
  mutate(
    rural = case_when(
      loc2 == 1 ~ 0,
      loc2 == 2 ~ 1
    )
  )

# ---- Agriculture: Land ----

agri_land <- agri_plot_s8b %>%
  filter(s8bq4b == 1 |  s8bq4b == 2 | s8bq4b == 3) %>% # keep entries with units we understand
  mutate(land_ropes = case_when(
    s8bq4b == 1 ~ round(s8bq4a * 9, 2),  # convert acres to rope (1:9)
    s8bq4b == 2 ~ round(s8bq4a * 9, 2),  # convert poles to rope (1:9)
    TRUE        ~ s8bq4a                 # keeps ropes (1:1)
  )) %>%
  group_by(nh, clust) %>% 
  summarize(hh_land_ropes = sum(land_ropes))  # keep sum of plots for each HH


# ---- Education code ----


edu_agg <- education %>% # general education @ individual level
  
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

# Grouped by household we now see the highest level of our 4 levels of education 
# achieved for each household

hh_edu_ag <- group_by(edu_agg, clust, nh) %>% 
  summarize(education_max = max(edu_level))

hh_edu_ag <- hh_edu_ag %>% 
  mutate(
    education_level = case_when(
      education_max == 3 ~ 'Secondary or Above',
      education_max == 2 ~ 'Primary',
      education_max == 1 ~ 'Koranic/Kinder',
      education_max == 0 ~ 'None'
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
      s2cq1 == 1 & s2cq2 != 1 ~ 'English and Ghanian',
      s2cq1 == 1 & s2cq2 == 1 ~ 'English',
      s2cq1 == 2 & s2cq2 != 1 ~ 'Ghanian',
      s2cq1 == 2 & s2cq2 == 1 ~ 'None'
    )
  ) %>% 
  
  mutate(
    write_level = case_when(
      #assigns 3 if they can write english and ghanian
      #2 if they only write english
      #1 if they only write ghanian
      #0 if they don't write either
      s2cq3 == 1 & s2cq4 != 1 ~ 'English and Ghanian',
      s2cq3 == 1 & s2cq4 == 1 ~ 'English',
      s2cq3 == 2 & s2cq4 != 1 ~ 'Ghanian',
      s2cq3 == 2 & s2cq4 == 1 ~ 'None',
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

aggrev <- agg2 %>%
  select(clust, nh, agri1c, agri2c, hhagdepn) %>%
  filter(agri1c + agri2c + hhagdepn != 0) %>%      # filter out households w/ no agri income or expenses 
  mutate(profit = agri1c + agri2c - hhagdepn)      # income minus depreciation


options(scipen = 999) #take out scientific notation


# ---- Community ----

# entries of "2" mean "no" and entries of "1" mean "yes"

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
      is.na(s3q11) ~ 0,
      s3q11 == 1 ~ 1
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


# Join the community variables with household data based on clust.
community_full <- community_econ %>% 
  inner_join(community_edu) %>% 
  left_join(community_agg)


# Now are data is tidy and ready to join

