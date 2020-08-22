# ---- Data Translation Challenge ----

# Upload appropriate libraries ----

library(tidyverse)  # visualize data
library(haven)      # load data


# Organize + Read in Data ----

#     Reads in data for WINDOWS
survey_info_a         <- read_dta("raw_data/sec0a.dta")  # USED Below
hh                    <- read_dta("raw_data/sec1.dta")

employment            <- read_dta("raw_data/sec4a.dta")

# Education files
education             <- read_dta("raw_data/sec2a.dta")
edu_career            <- read_dta("raw_data/sec2b.dta")
edu_lit               <- read_dta("raw_data/sec2c.dta")
education_exp         <- read_dta('raw_data/aggregates/exp1.dta')
scholarship_val       <- read_dta('raw_data/aggregates/subagg1.dta')
subagg_edu_exp        <- read_dta('raw_data/aggregates/subagg20.dta')

# Agriculture files
agri_land_s8a1        <- read_dta("raw_data/sec8a1.dta")  # USED Below
agri_livestock        <- read_dta("raw_data/sec8a2.dta")
agri_equipment        <- read_dta("raw_data/sec8a3.dta")
agri_plot_details     <- read_dta("raw_data/sec8b.dta")   # USED Below
agri_grain_harvest    <- read_dta("raw_data/sec8c1.dta")
agri_root_harvest     <- read_dta("raw_data/sec8c2.dta")
agri_seasons          <- read_dta("raw_data/sec8d.dta")
agri_other_inc        <- read_dta("raw_data/sec8e.dta")
agri_cost_exp         <- read_dta("raw_data/sec8f.dta")
agri_process          <- read_dta("raw_data/sec8g.dta")
agri_home_consumption <- read_dta("raw_data/sec8h.dta")
agri_hhid             <- read_dta("raw_data/sec8hid.dta") # Household reference number

farm_rent             <- read_dta("raw_data/aggregates/exp3.dta")
crop_exp              <- read_dta("raw_data/aggregates/exp4.dta")
processing_cost       <- read_dta("raw_data/aggregates/exp6.dta")
hp_consumption        <- read_dta("raw_data/aggregates/exp7.dta")
farm_equip_dep        <- read_dta("raw_data/aggregates/exp12.dta")

agg2                  <- read_dta("raw_data/aggregates/agg2.dta")
rent_land_inc         <- read_dta("raw_data/aggregates/agg4.dta")


subagg2               <- read_dta("raw_data/aggregates/subagg2.dta")
subagg10              <- read_dta("raw_data/aggregates/subagg10.dta")
subagg11              <- read_dta("raw_data/aggregates/subagg11.dta")
subagg12              <- read_dta("raw_data/aggregates/subagg12.dta")
subagg17              <- read_dta("raw_data/aggregates/subagg17.dta")
subagg36              <- read_dta("raw_data/aggregates/subagg36.dta")


inc_land              <- read_dta("raw_data/aggregates/inc7.dta")

inc_livestock         <- read_dta("raw_data/aggregates/inc8.dta")
livestock_exp         <- read_dta("raw_data/aggregates/exp5.dta")
livestock             <- read_dta("raw_data/sec8a2.dta")
agri_cost_exp         <- read_dta("raw_data/sec8f.dta")

sec6                  <- read_dta("raw_data/sec6.dta")  # USED Below

# Check Variable names
variables <- names(survey_info_a)
variables <- names(hh)


#   In this data frame s8aq31 is the same is incliv from inc8.dta
hh_livestock <- left_join(livestock, inc_livestock)



# ---- Agriculture code Attempt 1----

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




# ---- Agriculture code Attempt 2----


# Creates list of HH that own or operate farm, EXCLUDING livestock and fishing
#     s8bq4a - unit of measure
#     s8bq4b - farm land size
# Unit conversion source: 
#     https://editorialexpress.com/cgi-bin/conference/download.cgi?db_name=CSAE2015&paper_id=708
#     1 pole = 1 acre, 9 rope = 1 acre
# Include location information of HH
#     region - Reagion ID
#     district - District ID
#     loc2 - Urban vs. Rural
#     log5 - Accra, Other Urban, Rural Coastal, Rural Forest, Rural Savannah
#     loc3 - Accra, Other Urban, Rural
# Urban pop > 1500, semi-urban 5000 > pop < 1501, rural pop < 1500

agriculture_land <- agri_plot_details %>%
  filter(s8bq4b == 1 |  s8bq4b == 2 | s8bq4b == 3) %>% # keep entries with units we know about
  mutate(hh_plot_acres = case_when(
    s8bq4b == 3 ~ round(s8bq4a / 9, 2),  # converts rope to acres (9:1)
    TRUE ~ s8bq4a                        # keeps poles & acres (1:1)
    )
  ) %>%
  group_by(clust, nh) %>% # tidy data for one entry per HH
  summarize(hh_land_acres = sum(hh_plot_acres)) %>%
  left_join(survey_info_a) %>%    # add location info to HHs w/ plots
  select(c(nh, clust, hh_land_acres, region, district, loc2, loc3, loc5)) 


