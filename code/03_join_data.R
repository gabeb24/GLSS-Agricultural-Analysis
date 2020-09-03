# ---- Data Translation Challenge ----

# This file joins all the sorted data into one data frame called hh_agri_profit.


# ---- Joining Agriculture / Education / Profit / Community ----

hh_agri_profit <- aggrev %>%
  left_join(survey_info_a) %>%    # add location info
  inner_join(agri_land) %>%
  left_join(hh_edu_ag) %>%   
  left_join(lit_levels) %>%
  left_join(community_full) %>%
  unite(region_district, region, district, sep = '_') %>% 
  mutate(profit_per_rope = round(profit / hh_land_ropes, 2)) %>%                             # create profit per area variable
  select(c(nh, clust, region_district, eco_zone, coastal_zone, forest_zone, savannah_zone, urban_rural, rural,                   # Location based
           profit_per_rope,                                                                  # Profit variable
           education_max, education_level, read_max, write_max, calc_max,                                     # HH Education / Literacy
           road, bank, daily_market, periodic_market,                                        # Community economy
           prim_school, jss_school,                                                          # Community education
           agg_ext_center, community_coop, irrigated_fields, sharecroppers, farm_mutual_aid  # Community agriculture
  ))

#Replaces all NA values with 0
#The 0 indicated not applicable or no for community variables
hh_agri_profit[is.na(hh_agri_profit)] <- 0


# After this point we have finalized our data set and are ready to move forward 
# with finding outliers.
