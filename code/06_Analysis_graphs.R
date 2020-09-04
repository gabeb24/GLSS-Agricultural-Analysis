# ---- Data Translation Challenge ----

# This file shows our analysis and graphs for visualizing and 
# understanding the final data set with outliers removed.


# ---- Analysis ----

# LINEAR Profit_per rope

#Linear model with savannah_zone and rural zone
model_savannah_lm <- lm(data = rev_hh_agri_profit,
                        profit_per_rope ~ savannah_zone + rural               # Location
                        + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                        + road + bank + daily_market + periodic_market      # Community economy
                        + prim_school + jss_school                          # Community education
                        + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_savannah_lm)


#Linear model with coastal_zone and rural zone
model_coastal_lm <- lm(data = rev_hh_agri_profit,
                       profit_per_rope ~ coastal_zone + rural               # Location
                       + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                       + road + bank + daily_market + periodic_market      # Community economy
                       + prim_school + jss_school                          # Community education
                       + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_coastal_lm)


#Linear model with forest_zone and rural zone
model_forest_lm <- lm(data = rev_hh_agri_profit,
                      profit_per_rope ~ forest_zone + rural               # Location
                      + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                      + road + bank + daily_market + periodic_market      # Community economy
                      + prim_school + jss_school                          # Community education
                      + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_forest_lm)


# LOG profit_per_rope

# Log model with savannah_zone and rural zone
model_savannah_log <- lm(data = rev_hh_agri_profit,
                         log(profit_per_rope) ~ savannah_zone + rural               # Location
                         + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                         + road + bank + daily_market + periodic_market      # Community economy
                         + prim_school + jss_school                          # Community education
                         + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_savannah_log)


# Log model with coastal_zone and rural zone
model_coastal_log <- lm(data = rev_hh_agri_profit,
                        log(profit_per_rope) ~ coastal_zone + rural               # Location
                        + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                        + road + bank + daily_market + periodic_market      # Community economy
                        + prim_school + jss_school                          # Community education
                        + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_coastal_log)


# Log model with forest_zone and rural zone
model_forest_log <- lm(data = rev_hh_agri_profit,
                       log(profit_per_rope) ~ forest_zone + rural               # Location
                       + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                       + road + bank + daily_market + periodic_market      # Community economy
                       + prim_school + jss_school                          # Community education
                       + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_forest_log)


# create bar chart for road variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =road)) +  
  geom_bar()
# create bar chart for prim_school variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =prim_school)) +  
  geom_bar()
# create bar chart for jss_school variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =jss_school)) +  
  geom_bar()
# create bar chart for sharecroppers variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =sharecroppers)) +  
  geom_bar()
# create bar chart for farm_mutual_aid variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =farm_mutual_aid)) +  
  geom_bar()
# create bar chart for coastal_zone variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =coastal_zone)) +  
  geom_bar()
# create bar chart for forest_zone variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =forest_zone)) +  
  geom_bar()
# create bar chart for rural variable
ggplot(data=hh_agri_profit) +
  (mapping=aes(x =rural)) +  
  geom_bar()
