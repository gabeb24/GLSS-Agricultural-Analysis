# ---- Data Translation Challenge ----

# This file shows our analysis and graphs for visualizing and 
# understanding the final data set with outliers removed.


# ---- Analysis ----

# create a summary of the model
summary(rev_hh_agri_profit)


# view the correlation between all variables in the model
cor(select(rev_hh_agri_profit, -c(nh, region_district, eco_zone, urban_rural,
                                  education_level, read_max, write_max)))

# LINEAR Models

# Linear model with savannah_zone and rural zone
model_savanah_lm <- lm(data =rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     savannah_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community

summary(model_all_lm) # view summary of linear model


# Linear model with coastal_zone and rural zone
model_coastal_lm <- lm(data = rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     coastal_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community

summary(model_all_lm) # view summary of linear model


# Linear model with forest_zone and rural zone
model_forest_lm <- lm(data = rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     forest_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community

summary(model_all_lm) # view summary of linear model

# LOG MODELS
# Log model with savannah_zone and rural zone
model_savannah_log <- lm(data = rev_hh_agri_profit,
                         log(profit_per_rope) ~            # Location
                         + factor(education_max)           # Education
                                                           # Community economy
                         + prim_school + jss_school        # Community education
                         + community_coop + sharecroppers) # Community

summary(model_savannah_log)


# Log model with coastal_zone and rural zone
model_coastal_log <- lm(data = rev_hh_agri_profit,
                        log(profit_per_rope) ~ coastal_zone # Location
                        + factor(education_max)             # Education
                        + bank                              # Community economy
                                                            # Community education
                        + community_coop + sharecroppers)   # Community

summary(model_coastal_log)


# Log model with forest_zone and rural zone
model_forest_log <- lm(data = rev_hh_agri_profit,
                       log(profit_per_rope) ~ forest_zone  # Location
                       + factor(education_max)             # Education
                       + bank                              # Community economy
                       + prim_school                       # Community education
                       + community_coop + sharecroppers)   # Community

summary(model_forest_log)

plot(model_forest_log)




# LINEAR Profit_per rope - ALL VARIABLES

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


# LOG profit_per_rope - ALL VARIABLES

#Log model with savannah_zone and rural zone
model_savannah_log <- lm(data = rev_hh_agri_profit,
                        log(profit_per_rope) ~ savannah_zone + rural               # Location
                        + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                        + road + bank + daily_market + periodic_market      # Community economy
                        + prim_school + jss_school                          # Community education
                        + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_savannah_log)


#Log model with coastal_zone and rural zone
model_coastal_log <- lm(data = rev_hh_agri_profit,
                       log(profit_per_rope) ~ coastal_zone + rural               # Location
                       + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                       + road + bank + daily_market + periodic_market      # Community economy
                       + prim_school + jss_school                          # Community education
                       + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_coastal_log)


#Log model with forest_zone and rural zone
model_forest_log <- lm(data = rev_hh_agri_profit,
                      log(profit_per_rope) ~ forest_zone + rural               # Location
                      + factor(education_max) + factor(read_max) +factor (write_max) + factor (calc_max)      # Education
                      + road + bank + daily_market + periodic_market      # Community economy
                      + prim_school + jss_school                          # Community education
                      + agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid) # Community

summary(model_forest_log)

# ---- Graphs ----

# create bar chart for education
ggplot(data=rev_hh_agri_profit) +
  (mapping=aes(x =education_level,fill='red')) +  
  geom_bar()


  geom_bar()
# create bar chart for sharecroppers variable
ggplot(data=rev_hh_agri_profit) +
  (mapping=aes(x =sharecroppers,fill='red')) +  
  geom_bar()
