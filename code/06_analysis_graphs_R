# ---- Data Translation Challenge ----

# This file shows our analysis and graphs for visualizing and 
# understanding the final data set with outliers removed.


# ---- Analysis ----


#Linear model with savannah_zone and rural zone
model_all_lm <- lm(data =rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     savannah_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community
#view summary of linear model
summary(model_all_lm)

#Linear model with coastal_zone and rural zone
model_all_lm <- lm(data = rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     coastal_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community
#view summary of linear model
summary(model_all_lm)
#Linear model with forest_zone and rural zone
model_all_lm <- lm(data = rev_hh_agri_profit,
                   profit_per_rope ~
                     +                     forest_zone+rural
                   +  factor(education_max )+ factor(read_max) +factor (write_max) + factor (calc_max) +   # Education
                     +                      road + bank + daily_market + periodic_market +      # Community economy
                     +                      prim_school + jss_school +                          # Community education
                     +                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid)# Community
#view summary of linear model
summary(model_all_lm)

# create bar chart for education
ggplot(data=rev_hh_agri_profit) +
  (mapping=aes(x =education_level,fill='red')) +  
  geom_bar()


  geom_bar()
# create bar chart for sharecroppers variable
ggplot(data=rev_hh_agri_profit) +
  (mapping=aes(x =sharecroppers,fill='red')) +  
  geom_bar()
