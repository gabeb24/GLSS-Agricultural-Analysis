# ---- Data Translation Challenge ----

# This file shows our analysis and graphs for visualizing and 
# understanding the final data set with outliers removed.


# ---- Analysis ----

# create a summary of the model
summary(hh_agri_profit)

# view the correlation between all variables in the model
cor(select(hh_agri_profit, -c(region_district, eco_zone, urban_rural, area_type)))


# Reduced linear model
model_lm <- lm(data = hh_agri_profit, 
               profit_per_rope ~
                 road +                      # Community economy
                 prim_school * jss_school +  # Community education
                 community_coop + sharecroppers + farm_mutual_aid # Community agriculture
)

summary(model_lm)
plot(model_lm)


# Reduced log model
model_log <- lm(data = hh_agri_profit, 
                log(profit_per_rope) ~
                  I(education_max^2) + education_max +   # HH Education / Literacy
                  road + bank +                          # Community economy
                  prim_school * jss_school +             # Community education
                  community_coop + sharecroppers + farm_mutual_aid # Community agriculture
)

summary(model_log)
plot(model_log)


# model_all_lm <- lm(data = hh_agri_profit,
#                 profit_per_rope ~
#                   education_max + read_max + write_max + calc_max +   # Education
#                   road + bank + daily_market + periodic_market +      # Community economy
#                   prim_school + jss_school +                          # Community education
#                   agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid # Community agriculture
# )
# summary(model_all_lm)
# 
# model_all_log <- lm(data = hh_agri_profit,
#                    log(profit_per_rope) ~
#                      education_max + read_max + write_max + calc_max +   # Education
#                      road + bank + daily_market + periodic_market +      # Community economy
#                      prim_school + jss_school +                          # Community education
#                      agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid # Community agriculture
# )
# summary(model_all_log)



# model_community <- lm(data = hh_agri_profit, 
#               profit_per_rope ~ 
#                 road + bank + daily_market + periodic_market +    # Community economy
#                 prim_school + jss_school +                        # Community education
#                 agg_ext_center + community_coop + irrigated_fields + sharecroppers + farm_mutual_aid # Community agriculture
#               )
# summary(model_community)
# 

# --- Analysis Graphs ----

ggplot(data = hh_agri_profit, aes(x = index, y = profit_per_rope)) +
  geom_point()

plot(hh_agri_profit$profit_per_rope)

hh_agri_profit %>%
  select() %>%
  plot(hh_agri_profit$profit_per_rope)

boxplot(data=hh_agri_profit, profit_per_rope ~ locality + eco.zone, las=2)

ggplot(data=cleaned.df, aes(x=eco.zone, y=profit.per.acre)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=eco.zone, y=profit)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=region, y=profit)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=locality, y=profit)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=attended.school, y=profit, group=attended.school)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=can.read.ghanaian, y=profit, group=can.read.ghanaian)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=can.read.english, y=profit, group=can.read.english)) + geom_boxplot()
ggplot(data=cleaned.df, aes(x=reorder(head.edu.level, profit), y=profit)) + geom_boxplot() + theme(axis.text.x = element_text(angle=90))



# create linear model of model 1
model_1 <- lm(data=hh_agri_profit,
              profit_per_rope ~ education_max  + 
                education_max + read_max + write_max + calc_max +   
                road + bank + daily_market+prim_school+jss_school+community_coop + sharecroppers + farm_mutual_aid )
# view summary of linear model
summary(model_1)

# create scatter plot education max, calc max, read max, write max
ggplot(data = model_1,
       mapping = aes(x = education_max + write_max + calc_max + read_max,
                     y = profit_per_rope)) +
  geom_point( mapping = aes(color='education_max'))

ggplot(data = model_1,
       mapping = aes(x = education_max, 
                     y = profit_per_rope,
                     color = 'educartin_max'))+
  geom_point(alpha = 0.1) +
  geom_point(mapping = aes(x = jss_school,
                           y = profit_per_rope,
                           color = 'jss_school'))+
  geom_point(alpha = 0.1)+
  geom_point(mapping = aes(x = prim_school,
                           y = profit_per_rope,
                           color = 'prim_school'))+
  geom_point(alpha = 0.1)+
  xlab("Level of Education")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Level of Education")

#create density plot calc max and education max and read max and write max combined
ggplot(data = model_1)+ 
  geom_density( mapping = aes(x = road, fill = 'road',))+
  geom_density( mapping = aes(x = bank, fill = 'bank',))+
  geom_density( mapping = aes(x = community_coop, fill = 'community_coop',))+
  geom_density( mapping = aes(x = sharecroppers, fill = 'sharecroppers',))+
  geom_density( mapping = aes(x = farm_mutual_aid, fill = 'farm_mutual_aid'))+
  xlab("Number of Banks, Community_ Coop, Farm_mutual_aid, road and Sharcroppers in Community") +
  labs(title = "Density of Community Features Per Household")

#create density plot of education
ggplot(data = model_1)+
  geom_density( mapping = aes(x = education_max, fill = 'education_max',))+
  geom_density( mapping = aes(x = prim_school, fill = 'prim_school',))+
  geom_density( mapping = aes(x = jss_school, fill = 'jss_school',))+
  xlab("Amount of Education") +
  labs(title = "Dnsity of Education Per Household")

#create histogram community 
ggplot(data = model_1)+ 
  geom_histogram( mapping = aes(x = road, fill ='road',))+
  geom_histogram( mapping = aes(x = bank, fill = 'bank',))+
  geom_histogram( mapping = aes(x = community_coop, fill = 'community_coop',))+
  geom_histogram( mapping = aes(x = sharecroppers, fill = 'sharecroppers',))+
  geom_histogram( mapping = aes(x = farm_mutual_aid, fill = 'farm_mutual_aid'))+
  xlab("Number of Banks, Community_ Coop, Farm_mutual_aid, road and Sharcroppers in Community") +
  labs(title = "Number of Community Features Per Household")

#create histogramof education
ggplot(data = model_1)+
  geom_histogram( mapping = aes(x = education_max, fill = 'education_max',))+
  geom_histogram( mapping = aes(x = prim_school, fill = 'prim_school',))+
  geom_histogram( mapping = aes(x = jss_school, fill = 'jss_school',))+
  xlab("Level") +
  
  #create scatter plot of education
  ggplot(data=model_1,
         mapping = aes(x = education_max,
                       y = profit_per_rope,
                       color = 'education_max')) +
  geom_point(alpha = 0.1) +
  geom_point( mapping = aes(x = jss_school,
                            y = profit_per_rope,
                            color = 'jss_school')) +
  geom_point(alpha = 0.1)+
  geom_point( mapping = aes(x = prim_school,
                            y = profit_per_rope,
                            color = 'prim_school')) +
  geom_point(alpha = 0.1)+
  xlab("Level of Education")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Level of Education")

#create scatter plot of community
ggplot(data=model_1,
       mapping = aes(x = bank,
                     y = profit_per_rope,
                     color = 'bank'))+
  geom_point(alpha = 0.1)+
  geom_point( mapping = aes(x = community_coop,
                            y = profit_per_rope,
                            color = 'community_coop'))+
  geom_point(alpha = 0.1)+
  geom_point( mapping = aes(x = farm_mutual_aid,
                            y = profit_per_rope,
                            color = 'farm_mutual_aid'))+
  geom_point(alpha = 0.1)+
  geom_point( mapping = aes(x = road,
                            y = profit_per_rope,
                            color = 'road'))+
  geom_point(alpha = 0.1)+
  geom_point( mapping = aes(x = sharecroppers,
                            y = profit_per_rope,
                            color = 'sharecroppers'))+
  geom_point(alpha = 0.1)+
  xlab("Number Within Community")+
  ylab("profit_per_rope")+
  labs(title=" Household Profit Per Rope by Existence of Community Feature")

