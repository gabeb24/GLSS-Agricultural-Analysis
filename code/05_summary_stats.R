# ---- Data Translation Challenge ----

# This file contains basic summary statistics.
# Comments include statistics after the code is run.


# ---- Summary statistics prior to removing outliers ----

summary(hh_agri_profit)

#Mean profit_per_rope = 91650
#Median profit_per_rope = 33708
#Min = -646912
#Max = 7031725
#Mean max education level is 2.05

#--With the mean profit per rope 57342 greater than the median, the data is heavily right skewed
# this skew is expected for income data. Outliers will be addressed later on.--

negative_values <- hh_agri_profit %>% 
  filter(profit_per_rope < 0) %>% 
  select(nh, clust, education_max, profit_per_rope)
#We can see that there are 216 households (5%) in the dataset with negative agricultural profits.


#---- Summary statistics of different eco-zones ----

coastal <- subset(hh_agri_profit, eco_zone == 'coastal')
summary(coastal)
#of 20% of households being on coastal areas
# the avg profit is 122642, median = 21603, max = 7031725(largest in entire dataset.), min = -582228
# Average max education level for a coastal household is 1.93

#Graph of proportion of max education level in coastal households
ggplot(data = coastal) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1))


forest <- subset(hh_agri_profit, eco_zone == 'forest')
summary(forest)
#of 53% of households being in forest:
#the avg profit per rope is 89012, median = 43023, max = 3236801, min = -79766
#Average max education level for forest households is 2.34

#Distribution of max education for coastal households

#Graph of proportion of max education level in forest households
ggplot(data = forest) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1, fill = 'red')) +
  ggtitle('Forest Zone education')
  #We can see that forest households have the largest proportion that have completed secondary or above.
  

savannah <- subset(hh_agri_profit, eco_zone == 'savannah')
summary(savannah)
#of 27% of households in savannah:
#avg profit per rope is 74015, median = 26545, max = 23352761, min = -646912(lowest in entire dataset)
# 1.58

#Graph of proportion of max education level in savannah households
ggplot(data = savannah) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1))
#Largest proportion of households with no education of all groups



#----Summary statistics of urban vs rural: ----

rural <- subset(hh_agri_profit, urban_rural == 'rural')
summary(rural)
#rural households account for 83% households
#avg profit per rope of 96442, median = 35941, min = -646912, max = 7031725

#Proportion of max education levels for rural households
ggplot(data = rural) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1))

urban <- subset(hh_agri_profit, urban_rural == 'urban')
summary(urban)
#urban households account for 16% households
#avg profit per rope of 67579, median = 21134, min = -193930, max = 3236801

#Proportion of max education levels for rural households
ggplot(data = urban) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1))



#----Summary statistics of education levels ----

secondary_above <- subset(hh_agri_profit, education_max == 3)
summary(secondary_above)
#54% of households have a max education of secondary or above
#Profit per rope: mean = 91164, median = 37055, max = 4723940, min = -58228

primary <- subset(hh_agri_profit, education_max == 2)
summary(primary)
#15% of households have a max education of primary
#mean profit per rope = 107756, median = 38636, max = 7031725, min = -646912(most extreme values)

no_educ <- subset(hh_agri_profit, education_max == 0)
summary(no_educ)
#18% of households have none as max education
#mean profit per rope = 74033, median = 24738, max = 3729090, min = -193930

koranic_kinder <- subset(hh_agri_profit, education_max == 1)
summary(koranic_kinder)
#11.4% of dataset has this level of education
#mean profit per rope = 100680, median = 30178, max = 3484467, min = -58288



#----Summary Stats after outlier removal----
#Mean profit per rope = 76616
#median ppr = 33708
#Max = 1906764
#min = -42821
#We can see that removing 40 of the most extreme values reduced the difference between the median and mean
#by 14434. We have chosen to not further remove outliers to keep some for analysis purpose.

#-eco-zones
rev_coastal <- subset(rev_hh_agri_profit, eco_zone == 'coastal')
summary(rev_coastal$profit_per_rope)
# the avg profit is 70680, median = 21449, max = 1906764 (largest in entire dataset.), min = -38299


rev_forest <- subset(rev_hh_agri_profit, eco_zone == 'forest')
summary(rev_forest$profit_per_rope)
#the avg profit per rope is 80097, median = 42906, max = 1377411, min = -42821


rev_savannah <- subset(rev_hh_agri_profit, eco_zone == 'savannah')
summary(rev_savannah$profit_per_rope)
#avg profit per rope is 74102, median = 27083, max = 1630514, min = -36655



#-education levels
rev_secondary_above <- subset(rev_hh_agri_profit, education_max == 3)
summary(rev_secondary_above$profit_per_rope)
#54% of households have a max education of secondary or above
#Profit per rope: mean = 81392, median = 37099, max = 1617234, min = -42821

rev_primary <- subset(rev_hh_agri_profit, education_max == 2)
summary(rev_primary$profit_per_rope)
#16% of households have a max education of primary
#mean profit per rope = 80656, median = 38636, max = 1848194, min = -38299(most extreme values)

rev_no_educ <- subset(rev_hh_agri_profit, education_max == 0) #not affected by outlier removal!
summary(no_educ$profit_per_rope)
#29% of households have none as max education
#mean profit per rope = 85414, median = 26958, max = 3729090, min = -262471

rev_koranic_kinder <- subset(rev_hh_agri_profit, education_level == 1) #not affected by outlier removal
summary(koranic_kinder$profit_per_rope)
#1% of dataset has this level of education
#mean profit per rope = 53547, median = 25994, max = 285995, min = -5183


ggplot(data = hh_agri_profit) +
  geom_bar(mapping = aes(x = education_level, y = stat(prop), group = 1))

