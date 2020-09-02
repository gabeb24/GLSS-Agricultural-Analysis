# ---- Data Translation Challenge ----

# This file looks at the joined data to determine potential outliers and
# filters them out of joined data set.


# ---- Outliers ----

# ---- Summary statistics prior to removing outliers
mean(hh_agri_profit$profit_per_rope)
#91650

median(hh_agri_profit$profit_per_rope)
#33707
#Clearly data is heavily right skewed.


#---- Summary statistics of different eco-zones
coastal <- subset(hh_agri_profit, eco_zone == 'coastal') #%>% 
#  select(1:22)
summary(coastal)
#of the 783 coastal households:
#avg max education = 1.9, median = 2
# the avg profit is 122642, median = 21603


forest <- subset(hh_agri_profit, eco_zone == 'forest') #%>% 
#  select(1:22)
summary(forest)
#of 2079 forest households:
#the avg profit per rope is 89012, median = 43023
#avg max education = 2.34, median = 3


savannah <- subset(hh_agri_profit, eco_zone == 'savannah') #%>% 
#select(1:22)
summary(savannah)
#of 1065 savannah households:
#avg profit per rope is 74015, median = 26545
#avg max education = 1.58, median = 2


#Removing outliers
hh_agri_profit <- hh_agri_profit %>% 
  filter(profit_per_rope < 300000) %>% 
  filter(profit_per_rope > -300000) #%>% 
#select(1:22)


summary(hh_agri_profit$profit_per_rope)

plot(hh_agri_profit$profit_per_rope, xlab = 'Individual Households')
hist(hh_agri_profit$profit_per_rope,
     breaks = 100,
     xlab = "Profit per Rope")

# Now our data is ready for additional analysis
