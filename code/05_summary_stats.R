# ---- Data Translation Challenge ----

# This file contains basic summary statistics.


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

