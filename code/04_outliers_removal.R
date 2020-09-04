# ---- Data Translation Challenge ----

# This file looks at the joined data to determine potential outliers and
# filters them out of joined data set.
# This file also contains some basic plots


# ---- Outliers Removal----


rev_hh_agri_profit <- hh_agri_profit %>% 
  arrange(desc(profit_per_rope)) %>% 
  slice(21:n()) %>% 
  arrange(profit_per_rope) %>% 
  slice(21:n())
#remember to run regression with and without outliers!


# ---- Outlier Summary Statistics ----

# Summary w/o outliers
summary(rev_hh_agri_profit$profit_per_rope)

plot(rev_hh_agri_profit$profit_per_rope, 
     xlab = 'Individual Households')

hist(rev_hh_agri_profit$profit_per_rope,
     breaks = 100,
     xlab = "Profit per Rope")


# Summary w/ outliers
summary(hh_agri_profit$profit_per_rope)

plot(hh_agri_profit$profit_per_rope, 
     xlab = 'Individual Households')

hist(hh_agri_profit$profit_per_rope,
     breaks = 100,
     xlab = "Profit per Rope")

# Now our data is ready for additional analysis
