
#Sorting Expenditure Variables in aggregates

#Expenses: Identification Variables:
  #nh
  #pid
  #clust

library(tidyverse)
library(haven)


educaion_exp <- read_dta('raw_data/glss4_new2/aggregates/exp1.dta')
  #individual level
  #nh
  #pid
  #clust
  #22211 obs
hh_bills <- read_dta('raw_data/glss4_new2/aggregates/exp2.dta')
  #hh level
  #nh
  #clust
  #5998 obs
farm_rent <- read_dta('raw_data/glss4_new2/aggregates/exp3.dta')
  #individual farm level
  #farm identifier farmcd
  #10570 obs
  
crop_exp <- read_dta('raw_data/glss4_new2/aggregates/exp4.dta')
#level of each crop unput
#crpexpcd: crop identifier

livestock_exp <- read_dta('raw_data/glss4_new2/aggregates/exp5.dta')
#level of each livestock input
#crpexpcd: livestock identifier

processing_cost <- read_dta('raw_data/glss4_new2/aggregates/exp6.dta')
#level of individual product
#proagrcd: individual product
  
hp_consumption <- read_dta('raw_data/glss4_new2/aggregates/exp7.dta')
#homagrcd: type of prodcut consumed?
#The consumption seems to be measured in months 0 - 12

farm_equip_dep <- read_dta('raw_data/glss4_new2/aggregates/exp12.dta')
#at level of individual item
#eqcdown: farm equipment item type

remittance <- read_dta('raw_data/glss4_new2/aggregates/exp14.dta')
#at level of individual remittance
#trexpcd: is NA for all observations
  
useval <- read_dta('raw_data/glss4_new2/aggregates/exp16.dta')
#at level of of each type of asset or good
#hassetcd: type of assset or good
  
rent <- read_dta('raw_data/glss4_new2/aggregates/exp17.dta')
#hh level
#1193 obs

est_rent_owner <- read_dta('raw_data/glss4_new2/aggregates/exp18.dta')
#hh level
#2511 obs

est_rent_parents <- read_dta('raw_data/glss4_new2/aggregates/exp19.dta')
#hh level
#2180 obs

                      #Subaggregates 1-20 (household level)

schol_val <- read_dta('raw_data/glss4_new2/aggregates/subagg1.dta')
#3674 obs

non_monetary_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg6.dta')
#6000 obs

self_emp_farm <- read_dta('raw_data/glss4_new2/aggregates/subagg7.dta')
#5998 obs

land_rent_sh_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg10.dta')
#4109 obs

rent_livestock_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg11.dta')
#111 obs

rent_equip_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg12.dta')
#1329 obs

cash_crop_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg13.dta')
#3729 obs

other_crop_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg14.dta')
#2523

other_agg_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg15.dta')
#4137

proccessed_crop_inc <- read_dta('raw_data/glss4_new2/aggregates/subagg16.dta')
#3671 obs
