#----Tidying Literacy to HH level to include in analysis----

#loading relvant libraries
library(tidyverse)
library(haven)
library(dplyr)

#Loading Literacy dataset
literacy <- read_dta("raw_data/sec2c.dta")


#Selecting relevent literacy columns
literacy <- literacy %>% 
  select(1:9) %>% 
  
#Assigning levels to reading, writing and calculation variables to ease conversion to household level
  
  mutate(
    read_level = case_when(
      #assigns 3 if they can read english and ghanian
      #2 if theyonly read english
      #1 if they only read ghanian
      #0 if they don't read either
      s2cq1 == 1 & s2cq2 != 1 ~ 3,
      s2cq1 == 1 & s2cq2 == 1 ~ 2,
      s2cq1 == 2 & s2cq2 != 1 ~ 1,
      s2cq1 == 2 & s2cq2 == 1 ~ 0
    )
  ) %>% 
  
  mutate(
    write_level = case_when(
      #assigns 3 if they can write english and ghanian
      #2 if theyonly write english
      #1 if they only write ghanian
      #0 if they don't write either
      s2cq3 == 1 & s2cq4 != 1 ~ 3,
      s2cq3 == 1 & s2cq4 == 1 ~ 2,
      s2cq3 == 2 & s2cq4 != 1 ~ 1,
      s2cq3 == 2 & s2cq4 == 1 ~ 0,
    )
  ) %>% 
  
  mutate(
    calculation = case_when(
      s2cq5 == 1 ~ 1,
      s2cq5 == 2 ~ 0,
    )
  )

#Grouping all 3 new columns by the household member with the highest level of each

#unfortunately wasn't able to do this all by piping once
hh_read <- group_by(literacy, clust, nh) %>% 
  summarize(read_max = max(read_level))

hh_write <- group_by(literacy, clust, nh) %>% 
  summarize(write_max = max(write_level))
  
hh_calc <- group_by(literacy, clust, nh) %>% 
  summarize(calc_max = max(calculation))

#Ignore this dataframe
lit_levels <- inner_join(hh_read, hh_write) %>% 
  
#This is the actual dataframe that has all 3 new variables present.

lit_levels <- inner_join(lit_levels, hh_calc)

