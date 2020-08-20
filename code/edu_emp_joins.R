
library(tidyverse)
library(haven)

education_exp <- read_dta('raw_data/glss4_new/aggregates/exp1.dta')

education <- read_dta("raw_data/glss4_new//sec2a.dta")
edu_career <- read_dta("raw_data/glss4_new/sec2b.dta")
edu_lit <- read_dta("raw_data/glss4_new/sec2c.dta")


edu_lit <- edu_lit[-c(10,12:16,18)]

edu_exp_detail <- left_join(education, education_exp)

edu_exp_2 <- edu_exp_detail[-c(10:18,19,20)]

edu_exp_lit <- left_join(edu_exp_2, edu_lit)


edu_career <- edu_career[-c(8,10:11)]



edu_exp_lit <- left_join(edu_exp_lit, edu_career)





















employment_1 <- read_dta("raw_data/glss4_new/sec4a.dta")
#If we want to look at the education level of individuals who work in agriculture we should join sec4a with education table
#lookS at employment and list of occupations in the last 12 months.
  
#s4AQ1 - s4aq6



employment_2 <- read_dta("raw_data/glss4_new/sec4b.dta")
#---- SEC4B / C / D / E / F: Employment and Time Use ----
 # (Characteristics of Main occupation, 2nd occupation, 3rd occupation, 4th occupation)

#S4BQ8 / / S4DQ11 / S4EQ11 - Employment status (agric)
#S4BQ9 / S4CQ12 / S4DQ12 / S4EQ12 - Work for whom / sS4BQ31: entitled to free medical care



employment_3 <- read_dta("raw_data/glss4_new/sec4c.dta")
#----SEC4C: secondary occupation during past year ----

#S4CQ1: employment type / S4CQ3: same work / S4CQ9: paid or unpaid S4CQ11a: annual hours worked


employment_5 <- read_dta("raw_data/glss4_new/sec4f.dta")
#----SEC4F: employment search in last 12 months ----

employment_history <- read_dta("raw_data/glss4_new/sec4h.dta")

