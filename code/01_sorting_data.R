# ---- Data Translation Challenge ----

# Upload appropriate libraries ----

library(tidyverse)
library(haven)


# Organize + Read in Data ----

#     Reads in data for WINDOWS
survey_info_a <- read_dta("raw_data/glss4_new/glss4_new/sec0a.dta")
hh <- read_dta("raw_data/glss4_new/glss4_new/sec1.dta")
education <- read_dta("raw_data/glss4_new/glss4_new/sec2a.dta")
edu_career <- read_dta("raw_data/glss4_new/glss4_new/sec2b.dta")
edu_lit <- read_dta("raw_data/glss4_new/glss4_new/sec2c.dta")

agriculture <- read_dta("raw_data/glss4_new/glss4_new/sec8a1.dta")

agg2 <- read_dta("raw_data/glss4_new/glss4_new/aggregates/agg2.dta")
subagg2 <- read_dta("raw_data/glss4_new/glss4_new/aggregates/subagg2.dta")

#   Reads in data for OS -- Brittney please check if this works
#   Uses the same variable names as those imported for WINDOWS

hh <- read_dta("raw_data/glss4_new/__MACOSX/glss4_new/._sec1.dta")

# Check Variable names
variables <- names(survey_info_1)

variables <- names(hh)

