# ---- Data Translation Challenge ----

# Upload appropriate libraries ----

library(tidyverse)
library(haven)


# Organize + Read in Data ----

#     Reads in data for WINDOWS
survey_info_1 <- read_dta("raw_data/glss4_new/glss4_new/sec0a.dta")

hh <- read_dta("raw_data/glss4_new/glss4_new/sec1.dta")

#   Reads in data for OS
#     Uses the same variable names as those imported for WINDOWS

hh <- read_dta("raw_data/glss4_new/__MACOSX/glss4_new/._sec1.dta")

variables <- names(survey_info_1)

variables <- names(hh)

