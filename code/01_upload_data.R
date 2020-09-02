# ---- Data Translation Challenge ----

# This file uploads the relevant libraries and data sets used in the following 
# files for tidying and analysis. 


# ---- Upload appropriate libraries ----

library(tidyverse)  # visualize data
library(haven)      # load data


# ---- Loading relevant data ----

#     Reads in data for WINDOWS

survey_info_a   <- read_dta("raw_data/sec0a.dta")   # Survey info + HH location
agri_plot_s8b   <- read_dta("raw_data/sec8b.dta")   # Agriculture - Plot Details
education       <- read_dta("raw_data/sec2a.dta")   # Education - General survey Qs
literacy        <- read_dta("raw_data/sec2c.dta")   # Education - Literacy / Apprenticeship 
agg2            <- read_dta("raw_data/aggregates/agg2.dta") # Agricultural income & farm depreciation

community_econ      <- read_dta("raw_data/community/cs2.dta")  # Economy and Infrastructure
community_edu       <- read_dta("raw_data/community/cs3.dta")  # Education
community_agg       <- read_dta("raw_data/community/cs5b.dta") # Agriculture

#     Reads in data for OS/MAC (local folder)

survey_info_a   <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec0a.dta")
agri_plot_s8b   <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec8b.dta") 
education       <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2a.dta")
literacy        <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/sec2c.dta")
agg2            <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/aggregates/agg2.dta")

community_econ      <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/community/cs2.dta") 
community_edu       <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/community/cs3.dta")  
community_agg       <- read_dta("~/Downloads/GIT Folder Seattle U/glss4_new 2/community/cs5b.dta") 


# After uploading, we are ready to sort the variables we will be analyzing.
