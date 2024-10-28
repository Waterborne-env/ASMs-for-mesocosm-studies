#===============================================================================
#     Copyright 2021, Integral Consulting Inc. All rights reserved.     #
#===============================================================================
# All code is the property of Integral Consulting Inc. 
# Only its application is granted under licensed use to the project listed below.
#===============================================================================
# C3143_Water_Temp_Conversions.R
#===============================================================================
# PURPOSE:
#    Convert water temperature from source files into units required for input to Aquatox
#
# NOTES:
#   1) Weather data has 2556 unique dates, water temp has 2555. One day is missing from the water temp data, but we don't know which one -- assuming for now the time series stops at 12/30/2019
#
# PROJECT INFORMATION:
#   Name: Ecosystem Modeling of Mesocosm
#   Number: C3143 
#
#
# AUTHOR(S):
#	  L.Mudge
#
# HISTORY
#	 Date		 Remarks
#----------------------------------------------------------------------
#	02/18/2021  L.Mudge creates script
#==========================================================================

############## SETUP ##################
#install.packages(c("tidyverse", "lubridate", "openxlsx"))
library(tidyverse)
library(lubridate)
library(openxlsx)

############## DATA WRANGLING ##################
d.raw <- read_csv("//pfs1w/C2000-C3999/C3143_Mesocosm_Waterborne/Data/Validation_to_control_2021/R Inputs/Daily_Mean_Water_Temp_2021_Mesocosm_Homberg.csv")


#1) Convert hourly values into daily averages:
df <- d.raw %>%
  select(-Date) %>%
  rename(mean_temp_deg_C = mean_depth) 

  
# 2) Create a vector of dates
Date <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days")
Date <- as.data.frame(Date)

# 3) Add dates column to our temperature df:
dat <- bind_cols(df, Date)


#read in 2013-2019 data
d.prev <- read.csv("//pfs1w/C2000-C3999/C3143_Mesocosm_Waterborne/Working_Files/R_Analyses/01_Model_Validation/Output/Daily_Mean_Water_Temp_2013-2019_Mesocosm_Homberg.csv")  %>%
  select(-day)

d.all <- rbind(dat,d.prev) 

d.all <- d.all %>%
  mutate(day = paste(1:nrow(.)))

# 4) Save file
write_csv(d.all, "2021_Validation/Output/Daily_Mean_Water_Temp_2013-2021_Mesocosm_Homberg.csv")



###### save for later:
# as written, doesn't do exactly what I want, but the complete function is a great way to add rows if you are missing dates
# dd <- df %>%
#   dplyr::select(day) %>%
#   tibble::add_column(Date = NA) %>% # initialize an empty column
#   mutate(Date = as.Date(Date)) %>%
#   complete(Date = seq.Date(from = as.Date("2013-01-01"), length.out= 2555, by = "day"))