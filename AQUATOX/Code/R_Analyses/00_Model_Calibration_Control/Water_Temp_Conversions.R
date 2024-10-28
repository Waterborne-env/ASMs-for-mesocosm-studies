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
d.raw <- read_csv("Input/Water temperature 2013-2019 mesocosm Homberg - V1_hourly_Aquatox_Input.csv")


#1) Convert hourly values into daily averages:
df <- d.raw %>%
  dplyr::select(c(1:3)) %>%
  rename(temp_deg_C = `0.5 m water depth`) %>%
  group_by(day) %>%
  summarize(mean_temp_deg_C = round(mean(temp_deg_C), 2)) %>%
  ungroup()
  
# 2) Create a vector of dates
Date <- seq(as.Date("2013-01-01"), as.Date("2019-12-30"), by="days")
Date <- as.data.frame(Date)

# 3) Add dates column to our temperature df:
dat <- bind_cols(df, Date)


# 4) Save file
write_csv(dat, "Output/Daily_Mean_Water_Temp_2013-2019_Mesocosm_Homberg.csv")


