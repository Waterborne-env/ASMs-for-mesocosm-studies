#===============================================================================
#     Copyright 2021, Integral Consulting Inc. All rights reserved.     #
#===============================================================================
# All code is the property of Integral Consulting Inc. 
# Only its application is granted under licensed use to the project listed below.
#===============================================================================
# C3143_Weather_Data_Convsersions.R
#===============================================================================
# PURPOSE:
#    Convert any weather variables from source files into units required for input to Aquatox
#
# NOTES:
#   1) Resource for unit conversions: https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/null/?cid=stelprdb1043619
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
#	02/09/2021  L.Mudge creates script
# 02/23/2021  L.Mudge updates to reformat DateTime for Aqautox and remove NA values from output
#==========================================================================

############## SETUP ##################
library(tidyverse)
library(lubridate)
library(openxlsx)

#Conversion: 1 Langley/day = 0.484583 Watt/m2- from USDA source in notes; use reverse because we are going from Watt/,2 to Langley
watt_to_lang <-  1/0.484583

############## INPUT DATA ##############
d.weather <- read_csv("Input/Weather-2013-2019_MesocosmGmbH_Station35km_RInput.csv")
names(d.weather) <- c("Date", "Time","Temp_C", "AirPressure_hPa", "RelHumidity", "Windspeed_ms", "Global_Radiation_W_per_m2", "Light_Langley_per_day")

d.light <- d.weather %>%
  dplyr::select(Date, Time, Global_Radiation_W_per_m2, Light_Langley_per_day)

############# DATA WRANGLING ##############
### Convert light variable: global radiation w/m2 --> Langley per day

#1) Fill in column for radiation in units of Langleys per day:
d.converted <- d.light %>%
  mutate(Light_Langley_per_day = Global_Radiation_W_per_m2* watt_to_lang,
         DateTime = as.POSIXct(as.character(paste(Date, Time)), format="%m/%d/%Y %H:%M:%S"),
         Time = format(DateTime, "%I:%M %p", usetz = F),
         Year= lubridate::year(DateTime)) %>%
  dplyr::select(Year, Date, Time, Light_Langley_per_day)

  ## Manually fix NAs for the time stamp; fixing that here. Format DateTime for Aquatox, remove rows with missing light data:
df <- d.converted %>%
  mutate(Time = ifelse(is.na(Time), paste("12:00 AM"), Time),
         Year = ifelse(is.na(Year), str_sub(Date, start=-4), Year),
         DateTime = paste(Date, Time, sep = " ")) %>%
  dplyr::select(-Date, -Time) %>%
  relocate(DateTime, .after=Year) %>%
  filter(!is.na(Light_Langley_per_day))


#2) Save df, but data for one year per sheet in Excel:
  #Smartsheet will only allow uploads with <20,000 rows so we are uploading all data as annual sheets
  # sample code: https://martinctc.github.io/blog/vignette-write-and-read-multiple-excel-files-with-purrr/

  # Creates a list of dataframes
df.list <- split(df, as.factor(df$Year)) 
  
  # Name the Dataframes, so they will appear on the sheets when written
names(df.list) <- df.list %>%
  purrr::map(~pull(.,Year)) %>% # Pull out year variable
  purrr::map(~as.character(.)) %>% # Convert numeric to character
  purrr::map(~unique(.)) #set each year as a name

  # Write the excel file:
df.list %>%
  openxlsx::write.xlsx("Output/Light_2013-2019_LangleyPerDay.xlsx")

# save copy of full df
    # 3/3/21-- format changes if save as .csv (even if do manually), so save as XLSX for now
#write_csv(df, "Output/Light_2013-2019_LangleyPerDay.csv") 
#write.csv(df, "Output/Light_2013-2019_LangleyPerDay.csv")
openxlsx::write.xlsx(df, "Output/Light_2013-2019_LangleyPerDay_all.xlsx")

## Get list of dates that have missing data for light variable:

df.na <- d.converted %>%
  filter(is.na(Light_Langley_per_day)) %>%
  distinct(Date)
write_csv(df.na, "Output/Missing_light_data.csv")
