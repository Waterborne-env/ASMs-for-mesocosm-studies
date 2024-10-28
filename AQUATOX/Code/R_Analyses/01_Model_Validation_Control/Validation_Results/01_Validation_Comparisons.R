# COPYRIGHT  --------------------------------------------------------------
# Copyright 2022, Integral Consulting Inc. All rights reserved. 


# PROJECT INFORMATION -----------------------------------------------------
# Name: Waterborne Mesocosm
# Number: C3143

# HISTORY -----------------------------------------------------------------
# Date        Author      Remarks
# 02-22-2022  L.Mudge     Creates Script
# 02-28-2022  L.Mudge     Commented out data cleaning lines for aq.results, as updated Excel files only have 1 observation per day, there is no need to filter by time.
# 03-02-2022  L.Mudge     Update plots to show modeled data from May 10-Sept 30 (but grading still restricted to experimental time frames only)
# 03-07-2022  L.Mudge     Add manual legend to plot, update formatted table column header names, incl. updated input files with Hirudinea record.
# 03-08-2022  L.Mudge     Add calculation of % criteria met, excluding the peaks; modify directory and outpath to account for more than 1 run per day
# 03-29-2022  L.Mudge     Revises scoring to combine scoring for Damselfly and Dragonfly into 1 grade for Odonata group- per request from Waterborne.
# 04-06-2022  A. Lindborg Revises input files to use new "noquantile" files from 11_Setup.. script - this was a request from waterborne to only calcuate min and max, not 5th and 95th percentile
# PURPOSE -----------------------------------------------------------------

# Compare Aquatox model results and control data to the calibration criteria -- all converted to biomass. 

# INPUTS = 
    # Aquatox results (folder path defined by user)
    # Calibration criteria, mesocosm control data - all loaded from this folder

# OUTPUTS = 
    # Integral comparison table (% of data points falling within mix/max criteria, by species and study)
    # Waterborne study comparison table (study specific calibration criteria met)
    # Waterborne summary table (Y/N)
    # Plots of model results, control data, and calibration criteria - by species and study




# USER INPUTS -------------------------------------------------------------

### Filepath to Aquatox model results:
#results_folder <- "AQUATOX_InitialCalibration_InProgress_IndYears_20220208"
#results_folder <- "AQUATOX_InitialCalibration_6yrMayStartSUM - Hirudinea"
results_folder <- "AQUATOX_006_ASM_RingStudy_Mesocosm_Model_ValidationDateRng"

# Number to append to end of folder (add +1 each time you run the script)
n <- 1

# SETUP -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)


# Plot theme:
theme <- theme(text = element_text(family = "sans")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.text = element_text(color="black", size=8),
        legend.background = element_rect( size=.5, linetype= "solid", color = "black")) +
  theme(panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size= 1)) +
  theme(axis.text.x = element_text(color="black", size=8, vjust=0.5, angle=0)) +
  theme(axis.text.y = element_text(color="black", size=8, vjust=0.5, angle=0)) +
  theme(axis.title.x = element_text(color="black", size=9, face="bold")) +
  theme(axis.title.y = element_text(color="black", size=9, face="bold")) +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="bold")) +
  theme(axis.ticks = element_line(size = 1)) +
  theme(axis.line = element_line(size = 1)) +
  theme(strip.background =element_rect(fill="white")) +
  theme(panel.border = element_rect(fill = NA, size = 0.25)) +
  theme(strip.text = element_text(face="bold", size = 9)) +
  theme(plot.margin = unit(c(0.5,0.5,.5,0.5), "cm"))

options(scipen = 9999) # removes scientific notation

# Set working directory:
setwd("01_Aquatox_Results_Validation")

# LOAD CALIBRATION FILES --------------------------------------------------------------------

crit <- read_csv("R_Analyses/01_Model_Validation/01_Aquatox_Results_Validation/Input/calibration_criteria.csv")
crit.study <- read_csv("Input/calibration_criteria_by_study.csv")
crit.study.long <- read_csv("Input/calibration_criteria_by_study_long.csv")
exp.dates <- read_csv("Input/experiment_dates.csv") %>%
  mutate(min_date = as.Date(min_date, format = "%m/%d/%Y"),
         max_date = as.Date(max_date, format = "%m/%d/%Y"))
d.meso <- read_csv("Input/mesocosm_control_data.csv")


# LOAD AND FORMAT AQUATOX RESULTS ----------------------------------------------------

### Read in Aquatox model results:
files <- list.files(path = results_folder,
                    pattern = "*.xls",
                    full.names = TRUE)

set_col_types <- c("date", "date", rep("numeric", times=49))

### Format AQ model results
aq.results <- tibble(filename = files) %>%
  mutate(contents = map(filename, ~readxl::read_excel(.x, col_types = set_col_types))) %>%
  unnest(cols=contents) %>%
  #mutate(time = lubridate::hour(`...2`)) %>%
  #filter(time ==12) %>% ## ONLY KEEP OBSERVATIONS FROM NOON
  dplyr::select(-c(`...2`, filename)) %>%
  rename(Date = `Date:`) %>%
  pivot_longer(cols= -c(Date),
               names_to = "aq_record",
               values_to = "model_result") %>%
  mutate(year = lubridate::year(Date)) %>%
  mutate(date.obj = as.Date(Date)) %>%
  filter(aq_record %in% unique(crit$aq_record)) %>%
  #filter(date.obj >= exp.dates$min_date[match(year, exp.dates$year)] & date.obj <= exp.dates$max_date[match(year, exp.dates$year)]) %>%
  arrange(aq_record)

### Calculate total periphyton and total phytoplankton from AQ results:
aq.pp <- aq.results %>%
  filter(grepl("Peri", aq_record ) | grepl("Phyto", aq_record )) %>%
  mutate(aq_record = ifelse(grepl("Peri", aq_record), "Total Periphyton", "Total Phytoplankton")) %>%
  group_by(aq_record, date.obj, year, Date) %>%
  summarize(model_result = sum(model_result))

## Calculate total for Odonata from AQ results: Added 3/29- LM
aq.od <- aq.results %>%
  filter(aq_record %in% c("Damselfly_Meso (g/m2 dry)", "Dragonfly_Meso (g/m2 dry)")) %>%
  mutate(aq_record = "Total Odonata") %>%
  group_by(aq_record, date.obj, year, Date) %>%
  summarize(model_result = sum(model_result))

### Dataframe with all model results (should be May 10-Sept 30)- use for plotting
aq.results.all <- bind_rows(aq.results, aq.pp, aq.od)

#write for importing in model validation comparison plots
#write_csv(aq.results.all, "Output/aq_results_all.csv")

### Dataframe with model results ONLY in the experimental windows -- use for grading
aq.results.exp <- aq.results.all %>%
  filter(date.obj >= exp.dates$min_date[match(year, exp.dates$year)] & date.obj <= exp.dates$max_date[match(year, exp.dates$year)])
  
# remove intermediary tables
rm(aq.pp)  
rm(aq.od)
rm(aq.results)


# CREATE OUTPUT FOLDER ----------------------------------------------------

run_no <- sub("\\_.*", "", x= basename(files[1])) # Extract model run number
#dir.exists(paste("Output/", run_no, "_Calibration_Results_", Sys.Date(), "_", n, sep=""))
dir.create(paste("Output/", run_no, "_Calibration_Results_", Sys.Date(), "_", n, sep="")) # create the directory
out.path <- paste("Output/", run_no, "_Calibration_Results_", Sys.Date(), "_", n, sep="") # save the path for writing output


# INTEGRAL COMPARISON -----------------------------------------------------


int.grade <-  aq.results.exp %>% ## USE RESULTS RESTRICTED TO EXPERIMENTAL TIME FRAME
  left_join(., crit.study) %>%
  mutate(meets_criteria = ifelse(model_result>= crit_biomass_min & model_result<=crit_biomass_max, TRUE, FALSE)) %>%
  group_by(aq_record) %>%
  summarize(n_tot = n(),
            n_meet_crit = sum(meets_criteria),
            per_meet_crit = round((n_meet_crit/n_tot)*100,2)) %>%
  arrange(desc(n_meet_crit))

full.grade <- int.grade

for(i in c(2014, 2016, 2018, 2019)){
  
  year.crit <- crit.study.long %>% filter(year == i) %>% left_join(., crit)
  
  d <- aq.results.exp %>% 
    filter(year == i) %>%
    left_join(., year.crit) %>%
    mutate(meets_criteria = ifelse(model_result>= crit_biomass_min & model_result<=crit_biomass_max, TRUE, FALSE),
           meets_study = ifelse(model_result>= min5 & model_result<= max95, TRUE, FALSE)) %>%
    group_by(aq_record) %>%
    summarize(n_tot = n(),
              n_meet_crit = sum(meets_criteria),
              per_meet_crit = round((n_meet_crit/n_tot)*100,2),
              n_meet_study = sum(meets_study),
              per_meet_study = round((n_meet_study/n_tot)*100, 2)) %>%
    arrange(desc(n_meet_crit)) %>%
    dplyr::select(-n_tot, -n_meet_crit, -n_meet_study)
  
  names(d) <- c("aq_record", paste0("per_meet_crit_", i), paste("per_meet_study_", i))
  
  full.grade <- left_join(full.grade, d)
}

names(full.grade) <- c("Aquatox Record", 
                       "Total Number of Modeled Points (all years combined)", 
                       "No. Modeled Points that Meet Criteria", 
                       "% Points Meet Calibration Criteria", 
                       "2014: % Criteria met", 
                       "2014: % Within Study Specific Ranges (5-95 percentiles)",
                       "2016: % Criteria met", 
                       "2016: % Within Study Specific Ranges (5-95 percentiles)",
                       "2018: % Criteria met", 
                       "2018: % Within Study Specific Ranges (5-95 percentiles)",
                       "2019: % Criteria met", 
                       "2019: % Within Study Specific Ranges (5-95 percentiles)")

### Save Workbook:
wb <- createWorkbook()
header_style <- createStyle(wrapText = TRUE)

addWorksheet(wb, sheetName= "Integral Calibration Check")
writeData(wb, sheet=1, x=full.grade , colNames = TRUE)
setColWidths(wb, sheet = 1, widths = 16, cols= 1:ncol(full.grade))
addStyle(wb, sheet=1, header_style, cols=1:ncol(full.grade), rows=1)


saveWorkbook(wb, "Output/006_Integral_Calibration_Table.xlsx", overwrite = FALSE)


# WATERBORNE COMPARISONS --------------------------------------------------------------

### Create Table 1- Individual study comparisons
wb.grade <-  aq.results.exp %>%
  left_join(., crit) %>%
  mutate(across(.cols = c(model_result, crit_biomass_min_peak, crit_biomass_max_peak, crit_biomass_min, crit_biomass_max), ~signif(.x, digits=2))) %>%
  group_by(aq_record, year) %>%
  mutate(peak_value = ifelse(!is.na(peak_date_start), max(model_result), NA_real_),
         peak_date = if_else(!is.na(peak_date_start), min(date.obj[model_result == peak_value]), NA_real_), #Take first peak date that occurs
         max_value = ifelse(is.na(peak_date_start), max(model_result), # if there is no peak date, then just return the absolute max
                            max(model_result[date.obj<peak_date_start | date.obj>peak_date_end])), # if there is a peak date, return the max for the observations outside of the peak window
         min_value = min(model_result),
         mean_value = mean(model_result)
  ) %>%
  ungroup() %>%
  distinct(aq_record, consensus_group, year, .keep_all = TRUE) %>%
  mutate(start_monthday = as.numeric(format(peak_date_start, "%m%d")),
         end_monthday = as.numeric(format(peak_date_end, "%m%d")),
         peak_monthday = as.numeric(format(peak_date, "%m%d")))  %>%
  dplyr::select(-c(Date, model_result, date.obj, peak_date_start, peak_date_end, peak_date, units_for_aquatox)) %>%
  relocate(parameter_group, consensus_group, .before = aq_record) %>%
  relocate(start_monthday, end_monthday, crit_biomass_min_peak, crit_biomass_max_peak, crit_biomass_min, crit_biomass_max, peak_monthday, 
           peak_value, min_value, max_value, .before=mean_value) %>%
  mutate(meet_peak = ifelse(is.na(peak_monthday), NA, 
                            ifelse(peak_monthday>=start_monthday & peak_monthday<=end_monthday & peak_value>=crit_biomass_min_peak & peak_value<= crit_biomass_max_peak, 1, 0)),
         meet_min = ifelse(min_value>=crit_biomass_min, 1, 0),
         meet_max = ifelse(max_value<=crit_biomass_max, 1, 0)) %>%
  group_by(aq_record) %>%
  mutate(peak_fulfilled = ifelse(sum(meet_peak) ==4, TRUE, FALSE),
         min_fulfilled = ifelse(sum(meet_min)==4, TRUE, FALSE),
         max_fulfilled = ifelse(sum(meet_max)== 4, TRUE, FALSE)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, 
              values_from = c("peak_monthday","peak_value" ,"min_value", "max_value","mean_value", "meet_peak", "meet_min", "meet_max")) %>%
  relocate(peak_value_2014, min_value_2014, max_value_2014, mean_value_2014,
           peak_monthday_2016, peak_value_2016, min_value_2016, max_value_2016, mean_value_2016,
           peak_monthday_2018, peak_value_2018, min_value_2018, max_value_2018, mean_value_2018,
           peak_monthday_2019, peak_value_2019, min_value_2019, max_value_2019, mean_value_2019, .after = peak_monthday_2014)

### Save Waterborne Table #1:
wb <- createWorkbook()
n <- 0
pos_style <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
for(i in unique(wb.grade$parameter_group)) {
  d <- wb.grade %>%
    filter(parameter_group == i) %>%
    dplyr::select(consensus_group:crit_biomass_max, peak_monthday_2014:mean_value_2019) %>%
    arrange(consensus_group) %>%
    rename(`Consensus Group` = consensus_group,
           `Aquatox Record` = aq_record,
           `Earliest Peak Date (MDD)` = start_monthday,
           `Latest Peak Date (MDD)` = end_monthday,
           `Minimum Peak Biomass` = crit_biomass_min_peak,
           `Maximum Peak Biomass` = crit_biomass_max_peak,
           `Minimum Biomass` = crit_biomass_min,
           `Maximum Biomass` = crit_biomass_max,
           
           `Peak Date 2014` = peak_monthday_2014,
           `Peak Biomass 2014`= peak_value_2014,
           `Minimum Biomass 2014` = min_value_2014,
           `Maximum Biomass 2014` = max_value_2014,
           `Mean Biomass 2014` = mean_value_2014,
           
           `Peak Date 2016` = peak_monthday_2016,
           `Peak Biomass 2016`= peak_value_2016,
           `Minimum Biomass 2016` = min_value_2016,
           `Maximum Biomass 2016` = max_value_2016,
           `Mean Biomass 2016` = mean_value_2016,
           
           `Peak Date 2018` = peak_monthday_2018,
           `Peak Biomass 2018`= peak_value_2018,
           `Minimum Biomass 2018` = min_value_2018,
           `Maximum Biomass 2018` = max_value_2018,
           `Mean Biomass 2018` = mean_value_2018,
           
           `Peak Date 2019` = peak_monthday_2019,
           `Peak Biomass 2019`= peak_value_2019,
           `Minimum Biomass 2019` = min_value_2019,
           `Maximum Biomass 2019` = max_value_2019,
           `Mean Biomass 2019` = mean_value_2019,
           ) %>%
    dplyr::select(where(~!all(is.na(.))))
    
  
  n_rows <- nrow(d)
  n_cols <- ncol(d) # peri & phyto will be 17, macro & zoo will be 29 bc have peak info
  
  addWorksheet(wb, sheetName= i)
  writeData(wb, sheet=i, x=d , colNames = TRUE)
  ## Add conditional formatting to color cells
}

saveWorkbook(wb, "Output/_Waterborne_Validation_Table1.xlsx", overwrite = TRUE)



### Create the summary "Y/N" Criteria fulfilled table
wb.grade.yn <- wb.grade %>%
  dplyr::select(parameter_group:crit_biomass_max, peak_fulfilled, min_fulfilled, max_fulfilled)

wb.grade.perc <- wb.grade.yn %>%
  pivot_longer(cols = c(peak_fulfilled, min_fulfilled, max_fulfilled), names_to = "criteria_type", values_to = "criteria_result") %>%
  filter(!is.na(criteria_result)) %>%
  filter(!aq_record %in% c("Damselfly_Meso (g/m2 dry)", "Dragonfly_Meso (g/m2 dry)")) %>% ##NOT INCLUDING INDIVIDUAL ODONATES IN THE SCORING - lM 3/29/22
  summarize(total_crit = n(),
            total_crit_met = sum(criteria_result),
            percent_met = total_crit_met/total_crit * 100) %>%
  dplyr::select(percent_met) %>%
  rename(`Percent Criteria Met (all studies/groups)` = percent_met)

  # Same grade as above, but exclude peak criteria & individual odonate records
wb.grade.perc.nopeak <- wb.grade.yn %>%
  pivot_longer(cols = c(peak_fulfilled, min_fulfilled, max_fulfilled), names_to = "criteria_type", values_to = "criteria_result") %>%
  filter(!is.na(criteria_result), criteria_type != "peak_fulfilled" ) %>%
  filter(!aq_record %in% c("Damselfly_Meso (g/m2 dry)", "Dragonfly_Meso (g/m2 dry)")) %>% ##NOT INCLUDING INDIVIDUAL ODONATES IN THE SCORING - lM 3/29/22
  summarize(total_crit = n(),
            total_crit_met = sum(criteria_result),
            percent_met = total_crit_met/total_crit * 100) %>%
  dplyr::select(percent_met) %>%
  rename(`Percent Criteria Met (EXCLUDE PEAKS)` = percent_met)

wb <- createWorkbook()

neg_style <- createStyle(bgFill = "#FF0000") # red background
pos_style <- createStyle(bgFill = "#CCFFCC") # green background

for(i in unique(wb.grade.yn$parameter_group)) {
  
  d <- wb.grade.yn %>%
    filter(parameter_group == i) %>%
    arrange(consensus_group) %>%
    mutate(peak_fulfilled = ifelse(peak_fulfilled==TRUE, "Y", "N"),
           min_fulfilled = ifelse(min_fulfilled==TRUE, "Y", "N"),
           max_fulfilled = ifelse(max_fulfilled ==TRUE, "Y", "N")) %>% 
    rename(`Species Group` = consensus_group,
           `AQUATOX record` = aq_record,
           `Earliest peak date` = start_monthday,
           `Latest peak date` = end_monthday,
           `Minimum Peak Biomass` = crit_biomass_min_peak,
           `Maximum Peak Biomass` = crit_biomass_max_peak,
           `Minimum Biomass` = crit_biomass_min,
           `Maximum Biomass` = crit_biomass_max,
           `Peak Date and Biomass` = peak_fulfilled,
           `Minimum Biomass Fulfilled` = min_fulfilled,
           `Maximum Biomass Fulfilled` = max_fulfilled) %>%
    dplyr::select(where(~!all(is.na(.)))) %>%
    dplyr::select(-parameter_group)
  
  n_rows <- nrow(d) +1 #add 1 so when writes out, will include the last row
  n_cols <- ncol(d) # peri & phyto will be 17, macro & zoo will be 29 bc have peak info
  
  addWorksheet(wb, sheetName= i)
  writeData(wb, sheet=i, x=d , colNames = TRUE)
  setColWidths(wb, sheet = i, cols = 1:n_cols, widths = "auto")
  conditionalFormatting(wb, sheet = i,
                        cols = 1:n_cols,
                        rows = 1:n_rows, 
                        rule = '="Y"', 
                        style = pos_style)
  conditionalFormatting(wb, sheet = i,
                        cols = 1:n_cols,
                        rows = 1:n_rows, rule = '="N"', 
                        style = neg_style)
}
writeData(wb, sheet = "Macroinvertebrates", x = wb.grade.perc, startCol= 13)
writeData(wb, sheet = "Macroinvertebrates", x = wb.grade.perc.nopeak, startCol= 14)

conditionalFormatting(wb, sheet = "Macroinvertebrates",
                      cols = c(13:14),
                      rows = 2,
                      rule = ">=90",
                      style = pos_style)
conditionalFormatting(wb, sheet = "Macroinvertebrates",
                      cols = c(13:14),
                      rows = 2,
                      rule = "<90",
                      style = neg_style)
saveWorkbook(wb, "Output/006_Waterborne_Validation_TableYN.xlsx", overwrite = TRUE)


# PLOTS -------------------------------------------------------------------

### Adjust timescale for plotting
aq.results.all$date2 <- aq.results.all$date.obj
year(aq.results.all$date2) <- 2021

d.meso$date2 <- d.meso$date
year(d.meso$date2) <- 2021

exp.dates.p <- exp.dates
year(exp.dates.p$min_date) <- 2021
year(exp.dates.p$max_date) <-2021

# Set min and max dates for X-axis, as requested by B.Sackmann:
min_date <- as.Date("2021-05-10")
max_date <- as.Date("2021-09-30")

### Print plots
  # Manual legends: https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
pdf(paste0(out.path, "/", run_no,  "_Calibration_Result_Plots.pdf", sep=""), paper="USr", width=10, height=7.25)
par(omi=c(1.5,0.5,0,0.5), pin=c(7,8),pty='m')

for(i in unique(d.meso$parameter_group)){
  pd <- d.meso %>% filter(parameter_group == i) %>% arrange(consensus_group, aq_record)
  
  for(j in unique(pd$aq_record)){

  md <- pd %>% filter(aq_record == j)
  cd <- aq.results.all %>% filter(aq_record == j)
  units <- regmatches(j, gregexpr("(?=\\().*?(?<=\\))", j, perl=T))[[1]]
  
  p <- ggplot() +
    geom_point(data = md, aes(x=date2, y = biomass, color = "green4")) +
    geom_path(data=cd, aes(x=date2,  y=model_result, color= "black"), size=1) +
    geom_vline(data = exp.dates.p, aes(xintercept = min_date, linetype= "Experiment Dates"),color= "blue") +
    geom_vline(data = exp.dates.p, aes(xintercept = max_date, linetype= "Experiment Dates"),color= "blue") +
    facet_wrap(~year) +
    geom_hline(data= data.frame(yint=crit$crit_biomass_min[crit$aq_record == j]), aes(yintercept=yint, linetype="Min/Max Criteria"), color = "darkgray") +
    geom_hline(data= data.frame(yint=crit$crit_biomass_max[crit$aq_record == j]), aes(yintercept=yint, linetype="Min/Max Criteria"), color = "darkgray") +
    geom_vline(data = data.frame(xint = crit$peak_date_start[crit$aq_record == j]), aes(xintercept=xint, linetype = "Peak Date Windows"), color="orange") +
    geom_vline(data = data.frame(xint = crit$peak_date_end[crit$aq_record == j]), aes(xintercept=xint, linetype = "Peak Date Windows"), color="orange") +
    labs(x= "", y= paste0("Biomass ", units), title = paste0("Parameter Group: ", i, "\nConsensus Group: ", unique(md$consensus_group), "\nAQUATOX record: ", j)) +
    scale_x_date(limits=c(min_date, max_date)) +
    scale_color_identity(name = "Legend",
                         breaks = c("green4", "black"),
                         labels = c("Mesocosm Control Data", "Model Results"),
                         guide="legend") +
    scale_linetype_manual(name = "Title of Legend",
                          values = c(1, 2, 1),
                          guide = guide_legend(override.aes = list(color= c("blue", "darkgray", "orange")))) +
    theme  
  print(p)
  }
}
dev.off()


# END ---------------------------------------------------------------------


