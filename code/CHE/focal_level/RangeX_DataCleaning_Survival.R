################################################################################
### DATA CLEANING SCRIPT: COMPLETE YEARLY SIZE #################################
################################################################################

################################################################################

### Data used           : RangeX_clean_MetadataFocal_CHE.csv, survival data 2022 & 2023, clean YearlyDemographics data 2021 - 2023
### Date last modified  : 31.01.2024
### Purpose             : combine survival data for whole experiment duration (both spring and peak survival) into one data frame


################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex


### FUNCTIONS ##################################################################

# find 2nd biggest element (this now returns the max value if there's only 1 value - if this is not desired, put in the warning for "if(N>len)" instead of x)
max2 <- function(x, N=2){
  len <- length(x)
  if(N>len){
    x # warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}

### LOAD DATA SET ##############################################################

# load demographic data
dat_surv22 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Spring Survival Check/SurvCheck 2022/RangeX_raw_SpringSurvival_2022.csv")
dat_surv22_org <- dat_surv22

dat_surv23 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Spring Survival Check/SurvCheck 2023/RangeX_raw_SpringSurvival_2023.csv")
dat_surv23_org <- dat_surv23

dat_YS <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/RangeX_clean_YearlySize_2021_2023_CHE.csv")
dat_YS_org <- dat_YS

# load treatment key
meta_plant <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")

# define useful vectors
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")

final_columns <- c("unique_plant_ID", "species", "date_measurement", "date_planting", "collector", "variable", "value")

################################################################################
### SPRING 2022 ################################################################
################################################################################

dat_surv22 <- dat_surv22_org

### PREPARATION: COLUMNS, DATA CLASSES #########################################

# check data classes
str(dat_surv22)

# change column names: get column names
dput(colnames(dat_surv22))

# rename columns
dat_surv22 <- dat_surv22 %>%
  rename("date_measurement" = "Check_Date", 
        "region" =  "Country", 
        "site" =  "Site", 
        "block_ID_original" =  "Block_Number", 
        "plot_ID_original" =  "Plot_number", 
        "position_ID_original" =  "Position", 
        "species" =  "Species", 
        "value" =  "Survival",
        "collector" =  "Collector")

# add variable column, change species names, site etc.
dat_surv22 <- dat_surv22 %>%
  mutate(variable = "spring_survival",
         species = ifelse(species %in% names(species_names),  species_names[species], species),
         site = case_when(site == "NES" ~ "lo" , site == "CAL" ~ "hi")) 
  

### ADD TREATMENTS ETC. ########################################################

# select highest index for all positions where the last planting was before 03.08.2021 (everything which died beforehand is considered transplantation shock, everything which died and was replaced is a "real" death),
# and select highest plus second highest index for every position where the latest planting date was after 03.08.2023

meta_plant2022_23plus <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(
    case_when(
      max(date_planting) > as.Date("2021-08-03") ~ ind_number %in% c(max(ind_number), max2(ind_number)),  # keep the highest and second highest index numbers
      TRUE ~ ind_number == max(ind_number)  # keep only the highest index number
    )
  ) %>%
  ungroup()

# exactly 1836, great (1800 for the rest of the experiment plus 36 dying after 03.08.2021)

# also filter for only highest index an order to add index number to survival data first...
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup() %>%
  dplyr::select(c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "ind_number")) 

# 1800 as expected

# check scabiosa/ brapin Cal_1.1_8 and broere - brapin mixup Nes_7.1_1/3
dat_surv22[dat_surv22$block_ID_original == 1 & dat_surv22$plot_ID_original == 1 & dat_surv22$position_ID_original == 8 & dat_surv22$site == "hi",] # has an 0 for survival, can later be changed to 1 as brapin was even harvested in last year
dat_surv22[dat_surv22$block_ID_original == 7 & dat_surv22$plot_ID_original == 1 & dat_surv22$position_ID_original == 3 & dat_surv22$site == "lo",] # species name has to be changed
dat_surv22[dat_surv22$block_ID_original == 7 & dat_surv22$plot_ID_original == 1 & dat_surv22$position_ID_original == 1 & dat_surv22$site == "lo",] # species name has to be changed

dat_surv22[dat_surv22$block_ID_original == 7 & dat_surv22$plot_ID_original == 1 & dat_surv22$position_ID_original == 3 & dat_surv22$site == "lo",]$species <- "brapin"
dat_surv22[dat_surv22$block_ID_original == 7 & dat_surv22$plot_ID_original == 1 & dat_surv22$position_ID_original == 1 & dat_surv22$site == "lo",]$species <- "broere"

# merge (use full join so that individuals dead in spring 2022 but not transplant shock in 2021 are automatically added)
dat_surv22 <- full_join(dat_surv22, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))
dat_surv22 <- full_join(dat_surv22, meta_plant2022_23plus, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "ind_number"))
# nice, no mismatches, no NAs in metadata

# add metadata which is later necessary (typer etc. will be deleted anyway)
dat_surv22 <- dat_surv22 %>%
  mutate(date_measurement = case_when(is.na(date_measurement) == TRUE & site == "lo" ~ "04.05.22",
                                      is.na(date_measurement) == TRUE & site == "hi" ~ "14.05.22",
                                      TRUE ~ date_measurement),
         
         value = case_when(is.na(value) == TRUE & is.na(collector) == TRUE ~ 0, # set all the "new" NA entries to 0 (as those individuals did not survive), but don't do it for the missed Hypper (which has value = NA, but not collector = NA)
                           TRUE ~ value),
         collector = case_when(is.na(collector) == TRUE ~ "EI", 
                               TRUE ~ collector),
         variable = "spring_survival")


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_surv22_na <- dat_surv22 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in survival
dat_surv22_na <- dat_surv22 %>% 
  filter(is.na(value)) 

# 1 NA for Hypper Nes_1.2_5 - was forgotten, empty on data sheet

# check total
dat_surv22_na <- dat_surv22 %>%
  filter(if_any(-c(Comments, Plant_ID), is.na)) # 1 NA for the forgotten Hypper and 36 for the added individuals (prob. Typer)


### DELETE & ADD COLUMNS, DATATYPES ############################################

# delete all columns not in final data frame plus rearrange in right order
dat_surv22 <- dat_surv22 %>% 
  dplyr::select(any_of(final_columns))

str(dat_surv22)

# make sure to delete empty spaces in all columns
dat_surv22 <- dat_surv22 %>% 
  mutate(across(c(1:7), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_surv22 <- dat_surv22 %>%
  mutate(value = as.factor(value),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%y"),
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))



str(dat_surv22)



################################################################################
### SPRING 2023 ################################################################
################################################################################

dat_surv23 <- dat_surv23_org

### PREPARATION: COLUMNS, DATA CLASSES #########################################

# check data classes
str(dat_surv23)

# change column names: get column names
dput(colnames(dat_surv23))

# rename columns & delete empty row
dat_surv23 <- dat_surv23 %>%
  rename("date_measurement" = "Check_Date", 
         "region" =  "Country", 
         "site" =  "Site", 
         "block_ID_original" =  "Block_Number", 
         "plot_ID_original" =  "Plot_Number", 
         "position_ID_original" =  "Position", 
         "species" =  "Species", 
         "value" =  "Survival",
         "collector" =  "Collector") %>%
  remove_empty(which = "rows")

# add variable column, change species names, site etc.
dat_surv23 <- dat_surv23 %>%
  mutate(variable = "spring_survival",
         species = ifelse(species %in% names(species_names),  species_names[species], species),
         site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"))


### ADD TREATMENTS ETC. ########################################################


# check scabiosa/ brapin Cal_1.1_8 and broere - brapin mixup Nes_7.1_1/3
dat_surv23[dat_surv23$block_ID_original == 1 & dat_surv23$plot_ID_original == 1 & dat_surv23$position_ID_original == 8 & dat_surv23$site == "hi",] # has an 0 for survival, can later be changed to 1 as brapin was even harvested in last year
dat_surv23[dat_surv23$block_ID_original == 7 & dat_surv23$plot_ID_original == 1 & dat_surv23$position_ID_original == 3 & dat_surv23$site == "lo",] # species name has to be changed
dat_surv23[dat_surv23$block_ID_original == 7 & dat_surv23$plot_ID_original == 1 & dat_surv23$position_ID_original == 1 & dat_surv23$site == "lo",] # species name has to be changed

dat_surv23[dat_surv23$block_ID_original == 7 & dat_surv23$plot_ID_original == 1 & dat_surv23$position_ID_original == 3 & dat_surv23$site == "lo",]$species <- "brapin"
dat_surv23[dat_surv23$block_ID_original == 7 & dat_surv23$plot_ID_original == 1 & dat_surv23$position_ID_original == 1 & dat_surv23$site == "lo",]$species <- "broere"


# merge (use full join so that individuals dead in spring 2022 but not transplant shock in 2021 are automatically added)
dat_surv23 <- full_join(dat_surv23, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))
dat_surv23 <- full_join(dat_surv23, meta_plant2022_23plus, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "ind_number"))

# nice, no mismatches, no NAs in metadata

# add metadata which is later necessary (typer etc. will be deleted anyway)
dat_surv23 <- dat_surv23 %>%
  mutate(date_measurement = case_when(is.na(date_measurement) == TRUE & site == "lo" ~ "03.05.2023",
                                      is.na(date_measurement) == TRUE & site == "hi" ~ "24.05.2023",
                                      TRUE ~ date_measurement),
         collector = case_when(is.na(collector) == TRUE ~ "EI", 
                               TRUE ~ collector),
         variable = "spring_survival", 
         value = if_else(is.na(value) == TRUE, "0", value))


### PROBLEM FIXING #############################################################

# go through all comments

# has some rows with "?" in value (delete the question marks), 1 row with value = 9 (mistake already on data sheet, just make NA out of it (CHE.hi.warm.vege.wf.06.10.1)) 
dat_surv23 <- dat_surv23 %>%
  mutate(value = str_replace_all(value, "\\?", ""),
         value = ifelse(unique_plant_ID == "CHE.hi.warm.vege.wf.06.10.1", NA, value))

# ignore rest of comments, will be corrected after with peak measurement survival anyway

### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_surv23_na <- dat_surv23 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good 

# check for NAs: filter all rows with NA's in survival
dat_surv23_na <- dat_surv23 %>% 
  filter(is.na(value)) 

# is the manually added NA instead of 9 for CHE.hi.warm.vege.wf.06.10.1 (36 replaced individuals are fine)

# check total
dat_surv23_na <- dat_surv23 %>%
  filter(if_any(-c(Comments, Plant_ID), is.na)) # manually added NA plus 37 added individuals in some later deleted column


### DELETE & ADD COLUMNS, DATATYPES ############################################

# delete all columns not in final data frame plus rearrange in right order
dat_surv23 <- dat_surv23 %>% 
  dplyr::select(any_of(final_columns))

str(dat_surv23)

# make sure to delete empty spaces in all columns
dat_surv23 <- dat_surv23 %>% 
  mutate(across(c(1:7), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_surv23 <- dat_surv23 %>%
  mutate(value = as.factor(value),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"),
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))



str(dat_surv23)


################################################################################
### PEAK 2021 - 2023 ###########################################################
################################################################################

# reduce YearlySize measurements to survival data frame format, change column names, add variable column
dat_survYS <- dat_YS %>% 
  rename("value" = "survival") %>%
  mutate(variable = "peak_survival") %>%
  dplyr::select(any_of(final_columns)) %>%
  mutate(value = as.factor(value))

# check total for NAs
dat_survYS_na <- dat_survYS %>%
  filter(if_any(everything(), is.na)) # all good!


# filter out those 36 individuals replaced only after 03.08.2021 to add
meta_plant36_young <- meta_plant2022_23plus %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(length(unique(unique_plant_ID)) > 1,
         ind_number == min(ind_number)) %>%
  ungroup() %>%
  mutate(date_measurement = as.Date(NA), # add "correct" (median at the two sites) later
         collector = "EI", #just use EI for all 
         variable = "peak_survival",
         value = as.factor(0)) %>%
  dplyr::select(unique_plant_ID, species, date_measurement, date_planting, collector, variable, value)

# get median date of measurements at lo/ hi site to add to those "measurements" (were never measured as dead in 2021)
hi_2022 <- median(dat_YS[grepl("hi", dat_YS$unique_plant_ID) == TRUE & as.numeric(as.character(format(dat_YS$date_measurement, "%Y"))) == 2022, ]$date_measurement)
lo_2022 <- median(dat_YS[grepl("lo", dat_YS$unique_plant_ID) == TRUE & as.numeric(as.character(format(dat_YS$date_measurement, "%Y"))) == 2022, ]$date_measurement)
hi_2023 <- median(dat_YS[grepl("hi", dat_YS$unique_plant_ID) == TRUE & as.numeric(as.character(format(dat_YS$date_measurement, "%Y"))) == 2023, ]$date_measurement)
lo_2023 <- median(dat_YS[grepl("lo", dat_YS$unique_plant_ID) == TRUE & as.numeric(as.character(format(dat_YS$date_measurement, "%Y"))) == 2023, ]$date_measurement)

# prepare two data frames (one for 2022, one for 2023) to add
meta_plant36_young_2022 <- meta_plant36_young %>%
  mutate(date_measurement = if_else(grepl("lo", unique_plant_ID) == TRUE, as.Date(lo_2022), as.Date(hi_2022)))
meta_plant36_young_2023 <- meta_plant36_young %>%
  mutate(date_measurement = if_else(grepl("lo", unique_plant_ID) == TRUE, as.Date(lo_2023), as.Date(hi_2023)))

# add them 
dat_survYS <- bind_rows(dat_survYS, meta_plant36_young_2022, meta_plant36_young_2023)




################################################################################
### COMBINED ###################################################################
################################################################################

dat_surv_complete <- bind_rows(dat_surv22, dat_surv23, dat_survYS) # nice

# check for NA's
dat_surv_complete_na <- dat_surv_complete %>%
  filter(if_any(everything(), is.na)) # the two manually added NAs 

# check levels
dat_surv_complete_levels <- dat_surv_complete %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique)

unique(dat_surv_complete$value)


# check whether survival progression makes sense

# get year out of date and call them timepoint 1-5 (1 = peak 2021, 2 = spring 2022, 3 = peak 2022 etc.)
dat_surv_check <- dat_surv_complete %>%
  mutate(year = as.numeric(as.character(format(date_measurement, "%Y"))),
         timeperiod = case_when(variable == "spring_survival" & year == 2022 ~ "timeperiod_2",
                                variable == "spring_survival" & year == 2023 ~ "timeperiod_4",
                                variable == "peak_survival" & year == 2021 ~ "timeperiod_1",
                                variable == "peak_survival" & year == 2022 ~ "timeperiod_3",
                                variable == "peak_survival" & year == 2023 ~ "timeperiod_5"))

# make wide data frame
dat_surv_check_wide <- dat_surv_check %>%
  dplyr::select(-variable, -date_measurement, -date_planting, -year, -collector) %>%
  pivot_wider(names_from = timeperiod, values_from = value) %>%
  relocate(unique_plant_ID, species, paste0("timeperiod_", seq(1, 5)))


# again, is 1836 as 36 individuals were only replaced after timepoint_1 and therefore have a different ind_number and unqiue_plant_ID

illogical_survival <- dat_surv_check_wide %>%
  rowwise() %>%
  mutate(across(-c(1,2), ~as.numeric(as.character(.x)))) %>%
  mutate(logical = ifelse(any(diff(c_across(-c(1,2))) > 0, na.rm = TRUE), "not ok", "ok")) %>%
  ungroup()

length(illogical_survival[illogical_survival$logical == "not ok",]$unique_plant_ID)

# 67 illogical timeseries (used to be 80 - 13 were already corrected in YearlySize) 


# correct 
extrasurv_time4 <- illogical_survival[illogical_survival$logical == "not ok" & illogical_survival$timeperiod_4 == 0 & illogical_survival$timeperiod_3 == 1 & 
                                       illogical_survival$timeperiod_5 == 1,]$unique_plant_ID # peak 2022 and 2023 = 1 --> spring 2023 should also be 1
extrasurv_time2 <- illogical_survival[illogical_survival$logical == "not ok" & illogical_survival$timeperiod_2 == 0  & illogical_survival$timeperiod_1 == 1 & 
                                       illogical_survival$timeperiod_3 == 1,]$unique_plant_ID # peak 2021 and 2022 = 1 --> spring 2022 should also be 1
extrasurvNA_time2 <- illogical_survival[illogical_survival$logical == "not ok" & illogical_survival$timeperiod_2 == 0 & illogical_survival$timeperiod_3 == 1 & 
                                         is.na(illogical_survival$timeperiod_1) == TRUE,]$unique_plant_ID # peak 2021 = NA and peak 2022 = 1 --> spring 2022 should also be 1
deletesurv_time4 <- illogical_survival[illogical_survival$logical == "not ok" & illogical_survival$timeperiod_3 == 0 & illogical_survival$timeperiod_5 == 0 & 
                                         illogical_survival$timeperiod_4 == 1,]$unique_plant_ID # peak 2022 and 2023 = 0 --> spring 2023 should also be 0


dat_surv_complete <- dat_surv_complete %>%
  mutate(timeperiod = case_when(variable == "spring_survival" & lubridate::year(date_measurement) == 2022 ~ "timeperiod_2",
                                variable == "spring_survival" & lubridate::year(date_measurement) == 2023 ~ "timeperiod_4",
                                variable == "peak_survival" & lubridate::year(date_measurement) == 2021 ~ "timeperiod_1",
                                variable == "peak_survival" & lubridate::year(date_measurement) == 2022 ~ "timeperiod_3",
                                variable == "peak_survival" & lubridate::year(date_measurement) == 2023 ~ "timeperiod_5"),
         value = case_when(unique_plant_ID %in% extrasurv_time4 & timeperiod == "timeperiod_4" ~ as.factor(1),
                           unique_plant_ID %in% extrasurv_time2 & timeperiod == "timeperiod_2" ~ as.factor(1),
                           unique_plant_ID %in% extrasurvNA_time2 & timeperiod == "timeperiod_2" ~ as.factor(1),
                           unique_plant_ID %in% deletesurv_time4 & timeperiod == "timeperiod_4" ~ as.factor(0),
                           TRUE ~ value))  %>% # corrects 43 values, check the rest
  dplyr::select(-timeperiod)

# check whether no more illogical time series: get year out of date and call them timepoint 1-5 (1 = peak 2021, 2 = spring 2022, 3 = peak 2022 etc.)
dat_surv_check2 <- dat_surv_complete %>%
  mutate(year = as.numeric(as.character(format(date_measurement, "%Y"))),
         timeperiod = case_when(variable == "spring_survival" & year == 2022 ~ "timeperiod_2",
                                variable == "spring_survival" & year == 2023 ~ "timeperiod_4",
                                variable == "peak_survival" & year == 2021 ~ "timeperiod_1",
                                variable == "peak_survival" & year == 2022 ~ "timeperiod_3",
                                variable == "peak_survival" & year == 2023 ~ "timeperiod_5"))

# make wide data frame
dat_surv_check_wide2 <- dat_surv_check2 %>%
  dplyr::select(-variable, -date_measurement, -date_planting, -collector, -year) %>%
  pivot_wider(names_from = timeperiod, values_from = value) %>%
  relocate(unique_plant_ID, species, paste0("timeperiod_", seq(1, 5)))

# illogical time series?
illogical_survival2 <- dat_surv_check_wide2 %>%
  rowwise() %>%
  mutate(across(-c(1,2), ~as.numeric(as.character(.x)))) %>%
  mutate(logical = ifelse(any(diff(c_across(-c(1,2))) > 0, na.rm = TRUE), 'not ok', 'ok')) %>%
  ungroup()

length(illogical_survival2[illogical_survival2$logical == "not ok",]$unique_plant_ID) # 0!


# check for NAs in survival 
dat_surv_complete_na <- dat_surv_complete %>% 
  filter(is.na(value))

# CHE.lo.ambi.vege.wf.01.05.1 spring 2022 should be 1 (is alive later)
dat_surv_complete[dat_surv_complete$unique_plant_ID == "CHE.lo.ambi.vege.wf.01.05.1" & dat_surv_complete$date_measurement == "2022-05-05" & dat_surv_complete$variable == "spring_survival",]$value <- as.factor(1)

# CHE.hi.warm.vege.wf.06.10.1 spring 2023 should also be 1 (is alive at peak 2023)
dat_surv_complete[dat_surv_complete$unique_plant_ID == "CHE.hi.warm.vege.wf.06.10.1" & dat_surv_complete$date_measurement == "2023-05-24" & dat_surv_complete$variable == "spring_survival",]$value <- as.factor(1)


# check again
dat_surv_complete_na <- dat_surv_complete %>% 
  filter(is.na(value)) # nice



### SAVE CLEAN VERSION #########################################################

write.csv(dat_surv_complete, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Spring Survival Check/RangeX_clean_Survival_2021_2023_CHE.csv",
          row.names = FALSE)



















