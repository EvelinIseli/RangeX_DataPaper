################################################################################
### DATA CLEANING SCRIPT: EXTRACT PLANT ID #####################################
################################################################################

################################################################################

### Data used           : RangeX_raw_InitialSizePlantID_2021.csv, RangeX_Metadata_OLD.csv, RangeX_PlotMetadata_CHE_OLD.csv
### Date last modified  : 25.01.2024
### Purpose             : extract Plant_ID from all initial size measurements at the end of season 2021 to add to RangeX_raw_AutumnSize_2021.xls (added to raw excel file before cleaning)

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidyr) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# loading data for plant_id's
dat_ID <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_raw_InitialSizePlantID_2021.csv")

dat_ID_org <- dat_ID

functional_groups <- c("brapin" = "graminoid", "broere" = "graminoid", "daucar" = "forb", "hypper" = "forb",
                       "medlup" = "legume", "plamed" = "forb", "silvul" = "forb", "scacol" = "forb",
                       "cenjac" = "forb", "salpra" = "forb")


### CLEAN & EXTRACT IDs BEFORE AUTUMN MEASUEREMNTS #############################

# check data classes
str(dat_ID) # date is a string - change to date

# change date format, change sites, rename columns
dat_ID <- dat_ID_org %>% 
  rowwise() %>%
  mutate(date_planting = as.Date(Planting_Date, "%d.%m.%Y"),
         site = case_when(Site == "NES" ~ "lo" , Site == "CAL" ~ "hi")) %>%
  rename("region" = "Country", "block_ID_original" = "Block_Number", "plot_ID_original" = "Plot_Number", "position_ID_original" = "Position", "ind_number" = "Plant_ID") %>%
  ungroup() %>%
  dplyr::select(-Comment, -Site, -Planting_Date)

# delete all entries after autumn measurements (they're from replacements after autumn measurements)
dat_ID1 <- dat_ID %>%
  filter(date_planting < "2021-08-03")


# for each site/ block/ plot/ position select the highest ID 
highest_id_before_autumnM <- dat_ID1 %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original) %>%
  filter(ind_number == max(ind_number))

# has 1802 observations instead of 1800  - why?

# check for duplicated rows
highest_id_before_autumnM %>% dplyr::select(-date_planting) %>% distinct() # there are two rows apparently measured at two dates...

highest_id_before_autumnM %>% 
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, ind_number) %>% 
  filter(row_number() > 1) # problem are two Broere with wrong species planted and exchanged on 05.07.21, which of course have ID = 1 again
# --> only keep the two from 05.07.21

# delete "duplicated rows"
highest_id_before_autumnM <- highest_id_before_autumnM %>%
  filter(!(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 7 & date_planting == "2021-06-16"),
         !(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 3 & date_planting == "2021-06-16"))

# save this file to later 
write.csv(highest_id_before_autumnM, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_clean_PlantIDBeforeYearlySize_2021.csv",
          row.names = FALSE)



### CLEAN & EXTRACT IDs AFTER AUTUMN MEASUEREMNTS ##############################

# keep all entries (no deleting after autumn measurements)

# for each site/ block/ plot/ position select the highest ID 
highest_id_after_autumnM <- dat_ID %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original) %>%
  filter(ind_number == max(ind_number)) 

# has 1802 observations instead of 1800  - why?

# check for duplicated rows
highest_id_after_autumnM %>% 
  group_by(region, site, block_ID_original, plot_ID_original,position_ID_original, ind_number) %>% 
  filter(row_number() > 1) 

# problem 1 are two Broere with wrong species planted and exchanged on 05.07.21, which of course have ID = 1 again --> only keep the two from 05.07.21
# problem 2 is a plant in Nes_5.2, position 3, plant_id 3 with different date...delete 1st entry (is recorded on original data sheets but apparently plant not found the day after?)

# delete "duplicated rows"
highest_id_after_autumnM <- highest_id_after_autumnM %>%
  filter(!(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 7 & date_planting == "2021-06-16"),
         !(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 3 & date_planting == "2021-06-16"),
         !(site == "lo" & block_ID_original == 5 & plot_ID_original == 2 & position_ID_original == 3 & date_planting == "2021-09-21")) 


# save this file to later 
write.csv(highest_id_after_autumnM, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_clean_PlantIDafterYearlySize_2021.csv",
          row.names = FALSE)



### CREATE ID's AND SAVE PLANT METADATA ########################################

# load old key (with no ind_number) to see how columns are arranged etc.
key <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_Metadata_OLD.csv", stringsAsFactors = FALSE) %>%
  filter(region == "CHE") %>%
  dplyr::select(-block_ID, -position_ID) %>% # delete those as they won't be correct (1 instead of 01 etc.)
  mutate(plot_ID_original = as.integer(plot_ID_original),
         position_ID_original = as.integer(position_ID_original))

# check data type
str(key)
str(dat_ID)

# add treatments etc.
dat_ID_new <- dat_ID %>%
  left_join(key, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original"))

# create new columns in dat_ID
dat_ID_new <- dat_ID_new %>%
  mutate(block_ID = ifelse(grepl("[[:digit:]]{2}", block_ID_original) == TRUE, block_ID_original, paste0("0", block_ID_original)),
         position_ID = ifelse(grepl("[[:digit:]]{2}", position_ID_original) == TRUE, position_ID_original, paste0("0", position_ID_original)))

# add index number to plant ID
dat_ID_new <- dat_ID_new %>%
  mutate(unique_plant_IDx = paste(unique_plant_ID, ind_number, sep = "."))

# now check for dublicate IDs
dat_ID_new %>%
  group_by(unique_plant_IDx) %>%
  filter(row_number() > 1)


# same problems as above for IDs after autumn measurements
# problem 1 are two Broere with wrong species planted and exchanged on 05.07.21, which of course have ID = 1 again --> only keep the two from 05.07.21
# problem 2 is a plant in Nes_5.2, position 3, plant_id 3 with different date...delete 1st entry (is recorded on original data sheets but apparently plant not found the day after?)

# delete "duplicated rows"
dat_ID_new <- dat_ID_new %>%
  filter(!(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 7 & date_planting == "2021-06-16"),
         !(site == "lo" & block_ID_original == 9 & plot_ID_original == 2 & position_ID_original == 3 & date_planting == "2021-06-16"),
         !(site == "lo" & block_ID_original == 5 & plot_ID_original == 2 & position_ID_original == 3 & date_planting == "2021-09-21")) 

# delete "old" plant ID and also planting date
dat_ID_new <- dat_ID_new %>%
  dplyr::select(-unique_plant_ID) %>%
  rename("unique_plant_ID" = "unique_plant_IDx")

# add functional groups
dat_ID_new <- dat_ID_new %>%
  mutate(functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species))

# check whether there's the correct number of individuals (1800) if only the oldest ind_number fr each position is considered
dat_ID_continuous <- dat_ID_new %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID, functional_group) %>%
  filter(ind_number == max(ind_number))


# for future uses (yearly size etc.) the metadata can just be selected for the highest ind-number to get the correct ID for each individual (planting date will be in initial size data frame)

# check data classes once more
str(dat_ID_new)

# re-arrange columns
dat_ID_new <- dat_ID_new %>%
  relocate(date_planting, region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID, position_ID, 
           ind_number, unique_plot_ID, unique_plant_ID)
# save
write.csv(dat_ID_new, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv",
          row.names = FALSE)


### SAVE PLOT METADATA #########################################################

# read in old plot metadata
dat_ID_plot <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_PlotMetadata_CHE_OLD.csv", stringsAsFactors = FALSE)

dat_ID_plot <- dat_ID_plot %>%
  mutate(block_ID = ifelse(grepl("[[:digit:]]{2}", block_ID_original) == TRUE, block_ID_original, paste0("0", block_ID_original)))
  
# save
write.csv(dat_ID_plot, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv",
          row.names = FALSE)





  