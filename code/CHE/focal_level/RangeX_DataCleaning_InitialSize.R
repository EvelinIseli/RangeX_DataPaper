################################################################################
### DATA CLEANING SCRIPT: INITIAL SIZE 2021 ####################################
################################################################################

################################################################################

### Data used           : RangeX_raw_InitialSize_2021.csv, RangeX_clean_MetadataFocal_CHE.csv
### Date last modified  : 25.01.2024
### Purpose             : Clean the COMPLETE raw data file (Missing entries? Missing values? Implausible values? Wrong column names? Data classes defined?, add treatments

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidyr) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# loading data
#dat_InS <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_raw_InitialSize_2021.csv")
#meta_plant <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_clean_MetadataFocal_CHE.csv")
dat_InS <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_InitialSize/RangeX_raw_InitialSize_2021.csv")
meta_plant <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/1_Metadata/2_Metadata_FocalsPlots/RangeX_clean_MetadataFocal_CHE.csv")

functional_groups <- c("brapin" = "graminoid", "broere" = "graminoid", "daucar" = "forb", "hypper" = "forb",
                       "medlup" = "legume", "plamed" = "forb", "silvul" = "forb", "scacol" = "forb",
                       "cenjac" = "forb", "salpra" = "forb")

# wanted columns & data types
final_columns <- c("unique_plant_ID", "ind_number", "species", "functional_group", "date_measurement", "date_planting", "collector", "height_vegetative_str", "vegetative_width", 
                   "leaf_length1", "leaf_length2", "leaf_length3", "number_leaves")

integer_cols <- c("ind_number", "height_vegetative_str", "vegetative_width", "leaf_length1", "leaf_length2", "leaf_length3", "number_leaves")
string_cols <- c("unique_plant_ID", "species", "functional_group", "collector")
date_cols <- c("date_measurement", "date_planting")

### PREPARATION: COLUMNS, DATA CLASSES #########################################

# check data classes
str(dat_InS)

# change date from string to actual date 
dat_InS <- dat_InS %>%
  mutate(Measurement_Date = as.Date(Measurement_Date, "%d.%m.%Y"))

# change column names: get column names
dput(colnames(dat_InS))

# change them to new names
dat_InS <- dat_InS %>%
  rename("date_measurement" = "Measurement_Date",
         "region" = "Country",
         "site" = "Site",
         "block_ID" = "Block_Number",
         "plot_ID" = "Plot_Number",
         "position_ID" = "Position",
         "ind_number" = "Plant_ID",
         "species" = "Species",
         "height_vegetative_str" = "Veg_Height",
         "number_leaves" = "Number_Leaves",
         "leaf_length1" = "Leaflength_1",
         "leaf_length2" = "Leaflength_2",
         "leaf_length3" = "Leaflength_3",
         "collector" = "Collector")

# what is deleted? typer, writer
dat_InS <- dat_InS %>% 
  dplyr::select(- Typer, - Writer)


# update column content (CAL/ NES to hi/ lo, species to 6 letter code, add ind_number to unique_plant_ID
dat_InS <- dat_InS %>%
  mutate(site = case_when(site == "NES" ~ "lo" , site == "CAL" ~ "hi"),
         species = str_extract_all(paste0(" ", species), " .{3}")) %>%
  rowwise() %>%
  mutate(species = gsub(" ", "", tolower(paste(species, collapse=""))),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species)) %>%
  ungroup()


# add/ construct unique_plant_ID
dat_InS_test <- dat_InS %>%
  left_join(meta_plant, by = c("region", "site", "block_ID","plot_ID" = "plot_ID_original", "position_ID", "ind_number", "species", "functional_group")) %>%
  dplyr::select(-position_ID_original, -block_ID_original, -unique_plot_ID, -treat_warming, -treat_competition, -added_focals)


### PROBLEM FIXING #############################################################

# get NAs for which no corresponding uniqe_plant_ID was found
missing_ID <- dat_InS_test %>%
  filter(is.na(unique_plant_ID))

# solve the issues
# 1
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 2 & meta_plant$plot_ID_original == 2 & meta_plant$position_ID_original == 29,] # seems to be only round 1 and 2 of planting
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 2 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 29,] # measurement date is 2021-06-28, so it has to be from round two

dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 2 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 29,]$ind_number <- 2


# 2
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 7 & meta_plant$plot_ID_original == 1 & meta_plant$position_ID_original == 1,] # has all three rounds
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 7 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 1,] # problem seems to be the brapin - broere confusion: in Nes_7.1 position 1 is a broere and position 3 a brapin

dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 7 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 1,]$species <- "broere"
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 7 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 3,]$species <- "brapin"

# 3
meta_plant[meta_plant$site == "hi" & meta_plant$block_ID_original == 1 & meta_plant$plot_ID_original == 1 & meta_plant$position_ID_original == 10,] # has only one round, planting 17.06.2021
dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 1 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 10,] # measurement date is 2021-06-23, so it's highly probably it's after round 1

dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 1 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 10,]$ind_number <- 1

# 4
meta_plant[meta_plant$site == "hi" & meta_plant$block_ID_original == 5 & meta_plant$plot_ID_original == 4 & meta_plant$position_ID_original == 6,] # has only one round, planting 17.06.2021
dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 5 & dat_InS$plot_ID == 4 & dat_InS$position_ID == 6,] # measurement date is 2021-06-23, so it's highly probably it's after round 1

dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 5 & dat_InS$plot_ID == 4 & dat_InS$position_ID == 6,]$ind_number <- 1

# 5
meta_plant[meta_plant$site == "hi" & meta_plant$block_ID_original == 5 & meta_plant$plot_ID_original == 5 & meta_plant$position_ID_original == 6,] # has only one round, planting 17.06.2021
dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 5 & dat_InS$plot_ID == 5 & dat_InS$position_ID == 6,] # measurement date is 2021-06-23, so it's highly probably it's after round 1

dat_InS[dat_InS$site == "hi" & dat_InS$block_ID == 5 & dat_InS$plot_ID == 5 & dat_InS$position_ID == 6,]$ind_number <- 1

# check whether it worked and add unique_plant_ID again
dat_InS_testnew <- dat_InS %>%
  left_join(meta_plant, by = c("region", "site", "block_ID","plot_ID" = "plot_ID_original", "position_ID", "ind_number", "species")) %>%
  dplyr::select(-position_ID_original, -block_ID_original, -unique_plot_ID, -treat_warming, -treat_competition, -added_focals)

# get NAs for which no corresponding unique_plant_ID was found
missing_ID <- dat_InS_testnew %>%
  filter(is.na(unique_plant_ID))

# no NAs left

# check for duplicated rows
dat_InS_duplicated <- dat_InS_testnew %>% 
  group_by(region, site, block_ID, plot_ID,position_ID, ind_number, unique_plant_ID) %>% 
  filter(row_number() > 1) 

# solve the issues 2.0
# 1
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 10 & meta_plant$plot_ID_original == 1 & meta_plant$position_ID_original == 7,] # 3 rounds, planting dates 16.06., 22.06., 05.07.
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 10 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 7,] # measurement dates are from 28.06. and 05.07., so they must be from round 2 and 3

dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 10 & dat_InS$plot_ID == 1 & dat_InS$position_ID == 7 & dat_InS$date_measurement == "2021-07-05",]$ind_number <- 3

# 2
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 9 & meta_plant$plot_ID_original == 2 & meta_plant$position_ID_original == 3,] # 1 round
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 9 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 3,] # need to see full comment, seems to be a species mix-up
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 9 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 3,]$Comments # plant with measuring date 21.06. needs to be deleted

dat_InS <- dat_InS %>%
  filter(!(site == "lo" & block_ID == 9 & plot_ID == 2 & position_ID == 3 & date_measurement == "2021-06-21"))

# 3
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 9 & meta_plant$plot_ID_original == 2 & meta_plant$position_ID_original == 7,] # 1 round
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 9 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 7,] # need to see full comment, seems to be a species mix-up
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 9 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 7,]$Comments # # plant with measuring date 21.06. needs to be deleted

dat_InS <- dat_InS %>%
  filter(!(site == "lo" & block_ID == 9 & plot_ID == 2 & position_ID == 7 & date_measurement == "2021-06-21"))

# 4
meta_plant[meta_plant$site == "lo" & meta_plant$block_ID_original == 5 & meta_plant$plot_ID_original == 2 & meta_plant$position_ID_original == 3,] # 3 rounds
dat_InS[dat_InS$site == "lo" & dat_InS$block_ID == 5 & dat_InS$plot_ID == 2 & dat_InS$position_ID == 3,] # two measurements are really close together (21.09. and 22.09.), must be accidental --> delete the one from 22.09.

dat_InS <- dat_InS %>%
  filter(!(site == "lo" & block_ID == 5 & plot_ID == 2 & position_ID == 3 & date_measurement == "2021-09-22"))

# check whether it worked and add unique_plant_ID again
dat_InS <- dat_InS %>%
  left_join(meta_plant, by = c("region", "site", "block_ID","plot_ID" = "plot_ID_original", "position_ID", "ind_number", "species", "functional_group")) %>%
  dplyr::select(-position_ID_original, -block_ID_original, -unique_plot_ID, -treat_warming, -treat_competition, -added_focals)


# check for NAs and duplictaed rows
dat_InS %>%
  filter(is.na(unique_plant_ID)) # NONE!
dat_InS %>% 
  group_by(region, site, block_ID, plot_ID,position_ID, ind_number, unique_plant_ID) %>% 
  filter(row_number() > 1)  # NONE!


# also check whether there's 1800 initial measurements if only the highest ind_number is considered
dat_InS_check <- dat_InS %>%
  group_by(region, site, block_ID, plot_ID, position_ID, species) %>%
  filter(ind_number == max(ind_number))

# perfect, 1800 entries!



### MISSING ENTRIES/ VALUES/ NA's/ IMPLAUSIBLE VALUES ##########################

# check for NAs: filter all rows with NA's
dat_InS_na <- dat_InS %>% 
  filter_all(any_vars(is.na(.))) 

# what to do with NA's?
# NA in leaflength_3 or leaflength_2 and leaflength_3: ok, leave it be, just a crappy plant
# NA in veg_height, but not in leaflength: is it Salvia, Plantago, Scabiosa, Daucus or Grasses, use longest leaflength instead
# NA in number_leaves, but not rest: ?
# NA everywhere: ?

# use leaflength for veg_height
dat_InS[dat_InS$unique_plant_ID == "CHE.hi.warm.bare.wf.06.10.1",]$height_vegetative_str <- dat_InS[dat_InS$unique_plant_ID == "CHE.hi.warm.bare.wf.06.10.1",]$leaf_length1


# cases checked with data sheets, but not resolved
# missing # leaves: Cal_2.1_21, Cal_2.1_13, Cal_5.5_25, Cal_6.1_29, Cal_6.6_30, Cal_7.1_21, Cal_7.2_6, Cal_9.4_22, Cal_10.4_25
# 2 leaves recorded but only 1 measured: Cal_6.1_3
# completely forgotten: Cal_2.3_8, Cal_2.4_18

# plot to check for implausible values
# make plotting data frame

dat_InS_plot <- dat_InS %>% 
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16))

dat_InS_plot <- dat_InS_plot %>%
  pivot_longer(cols = 9:13, names_to = "variable", values_to = "values")

ggplot(data = dat_InS_plot[dat_InS_plot$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = dat_InS_plot[dat_InS_plot$site == "hi",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = dat_InS_plot, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

# potential outlier values which were double-checked in original data, but were fund to be typed correctly: Cal_1.3_7 (Bromus) and Cal_7.4_2 (Daucus)



### DELETE COLUMNS, DATATYPES, SAVE CLEAN VERSION ##############################

# check data classes once more
str(dat_InS)

# add empty columns
missing_columns <- setdiff(final_columns, colnames(dat_InS)) # find out which column are missing

dat_InS[ , missing_columns] <- NA # add those columns filled with NAs

# delete all columns not in final data frame plus rearrange in right order
dat_InS <- dat_InS %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
dat_InS <- dat_InS %>% 
  mutate(across(c(1:13), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_InS <- dat_InS %>%
  mutate(across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%Y-%m-%d"), 
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

# correct typo in date
dat_InS[dat_InS$unique_plant_ID == "CHE.hi.ambi.vege.wf.03.19.1",]$date_measurement <- as.Date("2021-06-24", format = "%Y-%m-%d")


# save clean version
#write.csv(dat_InS, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_clean_InitialSize_2021_CHE.csv",
#          row.names = FALSE)
write.csv(dat_InS, "/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/6_DataClean/RangeX_clean_InitialSize_2021_CHE.csv",
          row.names = FALSE)

