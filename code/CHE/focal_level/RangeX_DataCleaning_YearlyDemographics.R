################################################################################
### DATA CLEANING SCRIPT: COMPLETE YEARLY SIZE #################################
################################################################################

################################################################################

### Data used           : RangeX_raw_AutumnSize_2021.csv, RangeX_raw_AutumnSize_2022.csv, RangeX_clean_MetadataFocal_CHE.csv,  RangeX_raw_AutumnSize_2023.csv
### Date last modified  : 25.01.2024
### Purpose             : Clean the COMPLETE raw data file (Missing entries? Missing values? Implausible values? Wrong column names? Data classes defined?) and add treatments & Plant_ID 
###                       for each yearly size measurement data set (2021 - 2023)

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex


### LOAD DATA SET ##############################################################

# load demographic data
dat_YS21 <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/YearlySize 2021/RangeX_raw_YearlySize_2021.csv")
dat_YS21_org <- dat_YS21

dat_YS22 <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/YearlySize 2022/RangeX_raw_YearlySize_2022.csv")
dat_YS22_org <- dat_YS22

dat_YS23 <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/YearlySize 2023/RangeX_raw_YearlySize_2023.csv")
dat_YS23_org <- dat_YS23

# load additional measurements from 2022 for 
dat_YS22_CenjacAut <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/YearlySize 2022/RangeX_raw_AutumnNew_Cenjac_2022.csv")
dat_YS22_CenjacAut_org <- dat_YS22_CenjacAut

# load treatment key
meta_plant <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")

# define useful vector
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")
functional_groups <- c("brapin" = "graminoid", "broere" = "graminoid", "daucar" = "forb", "hypper" = "forb",
                      "medlup" = "legume", "plamed" = "forb", "silvul" = "forb", "scacol" = "forb",
                      "cenjac" = "forb", "salpra" = "forb")

# wanted columns & data types
final_columns <- c("unique_plant_ID", "species", "functional_group", "date_measurement", "date_planting", "collector", "survival", "height_vegetative_str", "height_reproductive_str", 
                   "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan", "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", 
                   "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_leafclusters", "number_flowers", "mean_inflorescence_size", 
                   "herbivory")

integer_cols <- c("height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan","stem_diameter", 
                  "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_leafclusters", 
                  "number_flowers", "mean_inflorescence_size")
string_cols <- c("unique_plant_ID", "species", "functional_group", "collector")
factor_cols <- c("survival", "herbivory")
date_cols <- c("date_measurement", "date_planting")




################################################################################
### 2021 #######################################################################
################################################################################


### PREPARATION: COLUMNS, DATA CLASSES #########################################

dat_YS21 <- dat_YS21_org

# check data classes
str(dat_YS21)

# change column names: get column names
dput(colnames(dat_YS21))

# change them to new names
dat_YS21 <- dat_YS21_org %>%
  rename("region" = "Country",
         "site" = "Site",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species",
         "height_vegetative_str" = "Veg_Height",
         "height_reproductive_str" = "Rep_Height",
         "number_leaves" = "Number_Leaves",
         "leaf_length1" = "Leaflength_1",
         "leaf_length2" = "Leaflength_2",
         "leaf_length3" = "Leaflength_3",
         "number_flowers" = "Number_Flowers",
         "collector" = "Collector",
         "date_measurement" = "Measurememnt_Date")

# correct column content etc.
dat_YS21 <- dat_YS21 %>%
  mutate(Inflorescence_Length = na_if(Inflorescence_Length, ""), # replace empty strings with NA
         site = case_when(site == "NES" ~ "lo" , site == "CAL" ~ "hi")) %>% # change site names
  separate(Inflorescence_Length, into = c("inflorescence_length1", "inflorescence_length2"), sep = ",") %>% # separate multiple inflorescence length into different columns (there's max. 2, so two columns are enough)
  mutate(inflorescence_length1 = as.numeric(inflorescence_length1),
         inflorescence_length2 = as.numeric(inflorescence_length2)) %>%
  rowwise() %>%
  mutate(mean_inflorescence_size = mean(c(inflorescence_length1, inflorescence_length2), na.rm = TRUE),
         mean_inflorescence_size = ifelse(is.nan(mean_inflorescence_size), NA, mean_inflorescence_size)) %>% # take mean for inflorescence
  #dplyr::select(-Plant_ID) %>%
  ungroup()

# warning message "Expected 2 pieces. Missing pieces filled with `NA` in 13 rows [17, 40, 47, 59, 70, 77, 100, 119, 149, 197, 220, 239, 250].": Fine, for most individuals only one flower was measured!
# see below for more detailed analysis of this problem

# add functional groups, change species names
dat_YS21 <- dat_YS21 %>%
  mutate(species = ifelse(species %in% names(species_names),  species_names[species], species),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species))

# add herbivory column (1 = herbivory, 0 = no herbivory)
mypattern <- c("herb", "erbi", "eat", "pathogen", "aphi", "lice", "cut")

dat_YS21 <- dat_YS21 %>%
  mutate(herbivory1 = ifelse(grepl(paste(mypattern, collapse='|'), Comments) == TRUE, 1, 0)) %>% # mark the rows with one of the above patterns as 1, rest as 0
  mutate(herbivory = herbivory1) %>% # herbivory of dead plants will later be set to NA instead of 0
  ungroup()

### ADD TREATMENTS ETC. ########################################################

# prepare treatment key
meta_plant <- meta_plant %>%
  filter(region == "CHE") %>%
  mutate(date_planting = as.Date(date_planting, "%Y-%m-%d"))

# autumn measurements were from 16.08. - 19.08.2021, so if all individuals planted after that are filtered out and then only retain the max ind_number, 
# the ID from when the autumn measurements were done is the result

meta_plant2021 <- meta_plant %>%
  filter(date_planting < "2021-08-20") %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup()
  
# exactly 1800, great


# merge treatments to 2021 size data frame
dat_YS21 <- full_join(dat_YS21, meta_plant2021, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "functional_group"))


### PROBLEM FIXING #############################################################

# check 
# problem with Cal_1.1 8: Should be brapin, but was assumed that a scacol was planted by mistake, so in 2021 this scacol was measured instead --> delete the scacol measurements for now, but add collector data 
# to supposed brapin for now (basically brapin and fill with NAs)

dat_YS21[dat_YS21$unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" & dat_YS21$species == "brapin" & dat_YS21$site == "hi",]$collector <- "EI"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" & dat_YS21$species == "brapin" & dat_YS21$site == "hi",]$Comments <- "was a scacol planted instead? --> a scacol was measured in 2021"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" & dat_YS21$species == "brapin" & dat_YS21$site == "hi",]$date_measurement <- "18.08.21"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" & dat_YS21$species == "brapin" & dat_YS21$site == "hi",]$herbivory <- NA
dat_YS21[dat_YS21$unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" & dat_YS21$species == "brapin" & dat_YS21$site == "hi",]$functional_group <- "graminoid"

# delete row with wrong scacol entries
dat_YS21 <-dat_YS21[!c(dat_YS21$position_ID_original == 8 & dat_YS21$block_ID_original == 1 & dat_YS21$plot_ID_original == 1 & dat_YS21$site == "hi" & dat_YS21$species == "scacol"),]


# problem with brapin/ broere in Nes_7.1: broere is on position 1 and brapin on position 3 (wrong planting), which wasn't yet noticed for the measurements in 2021 --> add collector, date etc. to correct metadata,
# delete row in original data frame
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.01.2" & dat_YS21$species == "broere" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 1,]$collector <- "PS"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.01.2" & dat_YS21$species == "broere" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 1,]$Comments <- "dead"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.01.2" & dat_YS21$species == "broere" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 1,]$date_measurement <- "16.08.21"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.01.2" & dat_YS21$species == "broere" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 1,]$functional_group <- "graminoid"

dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.03.2" & dat_YS21$species == "brapin" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 3,]$collector <- "PS"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.03.2" & dat_YS21$species == "brapin" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 3,]$Comments <- "dead"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.03.2" & dat_YS21$species == "brapin" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 3,]$date_measurement <- "16.08.21"
dat_YS21[dat_YS21$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.03.2" & dat_YS21$species == "brapin" & dat_YS21$site == "lo" & dat_YS21$position_ID_original == 3,]$functional_group <- "graminoid"

# delete row with wrong entries
dat_YS21 <-dat_YS21[!c(dat_YS21$position_ID_original == 3 & dat_YS21$block_ID_original == 7 & dat_YS21$plot_ID_original == 1 & dat_YS21$site == "lo" & dat_YS21$species == "broere"),]
dat_YS21 <-dat_YS21[!c(dat_YS21$position_ID_original == 1 & dat_YS21$block_ID_original == 7 & dat_YS21$plot_ID_original == 1 & dat_YS21$site == "lo" & dat_YS21$species == "brapin"),]


# take a look at plameds with inflorescence sizes
inflor_size <- dat_YS21[!is.na(dat_YS21$mean_inflorescence_size), ]
flowers <- dat_YS21[!is.na(dat_YS21$number_flowers), ]

# for all plameds only 1 inflorescence was measured, even if there were multiple flowers/ only for the 1 flowering broere both inflorescences were measured

# check for dublicated unique IDs
any(is.na(dat_YS21$unique_plant_ID)) # none, great

# check levels
dat_YS21_levels <- dat_YS21 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS21_na <- dat_YS21 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 survival check or phenology data)
dat_YS21_na <- dat_YS21 %>% 
  filter(is.na(height_vegetative_str) | is.na(number_leaves)) 

# individuals  where a measurement was forgotten or have other comments than "dead":
# --> other comments: Nes_2.2_3 was completely dry, is dead in spring 2023, Nes_8.1_22 is indeed dead in spring 2022, Cal_1.3_2 had marmot damage in 2021 and a new seedling in spring 2022
# --> veg_height forgotten:
# --> no_leaves forgotten: Cal_8.2_11, Cal_5.2_17, Nes_4.1_30
## veg_height was never forgotten, i.e. if an individual doesn't have veg_height it's dead! (exception: Cal_5.4_7 bromus is just completely eaten, but recovers again later)

# how about missing Leaflength_1 (2 and 3 are possible if plant is dying)
dat_YS21_na <- dat_YS21 %>% 
  filter(is.na(leaf_length1)) 

# all dead plants or plants with comments

# how about missing Leaflength_2 and _3?
dat_YS21_na <- dat_YS21 %>% 
  filter(is.na(leaf_length2) | is.na(leaf_length3)) 

# all NA's match with recorded leaf number (1 or 2), except: Cal_1.1_2 has 4 leaves, but only 2 longest leaves recorded, Cal_4.1_4 has 6 leaves, but only 2 recorded (however, comment is: ll3: eaten & herbivory)


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# add survival: 0 for all species with all NAs except a few that were forgotten or the "wrongly" planted Scacol etc. - plus make sure all dead plants have herbivory NA
dat_YS21 <- dat_YS21 %>%
  mutate(survival = ifelse(is.na(height_vegetative_str) == TRUE & is.na(number_leaves) == TRUE & is.na(height_reproductive_str) == TRUE, 0, 1),
         survival = ifelse(unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1" | unique_plant_ID == "CHE.hi.warm.bare.wf.05.07.1" | unique_plant_ID == "CHE.lo.ambi.bare.wf.02.03.2" , 1, survival)) %>%
  mutate(herbivory = ifelse(survival == 0, NA, herbivory))


# add empty columns
missing_columns <- setdiff(final_columns, colnames(dat_YS21)) # find out which column are missing

dat_YS21[ , missing_columns] <- NA # add those columns filled with NAs
  
# delete all columns not in final data frame plus rearrange in right order
dat_YS21 <- dat_YS21 %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
dat_YS21 <- dat_YS21 %>% 
  mutate(across(c(1:26), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_YS21 <- dat_YS21 %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%y"), 
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))



str(dat_YS21)

# final NA check
dat_YS21_na <- dat_YS21 %>%
  filter(if_any(c(1:7), is.na)) # nicee!


################################################################################
### 2022 #######################################################################
################################################################################


### PREPARATION: COLUMNS, DATA CLASSES #########################################

dat_YS22 <- dat_YS22_org

# check data classes
str(dat_YS22)

# change column names: get column names
dput(colnames(dat_YS22))

# change them to new names
dat_YS22 <- dat_YS22 %>%
  rename("date_measurement" = "Measurement_Date",
         "region" = "Country",
         "site" = "Site",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species",
         "height_vegetative_str" = "Veg_Height",
         "height_reproductive_str" = "Rep_Height",
         "number_leaves" = "Number_Leaves",
         "leaf_length1" = "Leaflength_1",
         "leaf_length2" = "Leaflength_2",
         "leaf_length3" = "Leaflength_3",
         "number_flowers" = "Number_Flowers",
         "collector" = "Collector")

# change variables in columns to standard: replace empty strings with NAs, change site names
dat_YS22 <- dat_YS22 %>%
  mutate(Col_1 = as.integer(Inflorescence_Length_1), # replace empty strings with NA
         Col_2= as.integer(Inflorescence_Length_2),
         Col_3 = as.integer(Inflorescence_Length_3),
         leaf_length3 = as.integer(leaf_length3),
         number_flowers = as.integer(number_flowers),
         #number_leaves = as.integer(number_leaves),
         site = case_when(site == "NES" ~ "lo" , site == "CAL" ~ "hi"))  # change site names

# put all additional inflorescence length in separate columns to take the mean
dat_YS22 <- dat_YS22 %>%
  mutate(many_inflo = case_when(str_detect(Notes, "extra") == TRUE ~ Notes,
                                .default = NA),
         many_inflo = str_replace(many_inflo, ":", ",")) %>%
  separate(many_inflo, into = paste0("Col_", 4:15), sep = ",", fill = "right") %>%
  rowwise() %>%
  mutate_at(paste0("Col_", 1:15), as.integer) %>%
  #mutate(mean_inflorescence_size = mean(c_across(c(paste0("Col_", 1:15))), na.rm = TRUE),
  #       mean_inflorescence_size = ifelse(is.nan(mean_inflorescence_size), NA, mean_inflorescence_size)) %>% # take mean for inflorescence
  #dplyr::select(-paste0("Col_", 1:15)) %>%
  #dplyr::select(-Inflorescence_Length_1, -Inflorescence_Length_2, -Inflorescence_Length_3) %>%
  ungroup()

cols_to_summarize <- dat_YS22[startsWith(names(dat_YS22), "Col_")] # which columns should be randomly sampled?

# randomly sample 3 non-NA columns and take mean (see: https://stackoverflow.com/questions/77961675/rowwise-mean-of-randomly-selected-non-na-columns)
dat_YS22$mean_inflorescence_size <-
  mapply(
    \(x, k) mean(na.omit(c(x))[sample.int(sum(!is.na(x)), k)]),
    asplit(cols_to_summarize, 1),
    pmin(rowSums(!is.na(cols_to_summarize)), 3)
  ) 

dat_YS22 <- dat_YS22 %>%
  dplyr::select(-paste0("Col_", 1:15), -Inflorescence_Length_1, -Inflorescence_Length_2, -Inflorescence_Length_3)


# change species names
dat_YS22 <- dat_YS22 %>%
  mutate(species = ifelse(species %in% names(species_names),  species_names[species], species),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species))


# as some Medlups were seedlings, it was noted if any of the counted leaves was actually a cotyledon: add them to the number of "normal" leaves
dat_YS22 <- dat_YS22 %>%
  mutate(number_leaves = as.character(number_leaves),
         many_leaves = case_when(str_detect(number_leaves, "\\+") == TRUE ~ number_leaves,
                                .default = NA)) %>%
  separate(many_leaves, into = paste0("Col", 1:2), sep = "\\+", fill = "right") %>%
  rowwise() %>%
  mutate_at(paste0("Col", 1:2), as.integer) %>%
  mutate(many_leaves = as.character(sum(c_across(c(paste0("Col", 1:2))), na.rm = TRUE))) %>% # take sum of all the leaves, make character for putting back in column
  mutate(number_leaves = case_when(str_detect(number_leaves, "\\+") == TRUE ~ many_leaves,
                                   .default = number_leaves),
         number_leaves = as.integer(number_leaves)) %>%
  dplyr::select(-paste0("Col", 1:2), -many_leaves) %>%
  ungroup() %>%
  mutate(number_leaves = as.integer(number_leaves))

# add herbivory column (1 = herbivory, 0 = no herbivory)
mypattern2 <- c("little", "lot", "yes", "pathogen", "aphi", "lice", "cut")

dat_YS22 <- dat_YS22 %>%
  mutate(herbivory1 = ifelse(grepl(paste(mypattern2, collapse='|'), Herbivory) == TRUE, 1, 0),
         herbivory2 = ifelse(grepl(paste(mypattern2, collapse='|'), Comments) == TRUE, 1, 0)) %>% # reduce the categories of herbivory (a little, yes, a lot)
  rowwise() %>%
  mutate(herbivory3 = max(herbivory1, herbivory2, na.rm = FALSE),
         herbivory = ifelse(is.na(height_vegetative_str) == TRUE & is.na(number_leaves) == TRUE, NA, herbivory1)) %>% # change the 0's in rows with dead plants (i.e. no vegetative height) to NA
  ungroup()

# Some medlups (34) had new seedlings germinated in 2022 (still seeds from original soil? or from seeds produced in 2021? unclear), but all but 1 were dead again before 2023 peak measurements.
# As this would result in only a single data point (not even a completed season), the data of the seedlings in 2022 is just deleted instead of adding new individuals to the metadata etc.
# BUT: don't delete where the comment says "xy seedlings next to it"

measurement_cols <- c("height_vegetative_str", "height_reproductive_str", "number_leaves", "leaf_length1", "leaf_length2", "leaf_length3", "number_flowers", "mean_inflorescence_size", "herbivory") # define the columns which need to be NA if the plant is a seedling
dat_YS22 <- dat_YS22 %>%
  mutate(across(all_of(measurement_cols), 
                ~ ifelse(grepl("seedling(?! next to )", Comments, ignore.case = TRUE, perl = TRUE), NA, .))) # applies transformation only if "seedling" isn't followed by "next to it/ focal"


# correct collector/ writer: CM/EI was always EI measuring, CM writing, N was Nadine, i.e. NG
dat_YS22 <- dat_YS22 %>%
  mutate(collector = ifelse(grepl("CM/EI", collector) == TRUE, "EI", collector),
         Writer = ifelse(grepl("CM/EI", Writer) == TRUE, "CM", Writer),
         collector = ifelse(grepl("N", collector) == TRUE, "NG", collector),
         Writer = ifelse(grepl("N", Writer) == TRUE, "NG", Writer))


### ADD TREATMENTS ETC. ########################################################

# select oldest ind_number for all positions

meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup()

# exactly 1800, great

# before merging: change species from brapin in Nes_7.1_1 to broere and broere in Nes_7.1_3 to brapin (details about mixup in 2021 cleaning)
dat_YS22[dat_YS22$site == "lo" & dat_YS22$block_ID_original == 7 & dat_YS22$plot_ID_original == 1 & dat_YS22$position_ID_original == 1,]$species <- "broere"
dat_YS22[dat_YS22$site == "lo" & dat_YS22$block_ID_original == 7 & dat_YS22$plot_ID_original == 1 & dat_YS22$position_ID_original == 3,]$species <- "brapin"

# merge treatments to 2022 size data frame
dat_YS22 <- full_join(dat_YS22, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "functional_group"))

# nice!

### PROBLEM FIXING #############################################################

# go through all the comments

# "leaves dry"/ "stem leaves dry"/ "only stem leaves alive"/ "dead leaves": nothing to do about that, so ignore
# "most buds never opened"/ "never set seeds"/ "mainly closed buds"/ "dry bud": ignore - this could potentially lead to an overestimation of reproduction, so keep in mind


# rest of the comments are irrelevant ("broken leaves"/ "seeds collected" etc.)


# Due to excessive re-sprouting after the peak measurement of Cenjac (and because most Cenjac were measured when they were already half dry in August), leaves and flowers were re-counted in 
# September. As the size of 2023 will be better represented by the complete number of leaves and flowers in 2022 rather than the count of the half dry plant at peak measurement, the autumn 
# counts are added to the peak counts.
dat_YS22_CenjacAut <- dat_YS22_CenjacAut_org


dat_YS22_CenjacAut <- dat_YS22_CenjacAut %>%
  rename("date_measurement" = "Measurement_Date",
         "region" = "Country",
         "site" = "Site",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species") %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"),
         species = tolower(species))
  

# merge treatments to Cenjac data frame
dat_YS22_CenjacAut <- left_join(dat_YS22_CenjacAut, meta_plant2022_23[meta_plant2022_23$species == "cenjac",], by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

dat_YS22_CenjacAut <- dat_YS22_CenjacAut %>%
  dplyr::select(unique_plant_ID, Number_NewLeaves, Number_NewSeedFlowers)

dat_YS22 <- left_join(dat_YS22, dat_YS22_CenjacAut, by = "unique_plant_ID") %>%
  rowwise() %>%
  mutate(number_leaves = sum(number_leaves, Number_NewLeaves, na.rm = TRUE),
         number_flowers = sum(number_flowers , Number_NewSeedFlowers, na.rm =  TRUE),
         number_leaves = ifelse(number_leaves == 0 & is.na(height_vegetative_str) == TRUE, NA, number_leaves),
         number_flowers = ifelse(number_flowers == 0, NA, number_flowers)) %>%
  dplyr::select(-Number_NewLeaves, -Number_NewSeedFlowers) %>%
  ungroup()


# check for dublicated unique ID
any(is.na(dat_YS22$unique_plant_ID)) # none, great

# check levels
dat_YS22_levels <- dat_YS22 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS22_na <- dat_YS22 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 data)
dat_YS22_na <- dat_YS22 %>% 
  filter(is.na(height_vegetative_str) | is.na(number_leaves)) 


# a few Daucus and one Hypper have reproductive measurements, but no vegetative height or number of leaves (or both) as they were already dry when measured
# a few Daucars and Hyppers have a vegetative height but no leaf number --> for all of them it's marked that the leaves were completely dry and that veg. height is based on dry stem leaves
# no leaf counts without veg. height, i.e. nothing was forgotten

# do a final check when data from all years is put together to see whether dead individuals stay dead etc.

# how about missing Leaflength_1 (2 and 3 are possible if plant is dying)
dat_YS22_na <- dat_YS22 %>% 
  filter(is.na(leaf_length1)) 

# all dead plants or plants with comments, except Nes_2.2_12, which has dry leaves (counted, but not measured for leaflength)

# how about missing Leaflength_2 and _3?
dat_YS22_na <- dat_YS22 %>% 
  filter(is.na(leaf_length2) | is.na(leaf_length3)) 

# number of leaf length measurements match with number of leaves except for some forgotten leaf_length3 measurements
# forgotten leaf_length3: Cal_9.5_30, Cal_3.1_3, Nes_7.2_13, Nes_4.2_19

### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# add survival: 0 for all species with NA in veg. height AND # leaves AND rep. height (except Cal_1.1_8 as this is still the Brapin which was assumed to be a wrongly planted Scabiosa)
dat_YS22 <- dat_YS22 %>%
  mutate(survival = ifelse(is.na(height_vegetative_str) == TRUE & is.na(number_leaves) == TRUE & is.na(height_reproductive_str) == TRUE, 0, 1),
         survival = ifelse(unique_plant_ID == "CHE.hi.warm.vege.wf.01.08.1", 1, survival)) 

# check columns
str(dat_YS22)

# add empty columns
missing_columns22 <- setdiff(final_columns, colnames(dat_YS22)) # find out which column are missing

dat_YS22[ , missing_columns22] <- NA # add those columns filled with NAs

# delete all columns not in final data frame plus rearrange in right order
dat_YS22 <- dat_YS22 %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
dat_YS22 <- dat_YS22 %>% 
  mutate(across(c(1:26), \(x) str_remove_all(x, pattern = fixed(" "))))


# change data types
dat_YS22 <- dat_YS22 %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"), 
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))



str(dat_YS22)

# final NA check
dat_YS22_na <- dat_YS22 %>%
  filter(if_any(c(1:7), is.na)) # nicee!


################################################################################
### 2023 #######################################################################
################################################################################


### PREPARATION: COLUMNS, DATA CLASSES #########################################

dat_YS23 <- dat_YS23_org

# check data classes
str(dat_YS23)

# change column names: get column names
dput(colnames(dat_YS23))

# change them to new names
dat_YS23 <- dat_YS23 %>%
  rename("date_measurement" = "Measurement_Date",
         "region" = "Country",
         "site" = "Site",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species",
         "height_vegetative_str" = "Veg_HEIght_Stretched",
         "height_vegetative" = "Veg_HEIght_Real",
         "height_reproductive_str" = "Rep_HEIght",
         "number_leaves" = "Number_Leaves",
         "leaf_length1" = "Leaflength_1",
         "leaf_length2" = "Leaflength_2",
         "leaf_length3" = "Leaflength_3",
         "number_flowers" = "Number_Flowers",
         "collector" = "Collector")

# delete empty spaces in various columns (useful: https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r)
dat_YS23 <- dat_YS23 %>% 
  mutate(across(c(1:20, 23:24), \(x) str_remove_all(x, pattern = fixed(" "))))

# now re-assign correct data types
dat_YS23 <- dat_YS23 %>%
  mutate_at(c("block_ID_original", "plot_ID_original", "position_ID_original", "height_vegetative", "leaf_length1", "leaf_length3", "number_leaves", "Inflorescence_Length_1", "Inflorescence_Length_3"), as.integer)


# change variables in columns to standard: replace empty strings with NAs, change site names, take mean of inflorescences
dat_YS23 <- dat_YS23 %>%
  mutate(Col1 = as.integer(Inflorescence_Length_1), # replace empty strings with NA
         Col2 = as.integer(Inflorescence_Length_2),
         Col3 = as.integer(Inflorescence_Length_3),
         leaf_length3 = as.integer(leaf_length3),
         leaf_length2 = as.integer(leaf_length2),
         height_vegetative_real_rosette = as.integer(Veg_HEIght_Real_Rosette),
         site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi")) %>% # change site names
  rowwise() %>%
  mutate(mean_inflorescence_size = mean(c(Col1, Col2, Col3), na.rm = TRUE),
         mean_inflorescence_size = ifelse(is.nan(mean_inflorescence_size), NA, mean_inflorescence_size)) %>%  # take mean for inflorescence
  dplyr::select(-c(Veg_HEIght_Real_Rosette, Inflorescence_Length_1, Inflorescence_Length_2, Inflorescence_Length_3, Col1, Col2, Col3)) %>%
  ungroup()

# change species names
dat_YS23 <- dat_YS23 %>%
  mutate(species = tolower(species),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species))

# add herbivory column (1 = herbivory, 0 = no herbivory)
mypattern3 <- c("little", "lot", "yes", "pathogen", "aphi", "lice", "cut")

dat_YS23 <- dat_YS23 %>%
  mutate(herbivory1 = ifelse(grepl(paste(mypattern3, collapse='|'), Herbivory) == TRUE, 1, 0),
         herbivory2 = ifelse(grepl(paste(mypattern3, collapse='|'), Comments) == TRUE, 1, 0)) %>% # reduce the categories of herbivory (a little, yes, a lot)
  rowwise() %>%
  mutate(herbivory3 = max(herbivory1, herbivory2, na.rm = FALSE),
         herbivory = ifelse(is.na(height_vegetative_str) == TRUE & is.na(number_leaves) == TRUE, NA, herbivory3)) %>% # change the 0's in rows with dead plants (i.e. no vegetative height) to NA
  ungroup()

# fix problem with different flower stages (buds vs. flowers vs. will never flower)
# for now: add up buds and flowers, ignore "will never flower" (this could only lead to overestimation of flowering - check for IPMs)
dat_YS23 <- dat_YS23 %>%
  mutate(many_flowers = case_when(str_detect(number_flowers, "\\+") == TRUE ~ number_flowers, # if there's a + in the flower column, save values in new column
                                 .default = NA)) %>%
  separate(many_flowers, into = paste0("Col", 1:2), sep = "\\+", fill = "right") %>% # separate new column into two
  rowwise() %>%
  mutate_at(paste0("Col", 1:2), as.integer) %>%
  mutate(many_flowers = as.character(sum(c_across(c(paste0("Col", 1:2))), na.rm = TRUE))) %>% # take sum of all the leaves, make character for putting back in column
  mutate(number_flowers = case_when(str_detect(number_flowers, "\\+") == TRUE ~ many_flowers,
                                   .default = number_flowers),
         number_flowers = as.integer(number_flowers)) %>%
  dplyr::select(-paste0("Col", 1:2), -many_flowers) %>%
  mutate(number_flowers = ifelse(number_flowers == 0, NA, number_flowers)) %>%
  ungroup()


# if decided to delete "will never flower" flowers, check whether it needs to be done before merging buds + flowers as sometimes "will never flower" might only apply to buds

### ADD TREATMENTS ETC. ########################################################

# merge treatments to 2023 size data frame
dat_YS23 <- full_join(dat_YS23, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "functional_group"))

# Brapin - Broere mix-up was directly corrected in raw YearlySize of 2023, so perfect match here, but check
dat_YS23[dat_YS23$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.03.2", ] # brapin at position 3, correct
dat_YS23[dat_YS23$unique_plant_ID == "CHE.lo.ambi.bare.wf.07.01.3", ] # broere has now ind_number 3

# nice!

### PROBLEM FIXING #############################################################

# problem with Cal_1.1 8: Should be brapin, but was assumed that a scacol was planted by mistake, so in 2021 this scacol was measured instead --> in 2023 the brapin was measured, all good /but will seem to be dead in 2021/22, so maybe just delete?
a <- dat_YS23[dat_YS23$block_ID_original == 1 & dat_YS23$plot_ID_original == 1 & dat_YS23$position_ID_original == 8 & dat_YS23$site == "hi",]
a$Comments

# go through all the comments

# check whether medlup seedlings from 2022 survived to 2023 (none but 1 out of 34)
medlup22 <- dat_YS22 %>%
  filter(species == "medlup") %>%
  dplyr::select(1, 7, 20) # 36, 37, 20, 14 for 2022 data with no columns deleted # 1, 7, 20 for final 2022 data
medlup23 <- dat_YS23 %>%
  filter(species == "medlup") %>%
  dplyr::select(10, 12, 13, 36, 20, 19)


medlup_comp <- left_join(medlup22, medlup23, by = "unique_plant_ID")

## delete seedling measurements in 2023 (it can be seen that the 2022 seedlings hardly ever survived, so they were deleted - do the same for 2023 (additionally, they would only include 
# 1 single measurement, which is not useful))

measurement_cols <- c("height_vegetative_str", "height_reproductive_str", "height_vegetative", "number_leaves", "leaf_length1", "leaf_length2", "leaf_length3", "number_flowers", "mean_inflorescence_size", "herbivory") # define the columns which need to be NA if the plant is a seedling
seedlings_2023 <- dat_YS23 %>%
  filter(grepl("seedling", Comments))  # save seedling identity to later also delete in biomass 
seedlings_2023$unique_plant_ID 

dat_YS23 <- dat_YS23 %>%
  mutate(across(all_of(measurement_cols), 
                ~ ifelse(grepl("seedling", Comments, ignore.case = TRUE, perl = TRUE), NA, .))) # applies transformation only if "seedling" isn't followed by "next to it/ focal"


# check levels of factors
unique(dat_YS23$collector) # "zl" and empty spaces...

# measurement date for 3 silvuls in veg plot is 17.07., so collector has to be EI and for one bare daucar in block 10 and with measurement date 22.08. EI did the rest of the plot, so has to be EI
# Til did rest of salpras in plot Cal_4.5, so "zl" has to be "TL"
dat_YS23 <- dat_YS23 %>%
  mutate(collector = case_when(collector == "" ~ "EI",
                               collector == "zl" ~ "TL",
                               TRUE ~ collector))

# check other levels
dat_YS23_levels <- dat_YS23 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique)


# check for dublicated unique IDs
any(is.na(dat_YS23$unique_plant_ID)) # none, great



### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS23_na <- dat_YS23 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead)
dat_YS23_na <- dat_YS23 %>% 
  filter(is.na(height_vegetative_str) | is.na(number_leaves)) 

# Cal_8.2, Hypper15: has vegetative measurements, but no number of leaves --> but has comment that it's eaten, might be the reason?
# Nes_5.2 Plamed10 & 29, Nes_6.1 Plamed29, Nes_9.1 Plamed17 have reproductive height but no vegetative measurements --> leaves were completely dry/ dead & biomass wasn't harvested
# rest seems to be dead indeed


# how about missing Leaflength_1 (2 and 3 are possible if plant is dying)
dat_YS23_na <- dat_YS23 %>% 
  filter(is.na(leaf_length1)) 

# all dead plants or plants with comments, except Nes_2.2_12, which has dry leaves (counted, but not measured for leaflength)

# how about missing Leaflength_2 and _3?
dat_YS23_na <- dat_YS23 %>% 
  filter(is.na(leaf_length2) | is.na(leaf_length3)) 

# only one Hypper on Nes which is marked as "eaten" and all the Hypper where leaflength was forgotten...


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# add survival: 0 for all species with NA in veg. height AND # leaves AND rep. height 
# apparently Nes_10.1_10 and Nes_2.2_9 (CHE.lo.ambi.bare.wf.02.09.2) were not dead (collected for biomass, see comment), but not measured, so set survival to 1
dat_YS23 <- dat_YS23 %>%
  mutate(survival = ifelse(is.na(height_vegetative_str) == TRUE & is.na(number_leaves) == TRUE & is.na(height_reproductive_str) == TRUE & unique_plant_ID != "CHE.lo.ambi.bare.wf.02.09.2" & unique_plant_ID != "CHE.lo.ambi.bare.wf.10.10.2", 0, 1))


# add empty columns
missing_columns23 <- setdiff(final_columns, colnames(dat_YS23)) # find out which column are missing

dat_YS23[ , missing_columns23] <- NA # add those columns filled with NAs

# delete all columns not in final data frame plus rearrange in right order
dat_YS23 <- dat_YS23 %>% 
  dplyr::select(any_of(final_columns)) # THIS DELETES THE REAL ROSETTE HEIGHT FOR SCABIOSA!!! --> MAKE DECISION ON WHAT TO USE!

# make sure to delete empty spaces in all columns
dat_YS23 <- dat_YS23 %>% 
  mutate(across(c(1:26), \(x) str_remove_all(x, pattern = fixed(" "))))


# change data types
dat_YS23 <- dat_YS23 %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"), 
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))



str(dat_YS23)


# final NA check
dat_YS23_na <- dat_YS23 %>%
  filter(if_any(c(1:7), is.na)) # nicee!

################################################################################
### 2021 - 2023 ################################################################
################################################################################

dat_YS21_22 <- bind_rows(dat_YS21, dat_YS22)
dat_YS21_22_23 <- bind_rows(dat_YS21_22, dat_YS23)

# re-define data types again...
dat_YS21_22_23 <- dat_YS21_22_23 %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%Y-%m-%d"), 
         date_planting = as.Date(date_planting, format = "%Y-%m-%d"))

str(dat_YS21_22_23)

# check whether there are any mistaken NA in metadata columns
dat_YS21_22_23_na <- dat_YS21_22_23 %>% 
  filter(is.na(unique_plant_ID) | is.na(collector) | is.na(date_measurement) | is.na(species)) # all good

# check for dublicated unique IDs
any(is.na(dat_YS21_22_23$unique_plant_ID)) # none, great

# check levels
dat_YS21_22_23_levels <- dat_YS21_22_23 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### CHECK LOGIC OF FINAL DATA FRAME ############################################

# fix the problem with replaced plants:
# - if plants were replaced after the peak measurement 2021, they will have a different index in 2021 vs. 2022/23 for the peak measurements
# - if there is an individual with a lower index in a position, these 2021 peak measurements should be deleted (basically a whole row should be deleted)
# - instead, a new row with the individual measured in 2022/23 should be added, with all measurements and survival etc. set to NA

# check for how many individuals this is the case
index_problems <- dat_YS21_22_23 %>%
  mutate(unique_position_id = str_sub(unique_plant_ID, 1, 25),
         plant_id = str_sub(unique_plant_ID, start = -1)) %>%
  group_by(unique_position_id) %>%
  filter(n_distinct(plant_id) > 1) %>%
  ungroup() # 36 individuals

# change the lower ids and planting dates to the higher/later, re-create unique_plant_id and set all measurement columns to NA
dat_YS21_22_23 <- dat_YS21_22_23 %>%
  mutate(unique_position_id = str_sub(unique_plant_ID, 1, 25),
         plant_id = str_sub(unique_plant_ID, start = -1)) %>%
  group_by(unique_position_id) %>%
  mutate(min_plant_id = min(as.numeric(plant_id)),
         max_plant_id = max(as.numeric(plant_id)),
         plant_id = if_else(as.numeric(plant_id) == min_plant_id, as.character(max_plant_id), plant_id), # update the plant_id to the higher value
         min_planting = min(date_planting),
         max_planting = max(date_planting),
         date_planting2 = if_else(date_planting == min_planting, max_planting, date_planting),
         unique_plant_ID2 = paste(unique_position_id, plant_id, sep = ".")) %>%
  #filter(unique_plant_ID != unique_plant_ID2) %>%
  mutate(across(6:26, ~ if_else(unique_plant_ID != unique_plant_ID2, NA, .)),
         date_measurement = if_else(unique_plant_ID != unique_plant_ID2, NA, date_measurement)) %>%
  #filter(unique_plant_ID != unique_plant_ID2) %>%
  ungroup() %>%
  dplyr::select(-unique_plant_ID, -unique_position_id, -date_planting, -plant_id, -min_plant_id, -min_planting, -max_plant_id, -max_planting) %>%
  rename("unique_plant_ID" = "unique_plant_ID2", "date_planting" = "date_planting2")


# check whether all dead plants have survival 0 and herbivory NA
surv_check <- dat_YS21_22_23 %>%
  mutate(dead_alive = ifelse(is.na(height_vegetative_str) == TRUE & is.na(height_reproductive_str) == TRUE & is.na(number_leaves) == TRUE, "dead", "alive")) # all good

# define a vector of columns which should be NA if the plant is dead
measurement_cols_final <- c("height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "number_tillers", "vegetative_width", "height_nathan", "stem_diameter", 
                            "leaf_width", "petiole_length", "number_branches", "number_leafclusters",  "number_leaves", "leaf_length1", "leaf_length2", "leaf_length3", "number_flowers", 
                            "mean_inflorescence_size", "herbivory") 

# check whether they are indeed marked as survival = 0
surv_check <- surv_check %>%
  mutate(check_col = ifelse((dead_alive == "dead" & 
                         rowSums(!is.na(select(., all_of(measurement_cols_final)))) == 0) |
                           (dead_alive == "alive" & 
                              rowSums(!is.na(select(., all_of(measurement_cols_final)))) > 0), 
                       "ok", "not ok"))


# all good! (the "not ok" is a Bromus which was completely eaten to the ground in 2021)

# check whether survival makes sense (dead individuals stay dead etc.)

# make wide data frame containing all survival data from all years
surv_check2 <- dat_YS21_22_23 %>%
  mutate(year = as.numeric(as.character(format(date_measurement, "%Y"))),
         year = paste0("year_", year),
         survival = as.numeric(as.character(survival))) %>%
  dplyr::select(unique_plant_ID, species, functional_group, survival, year) %>%
  pivot_wider(names_from = year, values_from = survival)


# check the logic of the time line
illogical_survival_peak <- surv_check2 %>%
  rowwise() %>%
  mutate(across(-c(1,2,3), ~as.numeric(as.character(.x)))) %>%
  mutate(logical = ifelse(any(diff(c_across(-c(1,2,3))) > 0, na.rm = TRUE), 'not ok', 'ok')) %>%
  ungroup()


# only 9 individuals with an illogical time series --> will be corrected retrospectively


# now retrospectively correct survival to 1 for the 9 individuals with illogical time series

# save individuals with illogical time series

illogical2022 <- illogical_survival_peak[illogical_survival_peak$logical == "not ok" & illogical_survival_peak$year_2022 == 0,]$unique_plant_ID

dat_YS21_22_23 <- dat_YS21_22_23 %>%
  mutate(survival = case_when(unique_plant_ID %in% illogical2022 & lubridate::year(date_measurement) == 2022 ~ as.factor(1),
                              TRUE ~ survival))


# check whether there's initial size measurements of all 1836 individuals included in YearlySize
dat_InS <- read_csv( "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Initial Size Measurements/RangeX_clean_InitialSize_2021_CHE.csv")

completness_check <- left_join(illogical_survival_peak, dat_InS, by = "unique_plant_ID")

# there are three individuals with no initial size - check them

meta_plant[meta_plant$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.29.2",]
dat_InS[dat_InS$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.29.2",] # in initial size, but all NA --> checked data sheets, was simply forgotten


meta_plant[meta_plant$unique_plant_ID == "CHE.hi.warm.bare.wf.02.08.1",]
dat_InS[dat_InS$unique_plant_ID == "CHE.hi.warm.bare.wf.02.08.1",] # in initial size, but all NA --> checked data sheets, was simply forgotten


meta_plant[meta_plant$unique_plant_ID == "CHE.hi.ambi.bare.wf.02.18.1",]
dat_InS[dat_InS$unique_plant_ID == "CHE.hi.ambi.bare.wf.02.18.1",] # in initial size, but all NA --> checked data sheets, was simply forgotten

# nice!

# check whether all individuals with a leaf number also have a vegetative height
dat_YS21_22_23_na <- dat_YS21_22_23 %>%
  filter(is.na(height_vegetative_str) & !is.na(number_leaves))

# not the case for one plamed (CHE.hi.ambi.bare.wf.10.10.1) --> can corrected by using length of longest leaf as height_vegetative_str (practically the same)
dat_YS21_22_23[dat_YS21_22_23$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.10.1" & dat_YS21_22_23$date_measurement == "2023-08-30", ]$height_vegetative_str <- 
  dat_YS21_22_23[dat_YS21_22_23$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.10.1" & dat_YS21_22_23$date_measurement == "2023-08-30", ]$leaf_length1

# check the logic of inflorescence length: should only be measured for daucar, brapin, salpra and plamed
dat_YS21_22_23 %>%
  group_by(species) %>%
  filter(!is.na(mean_inflorescence_size)) %>%
  summarize(mean_inflorescence_size = n())

# there's 6 broere and 4 silvul measurements - delete them
dat_YS21_22_23 <- dat_YS21_22_23 %>%
  mutate(mean_inflorescence_size = ifelse(species %in% c("broere", "silvul"), NA, mean_inflorescence_size))

dat_YS21_22_23 %>%
  group_by(species) %>%
  filter(!is.na(mean_inflorescence_size)) %>%
  summarize(mean_inflorescence_size = n()) # fixed

# re-arrange column order
dat_YS21_22_23 <- dat_YS21_22_23 %>%
  dplyr::select(all_of(final_columns))

### CONTROL PLOTTING AND MODELLING #############################################


dat_YS21_22_23_plot <- dat_YS21_22_23 %>% 
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16),
         site = str_sub(unique_plant_ID, 5, 6))

dat_YS21_22_23_plot <- dat_YS21_22_23_plot %>%
  pivot_longer(cols = 8:25, names_to = "variable", values_to = "values")

ggplot(data = dat_YS21_22_23_plot[dat_YS21_22_23_plot$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = dat_YS21_22_23_plot[dat_YS21_22_23_plot$site == "hi",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = dat_YS21_22_23_plot, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = dat_YS21_22_23_plot[dat_YS21_22_23_plot$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_boxplot() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




### SAVE CLEAN VERSION #########################################################


write.csv(dat_YS21_22_23, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Yearly Size Measurements/RangeX_clean_YearlyDemographics_2021_2023_CHE.csv",
          row.names = FALSE)



