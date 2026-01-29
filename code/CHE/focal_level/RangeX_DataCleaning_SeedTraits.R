################################################################################
### DATA CLEANING SCRIPT: SEEDS ################################################
################################################################################

################################################################################

### Data used           : RangeX_raw_SeedWeight_2022.csv, RangeX_raw_SeedNumber_2022.csv, RangeX_clean_MetadataFocal_CHE.csv
### Purpose             : Clean and combine the two raw data files containing seed number and seed weight of seeds collected in 2022.

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex


### LOAD DATA SET ##############################################################

# load data
#number_org <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Seeds/RangeX_raw_SeedNumber_2022.csv")
#weight_org <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Seeds/RangeX_raw_SeedWeight_2022.csv")

number_org <- read_csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Seeds/RangeX_raw_SeedNumber_2022.csv")
weight_org <- read_csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Seeds/RangeX_raw_SeedWeight_2022.csv")

# load treatment key
#meta_plant <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")
meta_plant <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/1_Metadata/2_Metadata_FocalsPlots/RangeX_clean_MetadataFocal_CHE.csv")


# wanted columns & data types
final_columns <- c("unique_plant_ID", "species", "date_collection", "counter", "inflorescence_size", "no_seeds", "seedweight")

numeric_cols <- c("inflorescence_size", "no_seeds", "seedweight")
string_cols <- c("unique_plant_ID", "species", "counter")
date_cols <- c("date_collection")


################################################################################
### SEED NUMBER ################################################################
################################################################################

### PREPARATION: COLUMNS, DATA CLASSES #########################################

number <- number_org 

# check data classes
str(number)

# change column names: get column names
dput(colnames(number))

# change them to new names
number <- number %>%
  rename("site" = "Site",
         "species" = "Species",
         "block_ID_original" = "Block",
         "plot_ID_original" = "Plot",
         "position_ID_original" = "Position",
         "region" = "Country",
         "date_collection" = "Sampling_Date",
         "inflorescence_size" = "Flower_Size [mm]",
         "no_seeds" = "No_Seeds",
         "counter" = "Counter")

# change variables in columns to standard: change species names to all lower, change site names, divide seed number by number of seedheads
number <- number %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names
         species = tolower(species),
         no_seeds = round(no_seeds/No_Seedheads, digits = 0))


### ADD TREATMENTS ETC. ########################################################

# select oldest ind_number for all positions
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup() # exactly 1800, nice

# merge treatments to 2022 size data frame
number_merged <- left_join(number, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# there's 9 rows only in number...



### PROBLEM FIXING #############################################################

# find which columns don't have a match in metadata
missing_meta <- number_merged %>%
  filter(is.na(unique_plant_ID))


# cenjac 19: misspelled from cenjac 14 (bags doublechecked, all are alive and flowering in 2022)
number <- number %>%
  mutate(position_ID_original = ifelse(species == "cenjac" & position_ID_original == 19, 14, position_ID_original)) # 3 values changed, great

# medlup in hi 3.6       
meta_plant[meta_plant$site == "hi" & meta_plant$species == "medlup" & meta_plant$block_ID_original == 3,] # no plot 6 in block 3
weight_org[weight_org$Site == "Cal" & weight_org$Species == "Medlup" & weight_org$Block_Number == 3,] # also wrong in weight data frame --> written wrongly on bag
number_org[number_org$Site == "Cal" & number_org$Species == "Medlup" & number_org$Plot == 6,] # only hi X.6 plot with both medlup 21 & 27 flowering is 9.6 and those are not collected in number_org
# assume it's a miss-spelling on bag and it should be 9.6 (cannot be another plot in block 3 as none of them have both medlup21 and medlup27 flowering)
number <- number %>%
  mutate(block_ID_original = ifelse(species == "medlup" & site == "hi" & block_ID_original == 3 & plot_ID_original == 6, 9, block_ID_original)) # 2 values changed, great

# hi 2.2 plamed 17
meta_plant[meta_plant$site == "hi" & meta_plant$species == "plamed" & meta_plant$block_ID_original == 2,] # no plot 2 in block 2
weight_org[weight_org$Site == "Nes" & weight_org$Species == "Plamed" & weight_org$Block_Number == 2,] # no plot 2.2 in original weight data frame for Cal --> but for Nes!
number_org[number_org$Site == "Cal" & number_org$Species == "Plamed" & number_org$Block == 2,] # should be Nes instead of Cal (checked on bag)
number <- number %>%
  mutate(site = ifelse(species == "plamed" & site == "hi" & block_ID_original == 2 & plot_ID_original == 2 & position_ID_original == 17, "lo", site)) # 1 value changed

# lo 10.1 salpra 11 --> there is no salpra 11
weight_org[weight_org$Site == "Nes" & weight_org$Species == "Salpra" & weight_org$Block_Number == 10 & weight_org$Plot_Number == 1,] # also wrong in weight data frame, must be wrong on bag
number_org[number_org$Site == "Nes" & number_org$Species == "Salpra",] # all 3 salpra's in lo 10.1 flowered in 2022 --> change to salpra19, more likely miss-spelling
number <- number %>%
  mutate(position_ID_original = ifelse(species == "salpra" & site == "lo" & block_ID_original == 10 & position_ID_original == 11, 19, position_ID_original)) # 1 value changed, great

# lo 4.1 silvul 19 --> there is no silvul 19
weight_org[weight_org$Site == "Nes" & weight_org$Species == "Silvul" & weight_org$Block_Number == 4 & weight_org$Plot_Number == 1,] # also wrong in weight data frame, must be wrong on bag
number_org[number_org$Site == "Nes" & number_org$Species == "Silvul" & number_org$Block == 4 & number_org$Plot == 1,] # only silvul20 flowered in lo 4.1 in 2022 --> change position ID
number <- number %>%
  mutate(position_ID_original = ifelse(species == "silvul" & site == "lo" & block_ID_original == 4 & position_ID_original == 19, 20, position_ID_original)) # 1 value changed, great

# hi 8.5 silvul 12 --> there is no silvul 12
weight_org[weight_org$Site == "Cal" & weight_org$Species == "Silvul" & weight_org$Block_Number == 8 & weight_org$Plot_Number == 5,] # is from Denmark's counting --> delete as bag can't be checked and all silvuls in hi 8.5 are flowering
number <- number %>%
  filter(!(site == "hi" & species == "silvul" & block_ID_original == 8 & plot_ID_original == 5 & position_ID_original == 12)) # 1 row deleted


# re-merge to see if there's still problems
number_merged <- left_join(number, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# all good now
number <- number_merged


# now go through all the comments

unique(number$Comments)
unique(number[number$species == "silvul", ]$Comments)
silvul <- number[number$species == "silvul",]

# black seed: ignore, virus infected grasses
# flower size measured when dry: ignore for now
# underestimation of size: delete flower size measurement
# seeds are green: ok, mostly medlup collected slightly too early, doesn't matter for number
# xy very small seeds not included/ more seeds which are very small/tiny seeds not included/xy more seeds looking aborted etc.: ignore, don't add to seed number (prob. not viable)
# xy small seeds (in Denmark's counting): ignore, all seeds will be kept for all species
# already open: delete row

number <- number %>%
  filter(grepl("already open", Comments) == FALSE) %>%
  mutate(inflorescence_size = ifelse(grepl("underestimation of size", Comments) == TRUE, NA, inflorescence_size))


# check for dublicated unique ID
dupli <- number %>% 
  get_dupes(unique_plant_ID) # mainly camera silvuls

# check levels
number_levels <- number %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
number_na <- number %>% 
  filter(is.na(unique_plant_ID) | is.na(date_collection) | is.na(species)) # lo 8.1 salpra 19 is missing collection date --> add collection date of other lo salpra's of close blocks (i.e. 9)

unique(number[number$species == "salpra" & number$site == "lo" & number$block_ID_original == 9,]$date_collection)
number <- number %>%
  mutate(date_collection = ifelse(is.na(date_collection) & species == "salpra", "23.06.2024", date_collection)) # changed 1 value

# re-check whether there are any mistaken NA in metadata columns
number_na <- number %>% 
  filter(is.na(unique_plant_ID) | is.na(date_collection) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in no_seeds
number_na <- number %>% 
  filter(is.na(no_seeds)) # none


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# check columns
str(number)

# delete all columns not in final data frame plus rearrange in right order
number_ready <- number %>% 
  dplyr::select(any_of(final_columns), flower_id)


# final NA check
number_na <- number_ready %>%
  filter(if_any(c(1:4, 6), is.na)) # none


################################################################################
### SEED WEIGHT ################################################################
################################################################################

### PREPARATION: COLUMNS, DATA CLASSES #########################################

weight <- weight_org 

# check data classes
str(weight)

# change column names: get column names
dput(colnames(weight))

# change them to new names
weight <- weight %>%
  rename("site" = "Site",
         "species" = "Species",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "region" = "Country")

# change variables in columns to standard: change species names to all lower, change site names, divide seed number by number of seedheads
weight <- weight %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names
         species = tolower(species))

### ADD TREATMENTS ETC. ########################################################

# merge treatments to 2022 size data frame
weight_merged <- left_join(weight, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# there's 12 rows only in weight...

### PROBLEM FIXING #############################################################

# find which columns don't have a match in metadata
missing_meta <- weight_merged %>%
  filter(is.na(unique_plant_ID))

# typos in species - change sapra to salpra
unique(weight$species)
weight <- weight %>%
  mutate(species = ifelse(species == "sapra", "salpra", species)) # 2 values changed

# cenjac 19, medlup in hi 3.6, lo 10.1 salpra 11, lo 4.1 silvul 19, hi 8.5 silvul 12, hi 9.1 silvul 26: identical to errors in number data frame
weight <- weight %>%
  mutate(position_ID_original = ifelse(species == "cenjac" & position_ID_original == 19, 14, position_ID_original),
         block_ID_original = ifelse(species == "medlup" & site == "hi" & block_ID_original == 3 & plot_ID_original == 6, 9, block_ID_original),
         position_ID_original = ifelse(species == "salpra" & site == "lo" & block_ID_original == 10 & position_ID_original == 11, 19, position_ID_original),
         position_ID_original = ifelse(species == "silvul" & site == "lo" & block_ID_original == 4 & position_ID_original == 19, 20, position_ID_original)) %>%
  filter(!(site == "hi" & species == "silvul" & block_ID_original == 8 & plot_ID_original == 5 & position_ID_original == 12)) # ,
         #!(site == "hi" & species == "silvul" & block_ID_original == 9 & plot_ID_original == 1 & position_ID_original == 26))

# hi 7.1 hypper 10 --> there is no hyper 10
number_org[number_org$Species == "Hypper" & number_org$Site == "Cal" & number_org$Block == 7,]
weight_org[weight_org$Species == "Hypper" & weight_org$Site == "Cal" & weight_org$Block_Number == 7,] # must be Cal 7.1 Hypper 18 with 11 seeds in number if TL only weighted 10!
weight <- weight %>%
  mutate(position_ID_original = ifelse(species == "hypper" & site == "hi" & block_ID_original == 7 & position_ID_original == 10, 18, position_ID_original))

# hi 1.9 plamed 10 --> there is no plot 9 in block 1
number_org[number_org$Species == "Plamed" & number_org$Site == "Cal" & number_org$Block == 1,]
weight_org[weight_org$Species == "Plamed" & weight_org$Site == "Cal" & weight_org$Block_Number == 1,] # must be Cal 1.3 plamed 10 as this plamed 10 is missing in weight compared to number
weight <- weight %>%
  mutate(plot_ID_original = ifelse(species == "plamed" & site == "hi" & block_ID_original == 1 & plot_ID_original == 9, 3, plot_ID_original))

# re-merge to see if there's still problems
weight_merged <- left_join(weight, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# all good now
weight <- weight_merged


# now go through all the comments

unique(weight$Comments)

# black seed: ignore, virus infected grasses
# lost some seeds: doesn't matter for weight, ignore
# grosse Samen ausgesucht und ergänzt: ok, ignore
# tiny seeds: ignore
# no seeds in bad/ empty flower/ etc.: ok, ignore
# nur grosse wurden gewogen: not ideal, but cannot be fixed
# already open: delete row

weight <- weight %>%
  filter(grepl("already open", Comments) == FALSE) 


# check for dublicated unique ID
dupli <- weight %>% 
  get_dupes(unique_plant_ID) # mainly camera silvuls

# check levels
weight_levels <- weight %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
weight_na <- weight %>% 
  filter(is.na(unique_plant_ID) | is.na(species)) # all removed


# check for NAs: filter all rows with NA's in No_Seeds_Weighted and Total_Seedweight
weight_na <- weight %>% 
  filter(is.na(No_Seeds_Weighted) | is.na(Total_Seedweight)) # all the individuals with no seeds in bag according to TL --> double-check with LS counting later...


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# check columns
str(weight)

# delete all columns not in final data frame plus rearrange in right order
weight_ready <- weight %>% 
  dplyr::select(any_of(final_columns), No_Seeds_Weighted, Total_Seedweight, flower_id)


# final NA check
weight_na <- weight_ready %>%
  filter(if_any(c(1:4), is.na)) # 43 rows --> bags with 0 seeds have NA as seedweight


################################################################################
### COMBINE ####################################################################
################################################################################


# merge seed number and weight
seed_combined <- full_join(weight_ready, number_ready, by = c("unique_plant_ID", "species", "flower_id"))

# 4 rows only in number, 3 rows only in weight

# rows only in weight
anti_join(weight_ready, number_ready)

# rows only in number
anti_join(number_ready, weight_ready)

# the 4 mismatched rows are all plamed
# CHE.hi.warm.bare.wf.06.17.1
weight[weight$unique_plant_ID == "CHE.lo.ambi.vege.wf.06.17.1",] # checked on bag, should be hi --> correct in weight
number[number$unique_plant_ID == "CHE.hi.warm.bare.wf.06.17.1",]

weight_ready[weight_ready$unique_plant_ID == "CHE.lo.ambi.vege.wf.06.17.1",]$unique_plant_ID <- "CHE.hi.warm.bare.wf.06.17.1"

# CHE.lo.ambi.bare.wf.03.29.1
weight[weight$unique_plant_ID == "CHE.lo.ambi.bare.wf.03.29.1",] # checked on bag, should be lo --> correct in number 
number[number$unique_plant_ID == "CHE.hi.warm.bare.wf.03.29.1",]

number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.03.29.1",]$unique_plant_ID <- "CHE.lo.ambi.bare.wf.03.29.1"

# CHE.lo.ambi.bare.wf.01.17.1
weight[weight$unique_plant_ID == "CHE.lo.ambi.bare.wf.01.17.1",] # checked on bag, should be lo --> correct in number
number[number$unique_plant_ID == "CHE.hi.warm.vege.wf.01.17.1",]

number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.vege.wf.01.17.1",]$unique_plant_ID <- "CHE.lo.ambi.bare.wf.01.17.1"

# CHE.hi.warm.bare.wf.09.29.1
number[number$unique_plant_ID == "CHE.hi.warm.bare.wf.09.29.1",]
weight_org[weight_org$Block_Number == 9 & weight_org$Plot_Number == 1 & weight_org$Species == "Plamed",] # 2 identical plamed29 in Nes 9.1 --> the one with 0 seeds should be Cal (LS also counted 0 seeds)
number_org[number_org$Block == 9 & number_org$Plot == 1 & number_org$Species == "Plamed",]
number[number$block_ID_original == 9 & number$plot_ID_original == 1 & number$species == "plamed",c(1:10, 23)] # use CHE.hi.warm.bare.wf.09.29.1 for the plamed 29 in Nes 9.1 (in weight) with 0 seeds

weight_ready[weight_ready$unique_plant_ID == "CHE.lo.ambi.bare.wf.09.29.2", ]
number_ready[number_ready$unique_plant_ID == "CHE.lo.ambi.bare.wf.09.29.2", ]
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.09.29.1", ]

weight_ready[weight_ready$unique_plant_ID == "CHE.lo.ambi.bare.wf.09.29.2" & weight_ready$No_Seeds_Weighted == 0,]$unique_plant_ID <- "CHE.hi.warm.bare.wf.09.29.1"

# re-check
# rows only in weight
anti_join(weight_ready, number_ready) # good!

# rows only in number
anti_join(number_ready, weight_ready) # good! --> but how is it that weight_ready has a row more --> there must be duplicates

# merge seed number and weight
seed_combined <- full_join(weight_ready, number_ready, by = c("unique_plant_ID", "species", "flower_id"))

dupli <- seed_combined %>% 
  get_dupes(unique_plant_ID, flower_id)

# disentangle duplicates (sometimes several flowers per individual were collected if not enough individuals per species & treatment were flowering)

# CHE.hi.ambi.bare.wf.01.18.1: flower_id 1 for 47 counted/47 weighted, id 2 for 59 counted/60 weighted
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.01.18.1" & weight_ready$No_Seeds_Weighted == 47, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.01.18.1" & number_ready$no_seeds == 47, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.01.18.1" & weight_ready$No_Seeds_Weighted == 31, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.01.18.1" & number_ready$no_seeds == 36, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.04.18.2: flower_id 1 for 26 counted/26 weighted, id 2 for 36 counted/31 weighted
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.04.18.2" & weight_ready$No_Seeds_Weighted == 26, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.04.18.2" & number_ready$no_seeds == 26, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.04.18.2" & weight_ready$No_Seeds_Weighted == 60, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.04.18.2" & number_ready$no_seeds == 59, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.05.24.1: flower_id 1 for 68 counted/68 weighted, id 2 for 36 counted/31 weighted
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.05.24.1" & weight_ready$No_Seeds_Weighted == 68, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.05.24.1" & number_ready$no_seeds == 68, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.05.24.1" & weight_ready$No_Seeds_Weighted == 67, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.05.24.1" & number_ready$no_seeds == 50, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.06.15.1: flower_id 1 for 0 counted/0 weighted, id 2 for 9 counted/9 weighted
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.06.15.1" & weight_ready$No_Seeds_Weighted == 0, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.06.15.1" & number_ready$no_seeds == 0, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.06.15.1" & weight_ready$No_Seeds_Weighted == 9, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.06.15.1" & number_ready$no_seeds == 9, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.08.15.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.15.1" & weight_ready$No_Seeds_Weighted == 36, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.15.1" & number_ready$no_seeds == 46, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.15.1" & weight_ready$No_Seeds_Weighted == 60, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.15.1" & number_ready$no_seeds == 60, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.08.18.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.18.1" & weight_ready$No_Seeds_Weighted == 0, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.18.1" & number_ready$no_seeds == 0, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.18.1" & weight_ready$No_Seeds_Weighted == 63, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.08.18.1" & number_ready$no_seeds == 63, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.09.15.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.09.15.1" & weight_ready$No_Seeds_Weighted == 3, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.09.15.1" & number_ready$no_seeds == 5, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.09.15.1" & weight_ready$No_Seeds_Weighted == 16, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.09.15.1" & number_ready$no_seeds == 24, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.10.05.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.05.1" & weight_ready$No_Seeds_Weighted == 34, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.05.1" & number_ready$no_seeds == 34, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.05.1" & weight_ready$No_Seeds_Weighted == 37, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.10.05.1" & number_ready$no_seeds == 35, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.01.18.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.01.18.1" & weight_ready$No_Seeds_Weighted == 32, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.01.18.1" & number_ready$no_seeds == 32, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.01.18.1" & weight_ready$No_Seeds_Weighted == 67, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.01.18.1" & number_ready$no_seeds == 67, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.02.05.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.02.05.1" & weight_ready$No_Seeds_Weighted == 35, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.02.05.1" & number_ready$no_seeds == 35, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.02.05.1" & weight_ready$No_Seeds_Weighted == 46, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.02.05.1" & number_ready$no_seeds == 45, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.04.05.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.04.05.1" & weight_ready$No_Seeds_Weighted == 0, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.04.05.1" & number_ready$no_seeds == 0, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.04.05.1" & weight_ready$No_Seeds_Weighted == 56, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.04.05.1" & number_ready$no_seeds == 61, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.05.18.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.05.18.1" & weight_ready$No_Seeds_Weighted == 23, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.05.18.1" & number_ready$no_seeds == 23, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.05.18.1" & weight_ready$No_Seeds_Weighted == 7, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.05.18.1" & number_ready$no_seeds == 9, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.06.15.11
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.06.15.1" & weight_ready$No_Seeds_Weighted == 38, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.06.15.1" & number_ready$no_seeds == 58, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.06.15.1" & weight_ready$No_Seeds_Weighted == 37, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.06.15.1" & number_ready$no_seeds == 37, ]$flower_id <- 2

# CHE.hi.warm.bare.wf.07.18.1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.07.18.1" & weight_ready$No_Seeds_Weighted == 36, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.07.18.1" & number_ready$no_seeds == 36, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.07.18.1" & weight_ready$No_Seeds_Weighted == 10, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.warm.bare.wf.07.18.1" & number_ready$no_seeds == 11, ]$flower_id <- 2

# CHE.hi.ambi.bare.wf.03.05.1: re-weighted for correct assignment
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.03.05.1" & weight_ready$Total_Seedweight == 0.00052, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.03.05.1" & number_ready$no_seeds == 30, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.03.05.1" & weight_ready$Total_Seedweight == 0.00100, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.hi.ambi.bare.wf.03.05.1" & number_ready$no_seeds == 12, ]$flower_id <- 2

# CHE.lo.ambi.vege.wf.02.02.1: 490/0.029g = 1, 141/0.020g = 2 (re-weighted for correct assignment)
weight_ready[weight_ready$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.02.1" & weight_ready$Total_Seedweight == 0.02938, ]$flower_id <- 1
number_ready[number_ready$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.02.1" & number_ready$no_seeds == 490, ]$flower_id <- 1
weight_ready[weight_ready$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.02.1" & weight_ready$Total_Seedweight == 0.02003, ]$flower_id <- 2
number_ready[number_ready$unique_plant_ID == "CHE.lo.ambi.vege.wf.02.02.1" & number_ready$no_seeds == 141, ]$flower_id <- 2

# CHE.lo.ambi.vege.wf.03.16.1: infected seed was weighted separately as well --> delete row with 1 seed weighted
weight_ready <- weight_ready %>%
  filter(!(unique_plant_ID == "CHE.lo.ambi.vege.wf.03.16.1" & No_Seeds_Weighted == 1))


# re-merge seed number and weight
seed_combined <- full_join(weight_ready, number_ready, by = c("unique_plant_ID", "species", "flower_id"))

dupli <- seed_combined %>% 
  get_dupes(unique_plant_ID, flower_id) # none!

# for weight of a single seed: use all the seeds counted for Denmark (including the small ones as this is what TL did for the rest of the samples as well)

# 1. if number of seeds is bigger in weight than in number (i.e. TL weighted more than LS counted as she considered only the seeds deemed viable), use the bigger number as no_seeds
# 2. then calculate weight of 1 seed
seed_combined <- seed_combined %>%
  rowwise() %>%
  mutate(no_seeds_weight_calculation = ifelse(no_seeds < No_Seeds_Weighted, No_Seeds_Weighted, no_seeds),
         seedweight = Total_Seedweight/no_seeds_weight_calculation)

# done

### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################


# delete all columns not in final data frame plus rearrange in right order
seed_combined <- seed_combined %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
seed_combined <- seed_combined %>% 
  mutate(across(c(1:6), \(x) str_remove_all(x, pattern = fixed(" "))))

# change typo in date of CHE.lo.ambi.bare.wf.08.19.1 (2022, not 2024)
seed_combined[seed_combined$unique_plant_ID == "CHE.lo.ambi.bare.wf.08.19.1", ]$date_collection <- "23.06.2022"
  
# change data types
seed_combined <- seed_combined %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_collection = as.Date(date_collection, format = "%d.%m.%Y")) # no new NA's, great



# final NA check
seed_combined_na <- seed_combined %>%
  filter(if_any(c(1:4, 6, 7), is.na)) # 43 rows --> bags with 0 seeds have NA as seedweight


### CONTROL PLOTTING AND MODELLING #############################################

seed_combined_plot <- seed_combined %>% 
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16),
         site = str_sub(unique_plant_ID, 5, 6))

seed_combined_plot <- seed_combined_plot %>%
  pivot_longer(cols = 6:7, names_to = "variable", values_to = "values")

ggplot(data = seed_combined_plot[seed_combined_plot$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_combined_plot[seed_combined_plot$site == "hi",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_combined_plot, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = seed_combined_plot[seed_combined_plot$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_boxplot() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = seed_combined_plot[seed_combined_plot$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### SAVE CLEAN VERSION #########################################################

#write.csv(seed_combined, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Seeds/RangeX_clean_SeedTraits_2022_CHE.csv",
#          row.names = FALSE)
write.csv(seed_combined, "/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/6_DataClean/RangeX_clean_SeedTraits_2022_CHE.csv",
          row.names = FALSE)
