################################################################################
### DATA CLEANING SCRIPT: BIOMASS FOCALS #######################################
################################################################################

################################################################################

### Data used           : RangeX_raw_Biomass_2023.csv, RangeX_cleanMetadataFocal_CHE.csv
### Date last modified  : 22.02.2024
### Purpose             : Clean the raw biomass data file (Missing entries? Missing values? Impluasible values? Wrong column names? Data classes defined?)

################################################################################

#rm(list = ls())

### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidyverse); library(tidylog) # data manipulation
library(janitor) # data wrangling
library(ggplot2) # test-plotting
library(stringr) # working with regex

# task-specific packages 

### LOAD DATA SET ##############################################################

# load biomass data
dat_bio23 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Biomass/Biomass Focals/RangeX_raw_BiomassDryweight_2023.csv")
dat_bio23_org <- dat_bio23

# load treatment key
meta_plant <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")

# define useful vector
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")

# wanted columns & data types
final_columns <- c("unique_plant_ID", "species", "date_collection", "dry_mass")

date_cols <- c("date_collection")
numeric_cols <- c("dry_mass")
string_cols <- c("unique_plant_ID", "species")




### PREPARATION: COLUMNS, DATA CLASSES #########################################

# check data classes
str(dat_bio23)

# change column names: get column names
dput(colnames(dat_bio23))

# change them to new names
dat_bio23 <- dat_bio23 %>%
  rename("date_collection" = "Collection_Date",
         "region" = "Country",
         "site" = "Site",
         "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species",
         "dry_mass" = "Dryweight")

# change variables in columns to standard, delete the many completely NA rows (should only be 1389 rows with data)
dat_bio23 <- dat_bio23 %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names
         species = str_trim(species), # get rid of whitespace
         species = tolower(species)) %>%
  janitor::remove_empty(which = "rows")




### ADD TREATMENTS ETC. ########################################################

# prepare treatment key
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup()


# merge treatments to data frame
dat_bio23 <- full_join(dat_bio23, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))



### PROBLEM FIXING #############################################################


# why are there 1801 rows? Check for duplicated unique ID's.
dat_bio23$unique_plant_ID[duplicated(dat_bio23$unique_plant_ID)] # "CHE.lo.ambi.vege.wf.08.04.1"
check <- dat_bio23[dat_bio23$unique_plant_ID == "CHE.lo.ambi.vege.wf.08.04.1", ]
check2 <- dat_bio23_org[dat_bio23_org$Block_Number == 8 & dat_bio23_org$Plot_Number == 2 & dat_bio23_org$Position == 4 & dat_bio23_org$Site == "Nes", ] %>% janitor::remove_empty(which = "rows")

# looks like the seeds were weighted separately, just merge the two weights together
dat_bio23[dat_bio23$unique_plant_ID == "CHE.lo.ambi.vege.wf.08.04.1" & dat_bio23$Weighing_Date == "18.09.2023",]$dry_mass <- sum(dat_bio23[dat_bio23$unique_plant_ID == "CHE.lo.ambi.vege.wf.08.04.1",]$dry_mass)
dat_bio23 <- dat_bio23 %>%
  filter(!(unique_plant_ID == "CHE.lo.ambi.vege.wf.08.04.1" & Weighing_Date == "20.09.2023")) # keep the one row without comment (lines with commnets will be set to NA later), but change dry_mass to sum of the two measurements
dat_bio23$unique_plant_ID[duplicated(dat_bio23$unique_plant_ID)] # no more duplicates

# delete rows which were added by full_join to metadata and don't have any dry_mass data
dat_bio23 <- dat_bio23 %>%
  filter(!(is.na(dry_mass) == TRUE & is.na(date_collection) == TRUE & is.na(Weighing_Date) == TRUE & is.na(Comments) == TRUE)) # 1388 rows, correct (would be 1389, minus duplcated ID)

# delete biomass weights of medlup & daucar seedlings (size measurements were also deleted in YearlySize)
seedlings_2023 <- c("CHE.hi.ambi.vege.wf.09.27.1", "CHE.lo.ambi.bare.wf.01.09.2", "CHE.lo.ambi.vege.wf.02.09.1", "CHE.lo.ambi.vege.wf.03.21.1", 
                           "CHE.lo.ambi.bare.wf.03.27.2", "CHE.lo.ambi.bare.wf.06.27.2", "CHE.lo.ambi.vege.wf.07.21.2", "CHE.lo.ambi.vege.wf.09.21.1", 
                           "CHE.lo.ambi.bare.wf.10.09.1", "CHE.hi.ambi.vege.wf.01.13.3", "CHE.hi.ambi.vege.wf.06.12.1", "CHE.hi.ambi.vege.wf.10.13.1")

dat_bio23 <- dat_bio23 %>%
  filter(!(unique_plant_ID %in% seedlings_2023))

# one date has 2024 instead of 2023 (CHE.hi.warm.bare.wf.02.09.1)
dat_bio23[dat_bio23$unique_plant_ID == "CHE.hi.warm.bare.wf.02.09.1",]
dat_bio23[dat_bio23$unique_plant_ID == "CHE.hi.warm.bare.wf.02.09.1",]$date_collection <- "14.08.2023"
  

### MISSING ENTRIES/ VALUES/ NA's ##############################################

# set dry_mass of all columns with comment and no "ok" to NA ("Bag is missing", "Seed" etc.) (all commented rows have been checked before and commented with ok when problem was solved)
dat_bio23 <- dat_bio23 %>%
  mutate(dry_mass = ifelse(grepl("[[:lower:]]", Comments) == TRUE & grepl("ok", Comments) == FALSE, NA, dry_mass))

# check whether there are any mistaken NA in metadata columns
dat_bio23_na <- dat_bio23 %>% 
  filter(is.na(unique_plant_ID) | is.na(date_collection) | is.na(species)) # 3 rows with no collection date as bags missing --> deduce from collection

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 survival check or phenology data)
dat_bio23_na <- dat_bio23 %>% 
  filter(is.na(dry_mass)) # all rows with either bags missing or only seeds in bag

### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# delete all columns not in final data frame plus rearrange in right order
dat_bio23 <- dat_bio23 %>%
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
dat_bio23 <- dat_bio23 %>%
  mutate(across(c(1:4), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_bio23 <- dat_bio23 %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_collection = as.Date(date_collection, format = "%d.%m.%Y"))


### CONTROL PLOTTING AND MODELLING #############################################

dat_bio23_plot <- dat_bio23 %>%
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16),
         site = str_sub(unique_plant_ID, 5, 6))

ggplot(data = dat_bio23_plot, aes(x= species, y = dry_mass, col = treat_comp_warm)) +
  geom_point() +
  facet_wrap(treat_comp_warm~site, scales = "free") +
  theme_bw() 

# looks more or less ok, expect a single crazy Cenjac outlier Cal ambi.bare, Brapin/Broere/Daucar/Hypper ambi.vege Nes and some very heavy Broere Cal warm.vege

# closer look at Plamed Cal (because of dirt)

ggplot(data = dat_bio23_plot[dat_bio23_plot$species == "plamed",], aes(x= species, y = dry_mass, col = treat_comp_warm)) +
  geom_point() +
  facet_wrap(~site) +
  theme_bw()

# also looks more or less ok at first sight, 1 outlier for Nes ambi.bare 

### SAVE CLEAN VERSION #########################################################

write.csv(dat_bio23, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Biomass/Biomass Focals/RangeX_clean_FocalBiomass_2023_CHE.csv",
          row.names = FALSE)






