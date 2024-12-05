################################################################################
### DATA CLEANING SCRIPT: BIOMASS GERMINATION/GREENHOUSE #######################
################################################################################

################################################################################

### Data used           : RangeX_raw_SeedlingBiomassGreenhouse_2024.csv, RangeX_raw_SeedlingBiomassExp2_2023.csv 
### Date last modified  : 08.08.2024
### Purpose             : Clean the raw size and biomass data file for the second germination experiment (autumn 2022 - autumn 2023) and the greenhouse experiment (2024). 
###                       Add them together and check for missing entries, impluasible values, wrong column names etc.

################################################################################

#rm(list = ls())

### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# load biomass data
exp2 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Biomass/Biomass Germination 2.0/RangeX_raw_BiomassSeedlingExp2_2023.csv")
exp2_org <- exp2

greenhouse <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Biomass/Biomass Greenhouse/RangeX_raw_BiomassSeedlingGreenhouse_2024.csv")
greenhouse_org <- greenhouse


# define useful vector
functional_groups <- c("brapin" = "graminoid", "broere" = "graminoid", "daucar" = "forb", "hypper" = "forb",
                       "medlup" = "legume", "plamed" = "forb", "silvul" = "forb", "scacol" = "forb",
                       "cenjac" = "forb", "salpra" = "forb")

# wanted columns & data types
final_columns <- c("germination_position_ID", "germination_seedling_ID", "species", "functional_group", "date_measurement", "date_sowing", "collector", "seed_origin", "height_vegetative_str", "leaf_length1", "leaf_length2",
                  "leaf_length3", "number_leaves",  "dry_mass")

date_cols <- c("date_measurement", "date_sowing")
numeric_cols <- c("height_vegetative_str", "leaf_length1", "leaf_length2",
                  "leaf_length3", "number_leaves",  "dry_mass")
string_cols <- c("germination_position_ID", "germination_seedling_ID", "species", "functional_group", "collector", "seed_origin")


################################################################################
### GERMINATION EXP 2 ##########################################################
################################################################################

exp2 <- exp2_org

### PREPARATION: COLUMNS, DATA CLASSES #########################################

# change column names: get column names
dput(colnames(exp2))

# change them to new names
exp2 <- exp2 %>%
  rename("date_collection" = "Collection_Date",
         "region" = "Country",
         "site" = "Site",
         "species" = "Species",
         "date_measurement" = "Collection_Date",
         "height_vegetative_str" = "Veg_Height",
         "number_leaves" = "Number_Leaves",
         "leaf_length1" = "Leaflength_1",
         "leaf_length2" = "Leaflength_2",
         "leaf_length3" = "Leaflength_3",
         "dry_mass" = "Dryweight")

# change variables in columns to standard, delete any completely NA rows
exp2 <- exp2 %>%
  mutate(species = str_trim(species), # get rid of whitespace
         species = tolower(species)) %>%
  janitor::remove_empty(which = "rows")


### ADD TREATMENTS ETC. ########################################################

# as those are completely new plots, treatments need to be added manually: 
# Cal_a --> warm, vege, Cal_b --> warm, bare, Cal_c --> ambi, bare, Cal_d --> ambi, vege
# Nes_a --> ambi, vege, Nes_b --> ambi, bare


exp2 <- exp2 %>%
  mutate(Plot_Name = paste(site, Plot_NAme, sep = "_"),
           warm_treat = case_when(Plot_Name == "Cal_a" ~ "warm",
                                Plot_Name == "Cal_b" ~ "warm",
                                Plot_Name == "Cal_c" ~ "ambi",
                                Plot_Name == "Cal_d" ~ "ambi",
                                Plot_Name == "Nes_a" ~ "ambi",
                                Plot_Name == "Nes_b" ~ "ambi"),
         comp_treat = case_when(Plot_Name == "Cal_a" ~ "vege",
                                Plot_Name == "Cal_b" ~ "bare",
                                Plot_Name == "Cal_c" ~ "bare",
                                Plot_Name == "Cal_d" ~ "vege",
                                Plot_Name == "Nes_a" ~ "vege",
                                Plot_Name == "Nes_b" ~ "bare"))

# add a block (is all just block 1), separate the position ID into an individual index number and position ID, create a string position, standardize site
exp2 <- exp2 %>%
  separate(Position, into = c("Position", "ind_number"), sep = "\\.", fill = "right", convert = TRUE) %>%
  mutate(ind_number = ifelse(is.na(ind_number), 1, ind_number)) %>%
  mutate(block_ID = as.character("01"),
         position_string = str_pad(Position, width = 3, pad = "0"),
         site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"))

# fix the two rows where the position ID was uncertain in field
# daucar in Cal_c must be position 125 (nothing germinated in 124 according to germination data)
# medlup in Cal_c must be position 84 (83 should be a broere)
exp2[exp2$position_string == "124/125",]$position_string <- "125"
exp2[exp2$position_string == "84/83?",]$position_string <- "084"

# generate the unique plot ID (region.site.exp-no.warmingTreatment.competitionTreatment.block.position)
exp2 <- exp2 %>%
  mutate(germination_position_ID = paste(region, site, "exp2", warm_treat, comp_treat, block_ID, position_string, sep = "."),
         germination_seedling_ID = paste(germination_position_ID, ind_number, sep = "."))


### PROBLEM FIXING #############################################################


# why only 218 unique values in germination_seedling_ID?
#dupli <- exp2 %>% 
#  get_dupes(germination_seedling_ID) 

# CHE.hi.exp2.warm.vege.01.030 should be a broere --> add cenjac as a second sample from CHE.hi.exp2.warm.vege.01.028
exp2[exp2$germination_position_ID == "CHE.hi.exp2.warm.vege.01.030" & exp2$species == "cenjac",]$germination_seedling_ID <- "CHE.hi.exp2.warm.vege.01.028.2"
exp2[exp2$germination_position_ID == "CHE.hi.exp2.warm.vege.01.030" & exp2$species == "cenjac",]$germination_position_ID <- "CHE.hi.exp2.warm.vege.01.028"

# CHE.lo.exp2.ambi.vege.01.113 should be a scacol --> add broere as CHE.lo.exp2.ambi.vege.01.055 (same plot, so doesn't matter so much and there's two unmeasured seedlings at that position)
exp2[exp2$germination_position_ID == "CHE.lo.exp2.ambi.vege.01.113" & exp2$species == "broere",]$germination_seedling_ID <- "CHE.lo.exp2.ambi.vege.01.055.1"
exp2[exp2$germination_position_ID == "CHE.lo.exp2.ambi.vege.01.113" & exp2$species == "broere",]$germination_position_ID <- "CHE.lo.exp2.ambi.vege.01.055"

# check again for duplicates
dupli <- exp2 %>% 
  get_dupes(germination_seedling_ID) # none

# look at comments
unique(exp2$Comment)

# rep and flowers: ok, ignore
# Keimblatt/Keimblätter: leave for now, were also counted in greenhouse experiment and in initial size measurements (belong to biomass)

### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check for NA's in metadata
exp2_na <- exp2 %>%
  filter(is.na(germination_seedling_ID) | is.na(germination_position_ID) | is.na(species) | is.na(species) | is.na(date_measurement)) # none


# check for NAs: filter all rows with NA's in vegetative height and number of leaves or weight
exp2_na <- exp2 %>%
  filter(is.na(height_vegetative_str) | is.na(number_leaves)) # no NAs

# check for NAs: filter all rows with NA's in weight
exp2_na <- exp2 %>%
  filter(is.na(dry_mass)) # NAs if samples were too small or lost

### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# delete all columns not in final data frame plus rearrange in right order
exp2 <- exp2 %>%
  dplyr::select(any_of(final_columns))

# add missing columns: collector (is all EI), seed origin, date_sowing and functional group
exp2 <- exp2 %>%
  mutate(collector = "EI",
         seed_origin = case_when(species %in% c("broere", "cenjac") ~ "commercial",
                                 .default = "collected"),
         date_sowing = as.Date("2022-10-10", fomrat = "%Y-%m-%d"),
         functional_group = functional_groups[species])

# make sure to delete empty spaces in all columns
exp2 <- exp2 %>%
  mutate(across(c(1:14), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
exp2 <- exp2 %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"),
         date_sowing = as.Date(date_sowing, format = "%Y-%m-%d"))

# re-arrange column order
exp2 <- exp2 %>%
  dplyr::select(any_of(final_columns))



################################################################################
### GREENHOUSE #################################################################
################################################################################

greenhouse <- greenhouse_org

### PREPARATION: COLUMNS, DATA CLASSES #########################################

# change column names: get column names
dput(colnames(greenhouse))

# change them to new names
greenhouse <- greenhouse %>%
  rename("dry_mass" = "weight")

# change variables in columns to standard, delete any completely NA rows
greenhouse <- greenhouse %>%
  mutate(species = str_trim(species)) %>% # get rid of whitespace
  janitor::remove_empty(which = "rows")


### ADD TREATMENTS ETC. ########################################################

# there are no treatments in the greenhouse experiment: add "none" both for competition and warming treatment in unique ID

greenhouse <- greenhouse %>%
  mutate(comp_treat = "none",
         warm_treat = "none")

# create continuous ID for all samples together (not 1-15 for each species - otherwise the germination_seedling_ID won't be unique as they're all in the same plot/treatment etc.)
greenhouse <- greenhouse %>%
  arrange(species, ID) %>%
  mutate(unique_ID = row_number()) %>%
  ungroup()

# add a block (is all just block 1), create a padded string position (3 digits), add ind_number (is always 1 as no real treatments/ positions), add site (gh for GreenHouse), add region (CHE)
greenhouse <- greenhouse %>%
  mutate(ind_number = as.character("1")) %>%
  mutate(block_ID = as.character("01"),
         position_string = str_pad(unique_ID, width = 3, pad = "0"),
         site = "gh",
         region = "CHE")

# generate the unique plot ID (region.site.exp-no.warmingTreatment.competitionTreatment.block.position) (grho for greenhouse to specify the experiment)
greenhouse <- greenhouse %>%
  mutate(germination_position_ID = paste(region, site, "grho", warm_treat, comp_treat, block_ID, position_string, sep = "."),
         germination_seedling_ID = paste(germination_position_ID, ind_number, sep = "."))


### PROBLEM FIXING #############################################################


# check for duplicates
dupli <- greenhouse %>% 
  get_dupes(germination_seedling_ID) # none

# look at comments
unique(greenhouse$comments)

# UFA seeds/Nes seeds etc.: will be used to deduce seed origin
# Keimblatt/Keimblätter: are included in leaf number for all species except medlup --> keep it this way, for medlup they are too small to contribute much


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check for NA's in metadata
greenhouse_na <- greenhouse %>%
  filter(is.na(germination_seedling_ID) | is.na(germination_position_ID) | is.na(species) | is.na(species) | is.na(date_measurement) | is.na(date_sowing)) # none


# check for NAs: filter all rows with NA's in vegetative height and number of leaves or weight
greenhouse_na <- greenhouse %>%
  filter(is.na(height_vegetative_str) | is.na(number_leaves)) # no NAs

# check for NAs: filter all rows with NA's in weight
greenhouse_na <- greenhouse %>%
  filter(is.na(dry_mass)) # none

### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################


# add missing columns: seed origin and functional group
greenhouse <- greenhouse %>%
  mutate(seed_origin = case_when(species %in% c("plamed", "scacol", "hypper", "salpra", "broere") ~ "collected",
                                 .default = "commercial"), # these are the originally sowed out seeds --> if later other seeds were used, it's noted in the comments (see protocol)
         seed_origin = ifelse(grepl("Nes", comments) == TRUE, "collected", seed_origin),
         seed_origin = ifelse(grepl("UFA", comments) == TRUE, "commercial", seed_origin),
         functional_group = functional_groups[species])

# delete all columns not in final data frame plus rearrange in right order
greenhouse <- greenhouse %>%
  dplyr::select(any_of(final_columns))


# make sure to delete empty spaces in all columns
greenhouse <- greenhouse %>%
  mutate(across(c(1:14), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
greenhouse <- greenhouse %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_measurement = as.Date(date_measurement, format = "%d.%m.%Y"),
         date_sowing = as.Date(date_sowing, format = "%d.%m.%Y"))

# re-arrange column order
greenhouse <- greenhouse %>%
  dplyr::select(any_of(final_columns))


################################################################################
### COMBINE ####################################################################
################################################################################

# add rows of both data frames
mass_combined <- bind_rows(exp2, greenhouse)

# check for NAs in metadata
combined_na <- mass_combined %>%
  filter(if_any(1:8, is.na)) # none

# check levels
combined_levels <- mass_combined %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique)


### CONTROL PLOTTING AND MODELLING #############################################


ggplot(data = mass_combined, aes(x = height_vegetative_str, y = dry_mass, col = species)) +
  geom_point() +
  #facet_wrap(~species, scales = "free") +
  theme_bw() +
  geom_smooth(method = "lm")

# there's one huge scacol: is from low site in bare plot, so true indeed as it was one of the only ones actually surviving --> might be an outlier anyway, but decide in actual analysis

# check without
ggplot(data = mass_combined[mass_combined$dry_mass < 0.25,], aes(x = height_vegetative_str, y = dry_mass, col = species)) +
  geom_point() +
  #facet_wrap(~species, scales = "free") +
  theme_bw() +
  geom_smooth(method = "lm")




### SAVE CLEAN VERSION #########################################################

write.csv(mass_combined,"/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Biomass/RangeX_clean_SeedlingBiomass_2023_2024_CHE.csv",
          row.names = FALSE)



