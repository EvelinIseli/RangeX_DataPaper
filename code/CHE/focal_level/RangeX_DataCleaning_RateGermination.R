################################################################################
### DATA CLEANING SCRIPT: GERMINATION EXPERIMENT ###############################
################################################################################

################################################################################

### Data used           : RangeX_raw_Germination1_2022.csv, RangeX_raw_Germination2_2023.csv, RangeX_raw_Germination1_SeedNo_2021.csv, RangeX_raw_Germination2_SeedNo_2022.csv, RangeX_clean_MetadataPlot_CHE.csv
### Date last modified  : 22.02.2024
### Purpose             : Load, clean and merge the survival and establishment data of the two germination experiments on Calanda (Exp1 --> started in 2021, within focal individual plots, Exp2 --> started in 2022,
###                       on extra new plots). Load, clean and merge the no. seeds-per-stick data of Exp1 and Exp2. Load, clean and merge the size measurements and biomass of a subset of individuals of Exp2 and 
###                       Exp3 (Exp3 --> greenhouse experiment done in 2024).

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library(lubridate) # handle dates


### LOAD DATA SET ##############################################################

# load germination/establishment data
dat_germ2 <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Germination/RangeX_raw_Germination2_2023.csv")
dat_germ2_org <- dat_germ2
dat_germ1 <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Germination/RangeX_raw_Germination1_2022.csv")
dat_germ1_org <- dat_germ1

# load seeds-per-toothpick data 
dat_seed1 <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Germination/RangeX_raw_Germination1_SeedNo_2021.csv")
dat_seed1_org <- dat_seed1
dat_seed2 <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/3_DataRaw/Raw_FocalLevel/Raw_Germination/RangeX_raw_Germination2_SeedNo_2022.csv")
dat_seed2_org <- dat_seed2

# load treatment key (on plot level, not focal individual level!)
meta_plot <- read.csv("/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/1_Metadata/2_Metadata_FocalsPlots/RangeX_clean_MetadataPlot_CHE.csv")

# define useful vector
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")

functional_groups <- c("brapin" = "graminoid", "broere" = "graminoid", "daucar" = "forb", "hypper" = "forb",
                       "medlup" = "legume", "plamed" = "forb", "silvul" = "forb", "scacol" = "forb",
                       "cenjac" = "forb", "salpra" = "forb")


# wanted columns & data types
final_columns <- c("germination_position_ID", "species", "functional_group", "date_sowing", "date_measurement", "collector", "seed_origin", "variable", "value")

integer_cols <- c("value")
string_cols <- c("germination_position_ID", "species", "functional_group", "collector", "seed_origin", "variable")
date_cols <- c("date_measurement", "date_sowing")


################################################################################
### Experiment 1 ###############################################################
################################################################################

### PREPARATION: COLUMNS, DATA CLASSES #########################################

dat_germ1 <- dat_germ1_org

# check data frame
str(dat_germ1)

# change column names: get column names
dput(colnames(dat_germ1))

# change them to new names
dat_germ1 <- dat_germ1 %>%
  rename("region" = "Country",
        "site" = "Site",
        "block_ID_original" = "Block_Number",
         "plot_ID_original" = "Plot_Number",
         "position_ID_original" = "Position",
         "species" = "Species",
        "Autumn2022_Comments" = "Autrumn2022_Comments")

# change variables in columns to standard, delete the many completely NA rows (should only be 1389 rows with data)
dat_germ1 <- dat_germ1 %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names 
         species = case_when(species == "Brachipodium pinnatum" ~ "Brachypodium pinnatum", 
                             species == "Broere" ~ "Bromus erectus",
                             TRUE ~ species),
         species = ifelse(species %in% names(species_names),  species_names[species], species),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species)) %>%
  janitor::remove_empty(which = "rows")

# transform dates into date format
dat_germ1 <- dat_germ1 %>%
  mutate(across(contains("Date"), \(x) as.Date(x, format = "%d.%m.%y")))


### ADD TREATMENTS ETC. ########################################################

# merge
dat_germ1 <- left_join(dat_germ1, meta_plot, by = c("region", "site", "block_ID_original", "plot_ID_original")) # 16 rows only in y are the non-focal plots (which were not used in germination experiment)

# nice, no mismatches, no NAs in metadata


### PROBLEM FIXING #############################################################

# comments autumn 2021:
# tooth stick missing/ tooth pick lay on the ground/ tooth pick and cocktail stick lay on the ground/ out, tooth pick missing/ out/ no toothpick/ down : ?
# Fabaceae: it's the seed contamination for Scabiosa - set all of them to 0
# ?: 

# comments spring 2022:
# herbivory/ plus 1 dead/ etc.: all comments can be ignored

# comments autumn 2022: 
# stick not found: set to NA, can be left NA
# rest of comments can be ignored


# fix the problems
dat_germ1 <- dat_germ1 %>%
  mutate(across(contains("Seedlings"), ~ ifelse(grepl("Fabaceae", Autumn2021_Comments), NA, .)))

### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_germ1_na <- dat_germ1 %>% 
  filter(is.na(unique_plot_ID) | is.na(Autumn2021_Collector) | is.na(Autumn2021_Date) | is.na(species)| is.na(Autumn2022_Collector) | is.na(Autumn2022_Date) | is.na(Spring2022_Collector) | is.na(Spring2022_Date)) # all good

# check for NAs: filter all rows with NA's in survival
dat_germ1_na <- dat_germ1 %>% 
  filter(if_any(contains("Seedlings"), is.na))

# all good, it's either the Fabaceae or stick not found

# check total
dat_germ1_na <- dat_germ1 %>%
  filter(if_any(-contains("Comments"), is.na)) # same

str(dat_germ1)

### DELETE & ADD COLUMNS, DATATYPES ############################################

# check whether there's many fluctuations between seedlings present, absent, present again etc.
illogical_seedlings <- dat_germ1 %>%
  mutate(across(contains("Seedlings"), ~ ifelse(. > 0, 1, 0))) %>%
  rowwise() %>%
  mutate(logical = ifelse(any(diff(c_across(contains("Seedlings"))) > 0, na.rm = TRUE), "not ok", "ok")) %>%
  ungroup()

# There's some positions where seedlings germinate late, there's some where where there's seedlings - no seedlings - seedlings or basically any other combination. HOWEVER: Not sure it's ok to retrospectively correct,
# as new seedlings could germinate any time while germinated seedlings can always die...

# make long!
dat_germ1_long <- dat_germ1 %>%
  pivot_longer(cols = contains("Seedlings") | contains("Date")| contains("Collector")  | contains("Comments") , 
               names_to = c("Timepoint", ".value"), 
               names_pattern = "(.*)_(.*)",
               values_drop_na = FALSE)


# rename the new columns, change the names of the timepoints, add a sowing date, add seed origin (all commercial for exp 1)
dat_germ1_long <- dat_germ1_long %>%
  rename("date_measurement" = "Date",
         "collector" = "Collector",
         "variable" = "Timepoint",
         "value" = "Seedlings") %>%
  mutate(variable = case_when(variable == "Autumn2021" ~ "autumn_count",
                              variable == "Autumn2022" ~ "peak_count",
                              variable == "Spring2022" ~ "spring_count"),
         date_sowing = as.Date("2021-09-09", fomrat = "%Y-%m-%d"),
         seed_origin = "commercial")


# create desired germination_position_ID: region.site.exp-no.warmingTreatment.competitionTreatment.block.position
dat_germ1_long <- dat_germ1_long %>%
  mutate(block_string = ifelse(block_ID == 10, as.character(block_ID), paste0("0", as.character(block_ID))),
         position_string = str_extract(as.character(position_ID_original), "^\\d."),
         position_string = str_replace(position_string, "\\.", ""),
         position_string = str_pad(position_string, width = 3, pad = "0"),
         germination_position_ID = paste(region, site, "exp1", treat_warming, treat_competition, block_string, position_string, sep = ".")) %>%
  dplyr::select(-contains("string"))


# delete all columns not in final data frame plus rearrange in right order
dat_germ1_long <- dat_germ1_long %>% 
  dplyr::select(any_of(final_columns))

str(dat_germ1_long)

# make sure to delete empty spaces in all columns
dat_germ1_long <- dat_germ1_long %>% 
  mutate(across(c(1:9), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_germ1_long <- dat_germ1_long %>%
  mutate(across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         across(contains("date"), ~as.Date(., format = "%Y-%m-%d")))

str(dat_germ1_long)



################################################################################
### Experiment 2 ###############################################################
################################################################################


### PREPARATION: COLUMNS, DATA CLASSES #########################################

dat_germ2 <- dat_germ2_org

# check data frame
str(dat_germ2)

# change column names: get column names
dput(colnames(dat_germ2))

# change them to new names
dat_germ2 <- dat_germ2 %>%
  rename("region" = "Country",
         "site" = "Site",
         "species" = "Species",
         "Autumn2023_Comments" = "Autrumn2023_Comments")

# change variables in columns to standard, delete the many completely NA rows 
dat_germ2 <- dat_germ2 %>%
  janitor::remove_empty(which = "rows") %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names 
         species = ifelse(species == "Brachipodium pinnatum", "Brachypodium pinnatum", species),
         species = ifelse(species %in% names(species_names),  species_names[species], species),
         functional_group = ifelse(species %in% names(functional_groups),  functional_groups[species], species)) 

# transform dates into date format
dat_germ2 <- dat_germ2 %>%
  mutate(across(contains("Date"), \(x) as.Date(x, format = "%d.%m.%Y")))


### ADD TREATMENTS ETC. ########################################################

# as those are completely new plots, treatments need to be added manually: 
# Cal_a --> warm, vege, Cal_b --> warm, bare, Cal_c --> ambi, bare, Cal_d --> ambi, vege
# Nes_a --> ambi, vege, Nes_b --> ambi, bare

# generate a position number which is unique (1:125) instead of of the subplot_position system
dat_germ2 <- dat_germ2 %>%
  group_by(Plot_Name) %>%
  mutate(Position_unique = row_number()) %>%
  ungroup()

dat_germ2 <- dat_germ2 %>%
  mutate(warm_treat = case_when(Plot_Name == "Cal_a" ~ "warm",
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

# add a block (is all just block 1) and create a string position
dat_germ2 <- dat_germ2 %>%
  mutate(block_ID = as.character("01"),
         Position_unique = as.numeric(Position_unique),
         position_string = str_pad(Position_unique, width = 3, pad = "0"))

# generate the unique plot ID (region.site.exp-no.warmingTreatment.competitionTreatment.block.position)
dat_germ2 <- dat_germ2 %>%
  mutate(germination_position_ID = paste(region, site, "exp2", warm_treat, comp_treat, block_ID, position_string, sep = "."))


### PROBLEM FIXING #############################################################

# comments autumn 2022:
# ?: 

# comments spring 2023:
# centaurea: change species to cenjac
# rest: can be ignored

# comments autumn 2023: 
# Cal_d, position 2 is Scacol instead of Daucar
# Cal_a, position 11 is Cenjac instead of Hypper
# Cal_c, position 13: not Broere, set Spring2023 to 0
# herbivory/ Keimblatt/reproductive measurements: ignore
# rest: ignore

# fix the problems
dat_germ2 <- dat_germ2 %>%
  mutate(species = case_when(Autumn2023_Comments == "centaurea" ~ "cenjac",
                             Plot_Name == "Cal_d" & Position == 2 ~ "scacol",
                             Plot_Name == "Cal_a" & Position == 11 ~ "cenjac",
                             TRUE ~ species),
         Spring2023_Seedlings = ifelse(Plot_Name == "Cal_c" & Position == 13, 0, Spring2023_Seedlings))


# sometimes ? in actual seedling count
dat_germ2 <- dat_germ2 %>%
  mutate(across(contains("Seedlings"), ~str_replace(., "\\?", "")))

# empty spaces and na's in Seedlings Autumn 2023: transform into 0's and real NA's
dat_germ2 <- dat_germ2 %>%
  mutate(Autumn2023_Seedlings = case_when(Autumn2023_Seedlings == "" ~ "0",
                                          Autumn2023_Seedlings == "na" ~ NA_character_,
                                          TRUE ~ Autumn2023_Seedlings))
  

### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_germ2_na <- dat_germ2 %>% 
  filter(is.na(germination_position_ID) | is.na(Autumn2022_Collector) | is.na(Autumn2022_Date) | is.na(species)| is.na(Autumn2023_Collector) | is.na(Autumn2023_Date) | is.na(Spring2023_Collector) | is.na(Spring2023_Date)) # all good

# check for NAs: filter all rows with NA's in survival
dat_germ2_na <- dat_germ2 %>% 
  filter(if_any(contains("Seedlings"), is.na))

# all good, it's only the bare soil plots (completely unusable due to pushed out toothpicks on Nesselboden, only half usable on Calanda)

str(dat_germ2)

### DELETE & ADD COLUMNS, DATATYPES ############################################

# delete all the size measurements in autumn 2023 and make long!
dat_germ2_long <- dat_germ2 %>%
  dplyr::select(-matches("Height|LeafNumber|Leaflength")) %>%
  pivot_longer(cols = contains("Seedlings") | contains("Date")| contains("Collector")  | contains("Comments") , 
               names_to = c("Timepoint", ".value"), 
               names_pattern = "(.*)_(.*)",
               values_drop_na = FALSE)


# rename the new columns, change the names of the timepoints, add a sowing date, add seed_origin
dat_germ2_long <- dat_germ2_long %>%
  rename("date_measurement" = "Date",
         "collector" = "Collector",
         "variable" = "Timepoint",
         "value" = "Seedlings") %>%
  mutate(variable = case_when(variable == "Autumn2022" ~ "autumn_count",
                              variable == "Autumn2023" ~ "peak_count",
                              variable == "Spring2023" ~ "spring_count"),
         date_sowing = as.Date("2022-10-10", fomrat = "%Y-%m-%d"),
         seed_origin = case_when(species %in% c("broere", "cenjac") ~ "commercial",
                                 .default = "collected"))


# delete all columns not in final data frame plus rearrange in right order
dat_germ2_long <- dat_germ2_long %>% 
  dplyr::select(any_of(final_columns))

str(dat_germ2_long)

# make sure to delete empty spaces in all columns
dat_germ2_long <- dat_germ2_long %>% 
  mutate(across(c(1:8), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_germ2_long <- dat_germ2_long %>%
  mutate(across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character),
         across(contains("date"), ~as.Date(., format = "%Y-%m-%d")))

str(dat_germ2_long)

################################################################################
### COMBINE ####################################################################
################################################################################


dat_germ_complete <- bind_rows(dat_germ1_long, dat_germ2_long) # nice

# check for NA's
dat_germ_complete_na <- dat_germ_complete %>%
  filter(if_any(everything(), is.na)) # look like it's only the bare soil plots in sring2023 and autumn2023

# check levels
dat_germ_complete_levels <- dat_germ_complete %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique)

unique(dat_germ_complete$value)


### SAVE CLEAN VERSION #########################################################

#write.csv(dat_germ_complete, "/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/6_DataClean/RangeX_clean_RateGermination_2022_2023_CHE.csv",
#          row.names = FALSE)


################################################################################
### SEEDS PER TOOTHPICK ########################################################
################################################################################

dat_seed1 <- dat_seed1_org
dat_seed2 <- dat_seed2_org

# rename column 1, sum up Medlup counts (green seeds are assumed to germinate as well), change species names
dat_seed1 <- dat_seed1 %>%
  rename("species" = "X") %>%
  mutate(species = case_when(species == "Medlup (green)" ~ "Medlup",
                             species == "Medlup (black)" ~ "Medlup",
                             TRUE ~ species),
         species = tolower(species)) %>%
  group_by(species) %>%
  summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(experiment = "exp1")

dat_seed2 <- dat_seed2 %>%
  rename("species" = "X") %>%
  mutate(species = tolower(species),
         experiment = "exp2") 


# make the data frames long
dat_seed1_long <- pivot_longer(dat_seed1, cols = contains("stick"), names_to = "stick_no", values_to = "seedcount") 
dat_seed2_long <- pivot_longer(dat_seed2, cols = contains("stick"), names_to = "stick_no", values_to = "seedcount") %>%
  drop_na(seedcount)


# combine
dat_seed_complete <- bind_rows(dat_seed1_long, dat_seed2_long) # nice

# check for NA's
dat_seed_complete_na <- dat_seed_complete %>%
  filter(if_any(everything(), is.na)) # none, nice

str(dat_seed_complete)

### SAVE CLEAN VERSION #########################################################

#write.csv(dat_seed_complete, "/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/6_DataClean/RangeX_clean_SeedNoGermination_2021_2022_CHE.csv",
#          row.names = FALSE)


################################################################################
### ADD AVERAGE SEEDS PER TOOTHPICK ############################################
################################################################################

# calculate the avergae no. seeds per toothpick and add this as a fixed variable to the germination rate data frame

# calculate average number of seeds per stick
summary_sticks <- dat_seed_complete %>%
  group_by(species, experiment) %>%
  summarize(mean_seedno = mean(seedcount),
            mean_seedno = round(mean_seedno, digits=0))

# add as variable back on to dat_germ_complete
dat_germ_complete <- dat_germ_complete %>%
  mutate(experiment = str_extract(germination_position_ID, "exp[0-9]+")) %>%
  left_join(summary_sticks, by = c("species", "experiment"))

# double-check
dat_germ_complete %>%
  distinct(species, experiment, mean_seedno) # looks good

# prepare for export
write.csv(dat_germ_complete, "/Users/mac/Desktop/ETH_Phd+/Projects/RangeX/RangeX_Data/6_DataClean/RangeX_clean_RateGermination_2022_2023_CHE.csv",
          row.names = FALSE)
