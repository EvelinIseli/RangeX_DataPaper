################################################################################
### DATA CLEANING SCRIPT: PHENOLOGY ############################################
################################################################################

################################################################################

### Data used           : RangeX_raw_Phenology_2022.csv, RangeX_raw_PhenologyDate_2022.csv, RangeX_clean_Survival_2021_2023_CHE.csv, RangeX_clean_MetadataFocal_CHE.csv
### Date last modified  : 22.02.2024
### Purpose             : Clean phenology data from 2022, add exact dates of checks, collector and distinguish between dead plants and plants with no flowers.

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library(lubridate) # handle dates


### LOAD DATA SET ##############################################################

# load phenology data
dat_phen <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Flower Phenology/RangeX_raw_Phenology_2022.csv")
dat_phen_org <- dat_phen

# load date key
dat_phendate <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Flower Phenology/RangeX_raw_PhenologyDate_2022.csv")

# load survival data (to distinguish between dead plants and plants with just no flowers)
dat_surv <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Survival & Demographics/Spring Survival Check/RangeX_clean_Survival_2021_2023_CHE.csv")
dat_surv_orginal <- dat_surv

# load treatment key
meta_plant <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")

# define useful vector
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")


# wanted columns & data types
final_columns <- c("unique_plant_ID", "species", "date_measurement", "collector", "phenology_stage", "value")

integer_cols <- c("value")
string_cols <- c("unique_plant_ID", "species", "collector")
factor_cols <- c("phenology_stage")
date_cols <- c("date_measurement")




### PREPARATION: COLUMNS, DATA CLASSES #########################################

# check data frame
str(dat_phen)

# correct data types to not accidentally mess things up later (no logicals!)
dat_phen <- dat_phen %>%
  mutate(across(starts_with("Comm"), ~ as.character(.)),
         across(starts_with("No_"), ~ as.integer(as.character(.))))

# change column names: get column names
dput(colnames(dat_phen))

# change them to new names
dat_phen <- dat_phen %>%
  rename("block_ID_original" = "Block",
         "plot_ID_original" = "Plot",
         "position_ID_original" = "Position",
         "species" = "Species")

# change variables in columns to standard, delete the many completely NA rows (should only be 1389 rows with data)
dat_phen <- dat_phen %>%
  mutate(site = case_when(Site == "Nes" ~ "lo" , Site == "Cal" ~ "hi"), # change site names (but the "old" site names will still be needed later for maerging with dates, so keep "Site")
         species = ifelse(species %in% names(species_names),  species_names[species], species)) %>%
  janitor::remove_empty(which = "rows")



### ADD TREATMENTS ETC. ########################################################

# prepare treatment key
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup()

# check broere/ prapin mixup in dat_phen to fix before merging
dat_phen[dat_phen$site == "lo" & dat_phen$block_ID_original == 7 & dat_phen$plot_ID_original == 1,c(1:6)] # yes, is wrong --> position 1 should be broere and 3 should be brapin
dat_phen[dat_phen$site == "lo" & dat_phen$block_ID_original == 7 & dat_phen$plot_ID_original == 1 & dat_phen$position_ID_original == 1, ]$species <- "broere"
dat_phen[dat_phen$site == "lo" & dat_phen$block_ID_original == 7 & dat_phen$plot_ID_original == 1 & dat_phen$position_ID_original == 3, ]$species <- "brapin"

# merge treatments to data frame
dat_phen <- full_join(dat_phen, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species")) # nice, 1800


### STRUCTURE & PROBLEM FIXING #################################################

# get rid of all columns not wanted for easier transfer to long data frame
dat_phen <- dat_phen %>%
  dplyr::select(starts_with("No_"), unique_plant_ID, species, Site)

# make long data frame
dat_phen <- dat_phen %>%
  pivot_longer(cols = -c(species, unique_plant_ID, Site), 
               names_to = c(".value", "Week"), 
               names_pattern = "(\\w+)_(w\\d+(?:\\.\\d+)?)") # regex to capture the "w18", "w19", "w22.1" part (from ChatGPT)

# prepare week column for adding dates
dat_phen <- dat_phen %>%
  mutate(Week = str_replace(Week, "w", ""))

# prepare date data frame
dat_phendate$Week <- as.character(dat_phendate$Week)

# join
dat_phen <- left_join(dat_phen, dat_phendate, by = c("Week", "Site")) # Cal week 18 is all NA as no data was recorded yet, so all good

# now add (clean & corrected) survival data from spring 2022 to be able to transform all NA for plants which were actually alive to 0s (and keep all dead plants to NA)

# prepare survival data (extract spring_survival 2022)
dat_surv <- dat_surv %>%
  filter(variable == "spring_survival" & lubridate::year(date_measurement) == 2022)

# merge on
dat_phen <- left_join(dat_phen, dat_surv, by = c("unique_plant_ID", "species")) # the 36 non-matching rows are the individuals replaced in 2021

# now change NAs to 0 for all individuals with survival == 1
dat_phen <- dat_phen %>%
  mutate(across(starts_with("No_"), ~ifelse(value == 1 & is.na(.), 0, .)))

# now also make long data frame out of the stages
dat_phen <- dat_phen %>%
  pivot_longer(cols = starts_with("No_"), 
               names_to = "phenology_stage",
               values_to = "value2")


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_phen_na <- dat_phen %>% 
  filter(is.na(unique_plant_ID) | is.na(Date) | is.na(species)) # it's the Calanda check for week 18 which didn't take place to begin with --> delete those rows

dat_phen <- dat_phen %>%
  filter(!(Site == "Cal" & Week == 18))

dat_phen_na <- dat_phen %>% 
  filter(is.na(unique_plant_ID) | is.na(Date) | is.na(species)) # all good


# check for NAs: check whether all rows with all NA are dead (value == 0)
dat_phen_na <- dat_phen %>% 
  filter(is.na(value2) == TRUE) # all good


# now check whether all survival == 0 are all NA
dat_phen_0 <- dat_phen %>%
  filter(value == 0) # nope, there's two individuals with weird 1 even though they're dead

# the two 1's in CHE.lo.ambi.bare.wf.01.27.2 must be wrong, that individual is dead...
check <- dat_phen[dat_phen$unique_plant_ID == "CHE.lo.ambi.bare.wf.01.27.2",]
dat_phen[dat_phen$unique_plant_ID == "CHE.lo.ambi.bare.wf.01.27.2" & dat_phen$phenology_stage == "No_FloWithrd",]$value2 <- NA # no need to specify date - all should be NA anyway
dat_phen[dat_phen$unique_plant_ID == "CHE.lo.ambi.bare.wf.01.27.2" & dat_phen$phenology_stage == "No_Seeds",]$value2 <- NA

# CHE.hi.warm.bare.wf.09.08.1 also has a 1 but should also be NA everywhere as it's dead...
check <- dat_phen[dat_phen$unique_plant_ID == "CHE.hi.warm.bare.wf.09.08.1",]
dat_phen[dat_phen$unique_plant_ID == "CHE.hi.warm.bare.wf.09.08.1" & dat_phen$phenology_stage == "No_Buds",]$value2 <- NA # no need to specify date - all should be NA anyway


# check again...
dat_phen_0 <- dat_phen %>%
  filter(value == 0) # all good

# now set the plots forgotten to check on 09.06.2022 to NA, whether the plants are dead or not (plots: Nes_4.1, Nes_5.1, Nes_6.1, Nes_7.2, Nes_8.2, Nes_9.2)
not_checked <- c("CHE.lo.ambi.vege.wf.04", "CHE.lo.ambi.vege.wf.05", "CHE.lo.ambi.vege.wf.06", "CHE.lo.ambi.vege.wf.07", "CHE.lo.ambi.vege.wf.08", "CHE.lo.ambi.vege.wf.09") # save plots not checked

dat_phen <- dat_phen %>% # set to NA
  mutate(plot = str_sub(unique_plant_ID, 1, 22), 
         value2 = ifelse(Date == "09.06.2022" & plot %in% not_checked, NA, value2))

check <- dat_phen[dat_phen$Week == 23 & dat_phen$Site == "Nes" & dat_phen$plot %in% not_checked,] # check whether it worked - all good


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# rename columns, create collector, make date a date
dat_phen <- dat_phen %>%
  dplyr::select(-value, -date_measurement) %>%
  rename("value" = "value2") %>%
  mutate(date_measurement = as.Date(Date, "%d.%m.%Y"),
         collector = case_when(date_measurement == "2022-06-09" ~ "JA",
                               date_measurement == "2022-07-06" & grepl("hi", unique_plant_ID) == TRUE & grepl("bare", unique_plant_ID) == TRUE ~ "CB",
                               TRUE ~ "EI"))


# delete all columns not in final data frame plus rearrange in right order
dat_phen <- dat_phen %>%
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
dat_phen <- dat_phen %>%
  mutate(across(c(1:6), \(x) str_remove_all(x, pattern = fixed(" "))))

# change data types
dat_phen <- dat_phen %>%
  mutate(across(all_of(integer_cols), as.numeric),
         across(all_of(string_cols), as.character),
         across(all_of(factor_cols), as.factor),
         date_measurement = as.Date(date_measurement, "%Y-%m-%d"))

str(dat_phen)


### CONTROL PLOTTING AND MODELLING #############################################

# get sum within sites and species of all flower stages
dat_phen_sum <- dat_phen %>%
  mutate(site = str_sub(unique_plant_ID, 5, 6)) %>%
  group_by(species, site, date_measurement, phenology_stage) %>%
  summarize(n = sum(value, na.rm = TRUE))

ggplot(data = dat_phen_sum, aes(x = date_measurement, y = n, col = site)) +
  geom_point() +
  geom_line() +
  facet_grid(species~phenology_stage)

# cool! now scale relatively to no. individuals alive
dat_phen_sum2 <- dat_phen %>%
  mutate(site = str_sub(unique_plant_ID, 5, 6),
         survival = ifelse(is.na(value) == TRUE, 0, 1)) %>% # hacky, would be better to add survival data back on
  group_by(species, site, date_measurement, phenology_stage) %>%
  summarize(n_flow = sum(value, na.rm = TRUE),
            n_surv = sum(survival)) %>%
  mutate(rel_flow = n_flow/n_surv*100)

ggplot(data = dat_phen_sum2, aes(x = date_measurement, y = rel_flow, col = site)) +
  geom_point() +
  geom_line() +
  facet_grid(species~phenology_stage)



### SAVE CLEAN VERSION #########################################################

write.csv(dat_phen, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Flower Phenology/RangeX_clean_Phenology_2022_CHE.csv",
          row.names = FALSE)
















