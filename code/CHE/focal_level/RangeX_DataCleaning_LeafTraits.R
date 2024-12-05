################################################################################
### DATA CLEANING SCRIPT: LEAF TRAITS ##########################################
################################################################################

################################################################################

### Data used           : RangeX_raw_LeafTraits_2023.csv, RangeX_raw_LeafTraits_2022.csv, RangeX_clean_MetadataFocal_CHE.csv
### Date last modified  : 24.05.2024
### Purpose             : Clean and combine all raw data files containing leaf trait data (leaf area, leaf thickness, wet & dry weight, stomatal density)

################################################################################

#rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(dplyr); library(tidylog); library(janitor); library(tidyverse) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex


### LOAD DATA SET ##############################################################

# load data
leaves22 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Leaf Traits/2022/RangeX_raw_LeafTraits_2022.csv")
leaves23 <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Leaf Traits/2023/RangeX_raw_LeafTraits_2023.csv")

leaves22_org <- leaves22
leaves23_org <- leaves23

# load treatment key
meta_plant <- read.csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")


# wanted columns & data types
final_columns <- c("unique_plant_ID", "species", "date_collection", "wet_mass", "dry_mass", "leaf_thickness", "leaf_area", "SLA", 
                   "LDMC", "sto_density_top", "sto_density_bot", "C_N", "C13")

numeric_cols <- c("wet_mass", "dry_mass", "leaf_thickness", "leaf_area", "SLA", "LDMC", "sto_density_top", "sto_density_bot", "C_N", "C13")
string_cols <- c("unique_plant_ID", "species")
date_cols <- c("date_collection")


################################################################################
### 2022 #######################################################################
################################################################################


### PREPARATION: COLUMNS, DATA CLASSES #########################################

leaves22 <- leaves22_org

# check data classes
str(leaves22)

# change column names: get column names
dput(colnames(leaves22))

# change them to new names
leaves22 <- leaves22 %>%
  rename("site" = "Site",
         "species" = "Species",
         "wet_mass" = "Freshweight",
         "dry_mass" = "Dryweight",
         "leaf_area" = "sumarea",
         "position_ID_original" = "Position")

# change variables in columns to standard: change species names to all lower, create collection date, change site names, separate Block.Plot etc.
leaves22 <- leaves22 %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names
         species = tolower(species),
         date_collection = DateFresh - as.difftime(1, unit="days")) %>% # freshweight always 1 day after leaf collection  
  separate_wider_delim(Block.Plot, delim = ".", names = c("block_ID_original", "plot_ID_original")) %>%
  mutate(block_ID_original = as.integer(block_ID_original),
         plot_ID_original = as.integer(plot_ID_original))


### ADD TREATMENTS ETC. ########################################################

# select oldest ind_number for all positions
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup() # exactly 1800, nice

# merge treatments to 2022 size data frame
leaves22_full <- full_join(leaves22, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# nice! all rows in leaves22 have a match in the meta_plant2022_23, so do only left_join
leaves22 <- left_join(leaves22, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))



### PROBLEM FIXING #############################################################

# go through all the comments

unique(leaves22$Comments)

# "keine Öhrchen"/ leaflet missing: ok, is when Medlup stipules were lost
# yellowish/ yellow/ any color remarks/ spots/ etc.: ignore
# 2/ 3/ 4/ etc. pieces: ignore, just a help for weighting
# herbivory damage/ old leaf/ hole/ tear/ etc.: potentially relevant, but ignore for now
# type N/ type R: this is the two types for Scabiosa --> ignore here, but might be necessary to add comment to focal metadata or add comment to data paper
# pressed: not relevant, ignore

# check for dublicated unique ID
any(is.na(leaves22$unique_plant_ID)) # none, great

# check levels
leaves22_levels <- leaves22 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################


# check whether there are any mistaken NA in metadata columns
leaves22_na <- leaves22 %>% 
  filter(is.na(unique_plant_ID) | is.na(date_collection) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 data)
leaves22_na <- leaves22 %>% 
  filter(is.na(leaf_area) | is.na(dry_mass) | is.na(wet_mass)) # none, nice


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################


# check columns
str(leaves22)

# add empty columns
missing_columns22 <- setdiff(final_columns, colnames(leaves22)) # find out which column are missing

leaves22[ , missing_columns22] <- NA # add those columns filled with NAs
leaves22_complete <- leaves22

# delete all columns not in final data frame plus rearrange in right order
leaves22 <- leaves22 %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
leaves22 <- leaves22 %>% 
  mutate(across(c(1:6), \(x) str_remove_all(x, pattern = fixed(" "))))


# change data types
leaves22 <- leaves22 %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_collection = as.Date(date_collection, format = "%Y-%m-%d")) # no new NA's, great

str(leaves22)

# final NA check
leaves22_na <- leaves22 %>%
  filter(if_any(c(1:5, 7), is.na)) # nicee!



################################################################################
### 2023 #######################################################################
################################################################################

### PREPARATION: COLUMNS, DATA CLASSES #########################################

leaves23 <- leaves23_org

# check data classes
str(leaves23)

# change column names: get column names
dput(colnames(leaves23))

# change them to new names
leaves23 <- leaves23 %>%
  rename("site" = "Site",
         "species" = "Species",
         "wet_mass" = "Freshweight",
         "dry_mass" = "Dryweight",
         "leaf_area" = "sumarea",
         "position_ID_original" = "Position",
         "date_collection" = "Date_Collected")

# change variables in columns to standard: change species names to all lower, change site names, separate Block.Plot, take mean for leaf thickness
leaves23 <- leaves23 %>%
  mutate(site = case_when(site == "Nes" ~ "lo" , site == "Cal" ~ "hi"), # change site names
         species = tolower(species)) %>% # freshweight always 1 day after leaf collection  
  separate_wider_delim(Block.Plot, delim = ".", names = c("block_ID_original", "plot_ID_original")) %>%
  mutate(block_ID_original = as.integer(block_ID_original),
         plot_ID_original = as.integer(plot_ID_original)) %>%
  rowwise() %>%
  mutate(leaf_thickness = mean(c_across(starts_with("Leaf_Thickness")), na.rm = TRUE),
         leaf_thickness = ifelse(is.nan(leaf_thickness), NA, leaf_thickness)) %>% # NA's are from leaves which were only collected for stomatal density and not for SLA etc. (so ok)
  ungroup()

# prepare stomatal density: transform microm into mm, calculate no. stomata/ mm
leaves23 <- leaves23 %>%
  mutate(sto_density_bot = as.numeric(Count_bottom)/as.numeric(Area_bottom) * 1e6,
         sto_density_top = as.numeric(Count_top)/as.numeric(Area_top) * 1e6)


### ADD TREATMENTS ETC. ########################################################

# merge treatments to 2022 size data frame
leaves23_full <- full_join(leaves23, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# there's a row only in leaves23, which is it?
leaves23_full[is.na(leaves23_full$unique_plant_ID) == TRUE,] # silvul6 in Nes_1.1 --> there is no silvul6! must be silvul20 as 11 and 22 are both marked in YearlySize as heavily damaged by herbivory
leaves23[leaves23$site == "lo" & leaves23$block_ID_original == 1 & leaves23$block_ID_original == 1 & leaves23$species == "silvul" & leaves23$position_ID_original == 6, ]$position_ID_original <- 20

leaves23_full <- full_join(leaves23, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species")) # now it's alright

# nice! all rows in leaves23 have a match in the meta_plant2022_23, so do only left_join
leaves23 <- left_join(leaves23, meta_plant2022_23, by = c("site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))


### PROBLEM FIXING #############################################################

# go through all the comments from weighting etc.

unique(leaves23$Comments_SLA)

# position corrected/ Plot ID is uncertain/ ID hergeleitet: was corrected while typing, is alright
# herbivory: ignore
# 2x/ 2 Teile/ 3x: help for weighting, irrelevant
# Rosettenblatt/ grundständig/ Stengelblatt: mention in paper? 

leaves23[leaves23$Comments_SLA == "Rosettenblatt" & is.na(leaves23$Comments_SLA) == FALSE,] # Rosettenblatt is actually wanted for Cenjac, so ok
leaves23[leaves23$Comments_SLA == "grundständig" & is.na(leaves23$Comments_SLA) == FALSE,] # this should maybe be mentioned?
leaves23[leaves23$Comments_SLA == "Stengelblatt" & is.na(leaves23$Comments_SLA) == FALSE,] # same, for Scacol Rosettenblätter were collected
leaves23[grepl("eed", leaves23$Comments_SLA) == TRUE & is.na(leaves23$Comments_SLA) == FALSE,] # is both for Medlup - ignore, not possible to discern between potential seedlings and original focal individuals


# check for dublicated unique ID
any(is.na(leaves23$unique_plant_ID)) # none, great

# check levels
leaves23_levels <- leaves23 %>%
  map(~str_c(unique(.x),collapse = ",")) %>% 
  bind_rows() %>% 
  gather(key = col_name, value = col_unique) # looks ok


### MISSING ENTRIES/ VALUES/ NA's ##############################################


# check whether there are any mistaken NA in metadata columns
leaves23_na <- leaves23 %>% 
  filter(is.na(unique_plant_ID) | is.na(date_collection) | is.na(species)) # all good

# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 data)
leaves23_na <- leaves23 %>% 
  filter(is.na(leaf_area) | is.na(dry_mass) | is.na(wet_mass) | is.na(leaf_thickness)) # those are all leaves collected for stomatal density only, so ok

# filter out leaves which were collected for stomatal density but none of the pics were good enough (i.e. both sto_density_top and sto_density_bot are NA)
leaves23 <- leaves23 %>%
  filter(!(is.na(sto_density_top) == TRUE & is.na(sto_density_bot) == TRUE & is.na(wet_mass) == TRUE))


### DELETE & ADD COLUMNS, DATATYPES, SAVE CLEAN VERSION ########################

# check columns
str(leaves23)

# add empty columns
missing_columns23 <- setdiff(final_columns, colnames(leaves23)) # find out which column are missing

leaves23[ , missing_columns23] <- NA # add those columns filled with NAs
leaves_23_complete <- leaves23

# delete all columns not in final data frame plus rearrange in right order
leaves23 <- leaves23 %>% 
  dplyr::select(any_of(final_columns))

# make sure to delete empty spaces in all columns
leaves23 <- leaves23 %>% 
  mutate(across(c(1:11), \(x) str_remove_all(x, pattern = fixed(" "))))


# change data types
leaves23 <- leaves23 %>%
  mutate(across(all_of(numeric_cols), as.numeric),
         across(all_of(string_cols), as.character),
         date_collection = as.Date(date_collection, format = "%Y-%m-%d")) # no new NA's, great

str(leaves23)

# final NA check
leaves23_na <- leaves23 %>%
  filter(if_any(c(1:7), is.na)) # the 49 leaves which were only collected for stomatal density, so ok


################################################################################
### 2021 - 2023 ################################################################
################################################################################


# add the 2022 and 2023 data together
leaves22_23 <- bind_rows(leaves22, leaves23)

# convert cm2 of leaf area into mm2 and g of dry and wet mass to mg
leaves22_23 <- leaves22_23 %>%
  mutate(leaf_area = leaf_area * 100,
         wet_mass_g = wet_mass, # needed for LDMC
         wet_mass = wet_mass * 1000,
         dry_mass = dry_mass * 1000)

# calculate SLA (leaf area/ dry mass) and LDMC (dry mass (mg)/ wet mass (g))
leaves22_23 <- leaves22_23 %>%
  mutate(SLA = leaf_area/dry_mass,
         LDMC = dry_mass/wet_mass_g) 

### CONTROL PLOTTING AND MODELLING #############################################

leaves22_23_plot <- leaves22_23 %>% 
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16),
         site = str_sub(unique_plant_ID, 5, 6))

leaves22_23_plot <- leaves22_23_plot %>%
  pivot_longer(cols = 4:14, names_to = "variable", values_to = "values")

ggplot(data = leaves22_23_plot[leaves22_23_plot$site == "lo",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = leaves22_23_plot[leaves22_23_plot$site == "hi",], aes(x = variable, y = values)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = leaves22_23_plot, aes(x = variable, y = values, fill = site)) +
  geom_boxplot() +
  facet_grid(species~treat_comp_warm, scales = "free")

ggplot(data = leaves22_23_plot[leaves22_23_plot$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_boxplot() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = leaves22_23_plot[leaves22_23_plot$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# check whether dry mass is heavier than the wet mass
dry_wet <- leaves22_23 %>%
  filter(dry_mass > wet_mass)

# leaves were re-weighted --> correct
a <- leaves_23_complete[leaves_23_complete$unique_plant_ID == "CHE.hi.warm.vege.wf.01.30.2",] # typing error dry mass (0.0082 instead of 0.082), but also re-weighted because evry small dry mass (0.0092)
leaves22_23[leaves22_23$unique_plant_ID == "CHE.hi.warm.vege.wf.01.30.2",]$dry_mass <- 0.0092 * 1000
a <- leaves_23_complete[leaves_23_complete$unique_plant_ID == "CHE.hi.warm.bare.wf.03.20.1",] # typing error dry mass
leaves22_23[leaves22_23$unique_plant_ID == "CHE.hi.warm.bare.wf.03.20.1",]$dry_mass <- 0.0098 * 1000


# go though extreme outliers for some of the species
brapin <- leaves22_23 %>%
  filter(species == "brapin")

a <- leaves22_complete[leaves22_complete$unique_plant_ID == "CHE.lo.ambi.bare.wf.06.08.2",] # check whether it's indeed so large --> looks alright

cenjac <- leaves22_23 %>%
  filter(species == "cenjac")

# CHE.lo.ambi.vege.wf.02.30.1: just generally really big leaf (dry mass, wet mass & area)
# CHE.hi.warm.vege.wf.01.30.2: re-weighted for dry mass & corrected

daucar <- leaves22_23 %>%
  filter(species == "daucar")

# CHE.lo.ambi.vege.wf.08.02.1: just very big leaf?
a <- leaves_23_complete[leaves_23_complete$unique_plant_ID == "CHE.lo.ambi.vege.wf.08.02.1",] # check whether it's indeed so large --> yes


salpra <- leaves22_23 %>%
  filter(species == "salpra")

# CHE.lo.ambi.bare.wf.05.28.2: just very big leaf?
a <- leaves_23_complete[leaves_23_complete$unique_plant_ID == "CHE.lo.ambi.bare.wf.05.28.2",] # check whether it's indeed so large --> yes indeed

silvul <- leaves22_23 %>%
  filter(species == "silvul")


# CHE.hi.ambi.vege.wf.05.22.1: very small LDMC
a <- leaves22_complete[leaves22_complete$unique_plant_ID == "CHE.hi.ambi.vege.wf.05.22.1",] # very small leaf, looks similar to Cal_8.2 silvul22, which has a much smaller wet mass --> delete? leave for now
b <- leaves22_complete[leaves22_complete$site == "hi" & leaves22_complete$species == "silvul" & leaves22_complete$block_ID_original == 8,]

# CHE.hi.warm.bare.wf.03.20.1: very high LDMC
a <- leaves_23_complete[leaves_23_complete$unique_plant_ID == "CHE.hi.warm.bare.wf.03.20.1",] # corrected above

# re-calculate LDMC and SLA with corrected values
# calculate SLA (leaf area/ dry mass) and LDMC (dry mass (mg)/ wet mass (g))
leaves22_23.2 <- leaves22_23 %>%
  mutate(SLA = leaf_area/dry_mass,
         LDMC = dry_mass/wet_mass_g) %>%
  dplyr::select(-wet_mass_g) # not needed anymore

# re-plot
leaves22_23_plot.2 <- leaves22_23.2 %>% 
  mutate(treat_comp_warm = str_sub(unique_plant_ID, 8, 16),
         site = str_sub(unique_plant_ID, 5, 6))

leaves22_23_plot.2 <- leaves22_23_plot.2 %>%
  pivot_longer(cols = 4:13, names_to = "variable", values_to = "values")

ggplot(data = leaves22_23_plot.2[leaves22_23_plot.2$site == "hi",], aes(x = variable, y = values, fill = treat_comp_warm)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # better, leave for now


### SAVE CLEAN VERSION #########################################################

write.csv(leaves22_23.2, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Leaf Traits/RangeX_clean_LeafTraits_2022_2023_CHE.csv",
          row.names = FALSE)





