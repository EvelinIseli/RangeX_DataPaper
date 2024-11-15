################################################################################
### DATA CLEANING SCRIPT: METADATA PREPARATION #################################
################################################################################

################################################################################

### Data used           : RangeX_Metadata_21_22_ZAF.csv, RangeX_Metadata_22_23_ZAF.csv, RangeX_Metadata_PlantID_ZAF.csv
### Date last modified  : 14.11.2024
### Purpose             : Clean metadata of ZAF of 2021/22 and 2022/23, add unique_plant_ID (this is the index identifying 
###                       replaced plants at identical positions: 1 = planted in 2021, 2 = planted in 2022) to metadata.

################################################################################



### packages etc. ##############################################################

library(tidyverse) # instead of tidyr, stringr etc. (data manipulation)
library(janitor) # clean up data (i.e. get rid of empty spaces)
library(tidylog) # how many lines of data deleted/ manipulated etc.
library(remotes) # intsall form github

#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# download data from OSF to computer
get_file(node = "bg2mu",
         file = "RangeX_Metadata_21_22_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF/raw")

get_file(node = "bg2mu",
         file = "RangeX_Metadata_22_23_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF/raw")

get_file(node = "bg2mu",
         file = "RangeX_Metadata_PlantID_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF/raw")

get_file(node = "bg2mu",
         file = "RangeX_PlotMetadata_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF/raw")


# import data into R studio
# plant metadata
meta_ZAF21_raw <- read_delim("data/ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
  clean_names()

meta_ZAF22_raw <- read_csv("data/ZAF/RangeX_Metadata_22_23_ZAF.csv") %>%  
  clean_names()

# plant index
plant_id_ZAF_raw <- read_csv("data/ZAF/RangeX_Metadata_PlantID_ZAF.csv") %>%
  clean_names()

# plot metadata
plot_id_ZAF_raw <- read_csv("data/ZAF/RangeX_PlotMetadata_ZAF.csv") %>%
  clean_names()

meta_ZAF21 <- meta_ZAF21_raw
meta_ZAF22 <- meta_ZAF22_raw
plant_id_ZAF <- plant_id_ZAF_raw
plot_id_ZAF <- plot_id_ZAF_raw


################################################################################
### PLOT METADATA ##############################################################
################################################################################

# create padded block variable & unique_plot_id
plot_id_ZAF <- plot_id_ZAF %>%
  mutate(block_id = str_pad(block_id_original, 2, pad = "0"),
         unique_plot_id = paste(region, site, treat_warming, treat_competition, added_focals, block_id, sep = "."))

# 80 unique values, 10 block values --> good

# check how many plot per block
plot_id_summary <- plot_id_ZAF %>%
  group_by(region, site, block_id_original) %>%
  summarize(no_plots = n())

# all good


################################################################################
### PLANT METADATA #############################################################
################################################################################


### PREPARE FILE ###############################################################

# the plot_id - treatment combinations are wrong in the raw plant metadata --> use plot metadata instead
meta_ZAF21 <- meta_ZAF21_raw %>%
  rename("treat_warm_plant" = "treat_warming", "treat_comp_plant" = "treat_competition",
         "added_focals_plant" = "added_focals", "unique_plot_id_plant" = "unique_plot_id",
         "unique_position_id_plant" = "unique_position_id",
         "block_id_plant" = "block_id") %>%
  left_join(plot_id_ZAF, by = c("region", "site", "block_id_original", "plot_id_original")) %>%
  mutate(position_id = str_pad(position_id_original, 2, pad = "0"),
           unique_position_id = paste(unique_plot_id, position_id, sep = "."))

# the 20 non-matching rows are the controll plots

# check for duplicates
meta_ZAF1_dubli <- meta_ZAF21 %>%
  group_by(unique_position_id) %>% # unique_position_id_plant, unique_position_id none
  mutate(duplicated = n() > 1)

# extract the cases where unique_plot_id_plant != unique_plot_id
no_match <- meta_ZAF21 %>%
  filter(unique_position_id_plant != unique_position_id) # 48 cases

# clean up 
meta_ZAF_21 <- meta_ZAF21 %>%
  select(-contains("_plant"))

# add plant ID (planting index) to current 2021/ 22 metadata (which only has one position ID, 1)
meta_ZAF21_merged <- meta_ZAF21 %>%
  left_join(plant_id_ZAF[, c(1:3)], by = "unique_position_id" )
  
# PROBLEM: 3 rows are not matching

no_match2 <- anti_join(plant_id_ZAF[, c(1:3)], meta_ZAF21, by = "unique_position_id")

# corrections in plant_id_ZAF (double-checked with planting plans for plots)
# ZAF.hi.warm.vege.wf.02.09 needs to be corrected to ZAF.hi.warm.bare.wf.02.09
# ZAF.hi.warm.bare.wf.04.05 needs to be corrected to ZAF.hi.ambi.bare.wf.04.05
# ZAF.hi.warm.bare.wf.06.11 needs to be corrected to ZAF.hi.ambi.vege.wf.06.11

plant_id_ZAF <- plant_id_ZAF %>%
  mutate(unique_position_id = case_when(unique_position_id == "ZAF.hi.warm.barewf.02.09" ~ "ZAF.hi.warm.bare.wf.02.09",
                                        unique_position_id == "ZAF.hi.ambibare.wf.04.05" ~ "ZAF.hi.ambi.bare.wf.04.05",
                                        unique_position_id == "ZAF.hi.ambi.vegre.wf.06.11" ~ "ZAF.hi.ambi.vege.wf.06.11",
                                        .default = unique_position_id))

# merge again
meta_ZAF21_merged <- meta_ZAF21 %>%
  left_join(plant_id_ZAF[, c(1:3)], by = "unique_position_id" )  # now it's ok      


# make date into date, create new unique_plant_id (position_ID + plant_ID)
meta_ZAF21_merged <- meta_ZAF21_merged %>%
  rename("plant_id_original" = "plant_id_2021",
         "planting_date" = "planting_date_2021") %>%
  mutate(planting_date = as.Date("2021-11-17"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 


# do the same for the 2022/ 23 data

# the plot_id - treatment combinations are wrong in the raw plant metadata --> use plot metadata instead
meta_ZAF22 <- meta_ZAF22_raw %>%
  rename("treat_warm_plant" = "treat_warming", "treat_comp_plant" = "treat_competition",
         "added_focals_plant" = "added_focals", "unique_plot_id_plant" = "unique_plot_id",
         "unique_position_id_plant" = "unique_position_id",
         "block_id_plant" = "block_id") %>%
  left_join(plot_id_ZAF, by = c("region", "site", "block_id_original", "plot_id_original")) %>%
  mutate(position_id = str_pad(position_id_original, 2, pad = "0"),
         unique_position_id = paste(unique_plot_id, position_id, sep = "."))

# the 20 non-matching rows are the controll plots

# check for duplicates
meta_ZAF22_dubli <- meta_ZAF22 %>%
  group_by(unique_position_id_plant) %>% # unique_position_id_plant has dublicates, unique_position_id none
  mutate(duplicated = n() > 1)

# extract the cases where unique_plot_id_plant != unique_plot_id
no_match <- meta_ZAF22 %>%
  filter(unique_position_id_plant != unique_position_id) # 48 cases

# clean up 
meta_ZAF_22 <- meta_ZAF22 %>%
  select(-contains("_plant"))

# add plant ID (planting index) to current 2022/ 23 metadata (which only has one position ID, 1)
meta_ZAF22_merged <- meta_ZAF22 %>%
  left_join(plant_id_ZAF[, c(1, 4, 5)], by = "unique_position_id" )

# PROBLEM: none

# make date into date, create new unique_plant_id (position_ID + plant_ID)
meta_ZAF22_merged <- meta_ZAF22_merged %>%
  rename("plant_id_original" = "plant_id_2022",
         "planting_date" = "planting_date_2022") %>%
  mutate(planting_date = as.Date("2022-10-19"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 


# delete all rows with plant_id = 1 (they will already be in meta_ZAF21 with no change - otherwise there will be a wrong planting date)
meta_ZAF22_merged <- meta_ZAF22_merged %>%
  filter(plant_id_original != 1)

# check for NA's
meta_ZAF22_na <- meta_ZAF22_merged %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

meta_ZAF21_na <- meta_ZAF21_merged %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

# none, amazing!

# bind 21 and 22
meta_ZAF21_22 <- bind_rows(meta_ZAF21_merged, meta_ZAF22_merged) %>% 
  mutate(species = case_when(species == "aloemac" ~ "alomac", # last minute typo corrections
                             species == "leucser" ~ "leuser",
                             species == "zervis" ~ "xervis",
                             .default = species))

# done!

# check again
meta_ZAF2122_na <- meta_ZAF21_22 %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) # no NAs

meta_ZAF2122_dubli <- meta_ZAF21_22 %>% 
  distinct(unique_plant_id, .keep_all = TRUE) # all distinct



# save
write_csv(meta_ZAF21_22, "data/ZAF/RangeX_FocalMetadata_ZAF_clean.csv")
write_csv(plot_id_ZAF, "data/ZAF/RangeX_PlotMetadata_ZAF_clean.csv")




