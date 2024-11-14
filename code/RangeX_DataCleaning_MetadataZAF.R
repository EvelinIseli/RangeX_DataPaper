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
         remote_path = "metadata/ZAF")

get_file(node = "bg2mu",
         file = "RangeX_Metadata_22_23_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF")

get_file(node = "bg2mu",
         file = "RangeX_Metadata_PlantID_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF")


# import data into R studio
meta_ZAF21_raw <- read_delim("data/ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
  clean_names()

meta_ZAF22_raw <- read_csv("data/ZAF/RangeX_Metadata_22_23_ZAF.csv") %>%  
  clean_names()
#meta_ZAF22_raw <- read_csv("/Users/eviseli/Desktop/RangeX_Metadata_ZAF_22_23_final1.csv") %>%  
#  clean_names()


plant_id_ZAF_raw <- read_csv("data/ZAF/RangeX_Metadata_PlantID_ZAF.csv") %>%
  clean_names()

meta_ZAF21 <- meta_ZAF21_raw
meta_ZAF22 <- meta_ZAF22_raw
plant_id_ZAF <- plant_id_ZAF_raw


### PREPARE FILE ###############################################################

# make unique_position_ID new (there's dublicates, somehow), add plant ID to current 2021/ 22 metadata (which only has one position ID, 1)
meta_ZAF21 <- meta_ZAF21 %>%
  mutate(block_id = str_pad(block_id_original, 2, pad = "0"),
         position_id = str_pad(position_id_original, 2, pad = "0"),
         unique_position_id = paste("ZAF", site, treat_warming, treat_competition, added_focals, block_id, position_id, sep = ".")) %>%
  group_by(unique_position_id) %>%
  mutate(duplicated = n() > 1)
  
# PROBLEM: block, plot and position ID are unique, but the treatment specifications are not


# make date into date, create new unique_plant_id (position_ID + plant_ID)
meta_ZAF21 <- meta_ZAF21 %>%
  rename("plant_id_original" = "plant_id_2021",
         "planting_date" = "planting_date_2021") %>%
  mutate(planting_date = as.Date("2021-11-17"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 


# do the same for the 2022/ 23 data
# make unique_position_ID new (there's dublicates, somehow), add plant ID to current 2022/ 23 metadata (which only has a position ID)
meta_ZAF22 <- meta_ZAF22 %>%
  mutate(block_id = paste0("0", block_id_original),
         position_id_original = paste0("0", position_id_original),
         unique_position_id = paste("ZAF", site, treat_warming, treat_competition)) %>%
  left_join(plant_id_ZAF[, c(1, 4, 5)], by = "unique_position_id" )

# make date into date, create new unique_plant_id (position_ID + plant_ID)
meta_ZAF22 <- meta_ZAF22 %>%
  rename("plant_id_original" = "plant_id_2022",
         "planting_date" = "planting_date_2022") %>%
  mutate(planting_date = as.Date("2022-10-19"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 

# delete all rows with plant_id = 1 (they will already be in meta_ZAF21 with no change - otherwise there will be a wrong planting date)
meta_ZAF22 <- meta_ZAF22 %>%
  filter(plant_id_original != 1)

# check for NA's
meta_ZAF22_na <- meta_ZAF22 %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

meta_ZAF21_na <- meta_ZAF21 %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

# none, amazing!

# bind 21 and 22
meta_ZAF21_22 <- bind_rows(meta_ZAF21, meta_ZAF22) %>% 
  mutate(species = case_when(species == "aloemac" ~ "alomac",
                             species == "leucser" ~ "leuser",
                             species == "zervis" ~ "xervis",
                             .default = species))

# done!

# save
write_csv(meta_ZAF21_22, "data/ZAF/RangeX_Metadata_ZAF_clean.csv")




