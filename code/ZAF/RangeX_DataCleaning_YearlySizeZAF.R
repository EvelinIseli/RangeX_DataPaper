################################################################################
### DATA CLEANING SCRIPT: YEARLY SIZE 2021/ 22 #################################
################################################################################

################################################################################

### Data used           : 2021_2_data.xls, RangeX_Metadata_21_22_ZAF.csv
### Date last modified  : 31.10.2023
### Purpose             : Transform the raw 2021 yearly size data set from ZAF into the data paper format.

################################################################################

#rm(list = ls()) # emptying global environment


### packages etc. ##############################################################

library(tidyverse) # instead of tidyr, strinr etc. (data manipulation)
#install.packages("janitor")
library(janitor) # clean up data (i.e. get rid of empty spaces)
#install.packages("tidylog")
library(tidylog) # how many lines of data deleted/ manipulated etc.
library(dataDownloader)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# download data from OSF to computer
get_file(node = "bg2mu",
         file = "2021_2_data_lowSite_ZAF.csv",
         path = "data/ZAF",
         remote_path = "focal_level/ZAF/raw") # high site 2021/22

get_file(node = "bg2mu",
         file = "2021_2_data_highSite_ZAF.csv",
         path = "data/ZAF",
         remote_path = "focal_level/ZAF/raw") # high site 2021/22

get_file(node = "bg2mu",
         file = "2023_lower1_ZAF.csv",
         path = "data/ZAF",
         remote_path = "focal_level/ZAF/raw") # high site 2022/23

get_file(node = "bg2mu",
         file = "2023_upper1_ZAF.csv",
         path = "data/ZAF",
         remote_path = "focal_level/ZAF/raw") # high site 2022/23

get_file(node = "bg2mu",
         file = "RangeX_Metadata_ZAF_clean.csv",
         path = "data/ZAF",
         remote_path = "metadata/ZAF")




# import data into R studio
# load demographic data
raw_dat_YS21_lo <- read_delim("data/ZAFdata_24/2021_2_data_lowSite_ZAF.csv") %>%
  clean_names() |> 
  mutate(species = str_trim(species, "left"),
         species = str_trim(species, "right"))

raw_dat_YS21_hi <- read_delim("data/ZAFdata_24/2021_2_data_highSite_ZAF.csv") %>%
  clean_names() |> 
  mutate(species = str_trim(species, "left"),
         species = str_trim(species, "right"))

raw_dat_YS23_lo <- read_csv("data/ZAFdata_24/2023_lower1_ZAF.csv") %>%
  clean_names()

raw_dat_YS23_hi <- read_csv("data/ZAFdata_24/2023_upper1_ZAF.csv") %>%
  clean_names()

key_raw <- read_delim("data/ZAF/RangeX_Metadata_ZAF_clean.csv") %>%
  clean_names()


# define useful vector

# wanted columns & data types
final_columns <- c("functional_group", "date_measurement", "date_planting", "collector", "survival", "height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                   "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                   "mean_inflorescence_size", "herbivory")

integer_cols <- c("height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                  "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                  "mean_inflorescence_size")
date_cols <- c("date_measurement", "date_planting")
string_cols <- c("collector")
factor_cols <- c("herbivory")



################################################################################
### 2021/ 22  ##################################################################
################################################################################

dat_YS21_lo <- raw_dat_YS21_lo
dat_YS21_hi <- raw_dat_YS21_hi

### CLEAN COLUMN NAMES & DATA CLASSES ##########################################

# delete unnecessary columns, make plot id column identical in both low and high data frames, add site column
dat_YS21_hi <- dat_YS21_hi %>%
  dplyr::select(-treat_veg, -treat_otc, -id) %>%
  mutate(plot = gsub("[[:upper:]]", "", plot),
         plot = gsub("^\\.", "", plot),
         across(matches("^(vh|nlc|lll|vw|nb|dia)"), as.numeric),
         site = "hi")

dat_YS21_lo <- dat_YS21_lo %>%
  dplyr::select(-date, -treatment) %>%
  mutate(plot = gsub("[[:upper:]]", "", plot),
         plot = gsub("^\\.", "", plot),
         across(matches("^(vh|nlc|lll|vw|nb|dia)"), as.numeric),
         site = "lo")

# merge high and low data sets
dat_YS21 <- bind_rows(dat_YS21_hi, dat_YS21_lo)


# make long format
dat_YS21 <- dat_YS21 %>%
  pivot_longer(
    cols = !c(flunctional_group, species, plot,  position, site),
    names_to = c("variable", "month", "year"),
    names_sep = "_",
    values_to = "value") %>% 
  pivot_wider(names_from = variable,
              values_from = value)

# make some manipulations to be able to merge key on
dat_YS21 <- dat_YS21 %>%
  mutate(region = "ZAF") %>%
  separate_wider_delim(plot, delim = ".", names = c("block_id_original", "plot_id_original")) #%>%
### SPECIES NEEDS TO BE REMOVED HERE EVENTUALLY!!!
  #dplyr::select(-"species") #-"month", -"year", 



# change them to new names
dat_YS21 <- dat_YS21 %>%
  rename("height_vegetative_str" = "vh",
         "number_leaves" = "nlc",
         "leaf_length1" = "lll",
         "vegetative_width" = "vw",
         "number_branches" = "nb",
         "stem_diameter" = "dia",
         "functional_group" = "flunctional_group",
         "position_id_original" = "position")


# get empty columns
present_columns <- colnames(dat_YS21)
missing_col <- setdiff(final_columns, present_columns)

dat_YS21[, missing_col] <- NA


### ADD TREATMENTS ETC. ########################################################

# prepare treatment key
key <- key_raw %>%
  filter(region == "ZAF") %>%
  mutate(block_id_original = as.character(block_id_original),
         position_id_original = as.integer(position_id_original),
         plot_id_original = as.character(plot_id_original),
         plant_id_original = as.character(plant_id_original))

# filter out 2021/ 22 metadata (there will be merging problems otherwise)
key21 <- key %>%
  filter(planting_date == "2021-11-17")

# merge treatments to 2021 size data frame
dat_YS21_merged <- left_join(dat_YS21 |> 
                               ### RENAME NEEDS NOT TO HAPPEN IN THE FINAL VERSION WHEN SPECIES IN THE DATA IS REMOVED!!!
                               rename("sp" = species), key21, by = c("region", "site", "block_id_original", "plot_id_original", "position_id_original")) |> 
  mutate(species = str_trim(species, "left"),
         species = str_trim(species, "right"))


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS21_na <- dat_YS21_merged %>% 
  filter(is.na(unique_plant_id) | is.na(year) | is.na(species)) 

# nothing! clean for now


################################################################################
### 2022/ 23  ##################################################################
################################################################################

dat_YS23_lo <- raw_dat_YS23_lo
dat_YS23_hi <- raw_dat_YS23_hi

### CLEAN COLUMN NAMES & DATA CLASSES ##########################################
  
# delete unnecessary columns, make plot id column identical in both low and high data frames, add site column
dat_YS23_hi <- dat_YS23_hi %>%
  #dplyr::select(-treat_veg, -treat_otc, -date, -id, -block, -plot_2) %>%
  dplyr::select(-treat_veg, -treat_otc, -x85) %>%
  separate_wider_delim(plot, delim = ".", names = c("block_id_original", "plot_id_original")) %>%
  mutate(site = "hi",
         #across(all_of(integer_cols_hi), as.numeric),
         across(all_of(c("block_id_original", "plot_id_original", "position")), as.character)) |>
  mutate(across(matches("^(vh|ncl|lll|vw|nb|dia)"), as.numeric)) |> 
  rename("position_id_original" = position,
         "functional_group" = flunctional_group)

dat_YS23_lo <- dat_YS23_lo %>% 
  #dplyr::select(-date, -treatment, -x2) %>%
  dplyr::select(-x3, -treatment, -x6) %>% 
  separate_wider_delim(plot, delim = ".", names = c("region", "block_id_original", "plot_id_original", "position_id_original")) %>%
  mutate(site = "lo",
         #across(all_of(integer_cols_lo), as.numeric),
         across(all_of(c("block_id_original", "plot_id_original", "position_id_original")), as.character)) |> 
  mutate(across(matches("^(vh|ncl|lll|vw|nb|dia)"), as.numeric))



### check warning: "mutate: converted 'vw_oct_22' from character to double (2 new NA) 
### converted 'vw_march_23' from character to double (1 new NA)



# merge high and low data sets
dat_YS23 <- bind_rows(dat_YS23_hi, dat_YS23_lo)

# make long format
dat_YS23 <- dat_YS23 %>%
  pivot_longer(
    cols = !c(functional_group, region, species, block_id_original, plot_id_original, position_id_original, site),
    names_to = c("variable", "month", "year"),
    names_sep = "_",
    values_to = "value") %>%
  pivot_wider(names_from = variable,
              values_from = value)


# make some manipulations to be able to merge key on
dat_YS23 <- dat_YS23 %>%
  mutate(region = "ZAF") #%>%
### SPECIES NEEDS TO BE REMOVED HERE EVENTUALLY!!!
  #dplyr::select(-"species") #-"month", -"year", 



# change them to new names
dat_YS23 <- dat_YS23 %>%
  rename("height_vegetative_str" = "vh",
         "number_leaves" = "nlc",
         "leaf_length1" = "lll",
         "vegetative_width" = "vw",
         "number_branches" = "nb",
         "stem_diameter" = "dia")


# get empty columns
present_columns <- colnames(dat_YS23)
missing_col <- setdiff(final_columns, present_columns)

dat_YS23[, missing_col] <- NA


### ADD TREATMENTS ETC. ########################################################


key23 <- key_raw %>%
  filter(region == "ZAF") |> 
  mutate(position_id_original = as.character(position_id_original),
         block_id_original = as.character(block_id_original),
         plot_id_original = as.character(plot_id_original)) |> 
  # grab plant replacement, group by unique position id and filter for largest number, which is the plant that was replaced
  mutate(ind_nr = stringr::str_extract(unique_plant_id, stringr::regex("(\\d+)(?!.*\\d)"))) |>
  group_by(unique_position_id, region, site, block_id_original, plot_id_original, position_id_original, species) |>
  tidylog::summarise(ind_nr = max(ind_nr))



# merge treatments to 2021 size data frame
dat_YS23_merged <- tidylog::left_join(dat_YS23 |> 
                                        ### RENAME NEEDS NOT TO HAPPEN IN THE FINAL VERSION WHEN SPECIES IN THE DATA IS REMOVED!!!
                     rename("sp" = species), key23, by = c("region", "site", "block_id_original", "plot_id_original", "position_id_original"))



# check if species names are correct from data and metadata
dat_YS23_merged |> count(species, sp) |> arrange(species) |> print(n = Inf)
dat_YS21_merged |> count(species, sp) |> arrange(species) |> print(n = Inf)




### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS23_na <- dat_YS23_merged %>% 
  filter(is.na(unique_plant_id) | is.na(year) | is.na(species)) 

# nothing! clean for now
















### DELETE COLUMNS & CHANGE DATATYPES ##########################################


## delete all columns not in final data frame plus rearrange in right order
#dat_YS21 <- dat_YS21 %>% 
#  dplyr::select(any_of(final_columns))

# change data types
dat_YS21_merged <- dat_YS21_merged %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character))


