################################################################################
### DATA CLEANING SCRIPT: Vegetation Surveys ###################################
################################################################################

################################################################################

### Data used           : RangeX_raw_VegSurv_2021.csv, RangeX_raw_VegSurv_2022.csv, RangeX_raw_VegSurv_2023.csv, RangeX_raw_VegSurv_2024.csv, RangeX_clean_MetadataPlot_CHE.csv, RangeX_raw_VegSurv_corrections.csv
### Date last modified  : 19.11.2024
### Purpose             : Go through all yearly data sets and correct metadata, align species over years, deal with grass/ carex group, sort out unidentified plants, add all yearly data together.

################################################################################

#rm(list = ls()) 


### packages etc. ##############################################################

# basic packages
library(tidyverse); library(tidylog) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library(lubridate) # dates

### functions etc. ##############################################################

# function to detect "empty" cells (cells containing only spaces)
is.empty <- function(x) {
  # check if the string is only spaces or empty after trimming spaces
  trimws(x) == ""
}

# function to detect leading or trailing spaces
lead.trail.spaces <- function(x) {
  # check if there are spaces at the beginning or end of the string
  grepl("^\\s|\\s$", x)
}

# function to negate %in%
`%nin%` <- Negate(`%in%`)


### LOAD DATA SET ##############################################################

# load vegetation data
dat2021_raw <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/VegSurv 2021/RangeX_raw_VegSurv_2021.csv")
dat2022_raw <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/VegSurv 2022/RangeX_raw_VegSurv_2022.csv")
dat2023_raw <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/VegSurv 2023/RangeX_raw_VegSurv_2023.csv")
dat2024_raw <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/VegSurv 2024/RangeX_raw_VegSurv_2024.csv")

# load metadata key
plot_key <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv")

# load manual corrections and additions for graminoids
corrections <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/RangeX_raw_VegSurv_corrections.csv")

# wanted columns & data types
final_columns <- c("unique_plot_ID", "date_measurement", "species", "cover", "collector", "only_focals")

string_cols <- c("unique_plot_ID", "species", "cover", "collector", "only_focals")
date_cols <- c("date_measurement")


################################################################################
### 2021 #######################################################################
################################################################################

dat2021 <- dat2021_raw

# make a year and a Genus_species variable, delete typer, genus, species and "how many dead focal" rows
dat2021 <- dat2021 %>%
  mutate(Date = dmy(Date),
         Species = if_else(is.na(Species) == TRUE, "", Species),
         Genus_species = paste(Genus, Species, sep = " "),
         across(where(is.character), ~str_trim(., side = "both"))) %>%
  dplyr::select(-Typer, -Genus, -Species, -Dead_Focals)

# check out unique values
lapply(dat2021, unique)

# fix 
# 1) missing collector information --> take from other plots
# 2) trailing spaces

# there's empty collector values (also empty typers, but that's alrigth)
dat2021[is.na(dat2021$Collector),] 
unique(dat2021[dat2021$Site == "Cal" & dat2021$Block_Number == 2 & dat2021$Plot_Number == 2,]$Collector) # check out the collector of this plot
dat2021[is.na(dat2021$Collector),]$Collector <- "CB"

# for some species and genuses there's an empty space at the end - get rid of it!
dat2021 <- dat2021 %>%
  mutate(across(where(is.character), ~str_trim(., side = "both")))

# check for NAs in important columns 
dat2021_na <- dat2021 %>% 
  filter(is.na(Date) | is.na(Site) | is.na(Block_Number) | is.na(Plot_Number) | is.na(Genus_species) | is.na(Percentage_Cover) | is.na(Collector)) # 2 empty grass groups - delete rows

dat2021 <- dat2021 %>%
  filter(!is.na(Percentage_Cover))

# check for empty cells which shouldn't be empty
dat2021_empty <- dat2021 %>% 
  filter(is.empty(Date) | is.empty(Site) | is.empty(Block_Number) | is.empty(Plot_Number) | is.empty(Genus_species) | is.empty(Percentage_Cover) | is.empty(Collector)) # none

# check for duplicates (complete and only in ID columns)
dat2021_dubli <- dat2021 %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # can be deleted, exact duplicates

dat2021_dubli <- dat2021 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # some - check in data sheets and delete wrong row (or transform it in case of AEG), delete true duplicates

dat2021 <- dat2021 %>%
  filter(!(Site == "Cal" & Block_Number == 3 & Plot_Number == 1 & Genus_species == "Galium anisophyllon" & Percentage_Cover == "2")) %>%
  mutate(Genus_species = ifelse(Site == "Cal" & Block_Number == 6 & Plot_Number == 3 & Genus_species == "AEG" & Percentage_Cover == "<0.5", "Primula auricula", Genus_species)) %>%
  filter(!duplicated(.)) # remove "true" duplicates

dat2021_dubli <- dat2021 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # none

# check for leading or trailing spaces
dat2021_spaces <- dat2021 %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none


################################################################################
### 2022 #######################################################################
################################################################################

dat2022 <- dat2022_raw

# delete weird columns, transform the Comment and Group_Species column characters to UTF-8 encoding (contains ö's!), correct wrong date (2023-07-18 is 2022-07-18)
dat2022 <- dat2022 %>%
  dplyr::select(-`...15`, -`...16`) %>%
  mutate(across(where(is.character), ~iconv(., to = "UTF-8", sub = "byte")),
         Date = if_else(Date == "07/18/2023", "07/18/2022", Date)) %>%
  dplyr::select(-Typer, -Genus, -Species, -Dead_Focals)

# check out unique values
lapply(dat2022, unique)

# fix problems 
# 1) get rid of row with question mark as percentage
# 2) change <0,5 to <0.5
# 3) sort out <0.5 + number for Centaurea at Nesselboden (it's experimental Cetaurea jacea + Centaurea jacea angustifolia, is separtaed in other years)
# 4) delete rows with x as percentage (it's the focal species in hi site plots where no focal species were planted)
# 5) CB/EI to CB & EI

dat2022 <- dat2022 %>%
  filter(Percentage_Cover != "?" & Percentage_Cover != "x") %>%
  mutate(Percentage_Cover = ifelse(Percentage_Cover == "<0,5", "<0.5", Percentage_Cover),
         Collector = ifelse(Collector == "CB/EI", "EI & CB", Collector))

dat2022[grepl("\\+", dat2022$Percentage_Cover) == TRUE,]
cenangi_6.1 <- data.frame(Date = "07.07.2022", Site = "Nes", Block_Number = as.integer(6), Plot_Number = as.integer(1), Genus = "Centaurea", Species = "jacea angustifolia", Genus_species = "Centaurea jacea angustifolia", 
                 Percentage_Cover = "1", Only_Focals = NA, Comment = NA, Group_Species = NA, Dead_Focals = NA, Collector = "CB", Typer = "TL")
cenangi_8.2 <- data.frame(Date = "07.07.2022", Site = "Nes", Block_Number = as.integer(8), Plot_Number = as.integer(2), Genus = "Centaurea", Species = "jacea angustifolia", Genus_species = "Centaurea jacea angustifolia", 
                 Percentage_Cover = "<0.5", Only_Focals = NA, Comment = NA, Group_Species = NA, Dead_Focals = NA, Collector = "EI", Typer = "TL")

dat2022 <- bind_rows(dat2022, cenangi_6.1, cenangi_8.2) %>%
  mutate(Dead_Focals = ifelse(grepl("\\+", Percentage_Cover) == TRUE, 0, Dead_Focals),
         Percentage_Cover = ifelse(grepl("\\+", Percentage_Cover) == TRUE, "<1", Percentage_Cover))


# fix problem with date: there's both mm/dd/yyyy and mm.dd.yyyy (- see whether lubridate can handle it!) lubridate can handle it
dat2022 <- dat2022 %>%
  mutate(Date = mdy(Date))

# check for NAs in important columns 
dat2022_na <- dat2022 %>% 
  filter(is.na(Date) | is.na(Site) | is.na(Block_Number) | is.na(Plot_Number) | is.na(Genus_species) | is.na(Percentage_Cover) | is.na(Collector)) # none

# check for empty cells which shouldn't be empty
dat2022_empty <- dat2022 %>% 
  filter(is.empty(Date) | is.empty(Site) | is.empty(Block_Number) | is.empty(Plot_Number) | is.empty(Genus_species) | is.empty(Percentage_Cover) | is.empty(Collector)) # none

# check for duplicates  (complete and only in ID columns)
dat2022_dubli <- dat2022 %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # can be deleted, exact duplicates

dat2022_dubli <- dat2022 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # some - check in data sheets and delete wrong row and delete true duplicates

dat2022 <- dat2022 %>%
  filter(!duplicated(.)) %>% # remove "true" duplicates
  filter(!(Site == "Nes" & Block_Number == 10 & Plot_Number == 2 & Genus_species == "Carduus defloratus" & Percentage_Cover == "<1"),
         !(Site == "Cal" & Block_Number == 9 & Plot_Number == 3 & Genus_species == "Phyteuma orbiculare" & Percentage_Cover == "<1")) %>%
  mutate(Percentage_Cover  = ifelse(Site == "Nes" & Block_Number == 10 & Plot_Number == 2 & Genus_species == "Carduus defloratus" & Percentage_Cover == "<0.5", "1", Percentage_Cover), # add the two together (<1 + <0.5)
         Percentage_Cover  = ifelse(Site == "Cal" & Block_Number == 9 & Plot_Number == 3 & Genus_species == "Phyteuma orbiculare" & Percentage_Cover == "<0.5", "1", Percentage_Cover)) # dito

dat2022_dubli <- dat2022 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # none


# check for leading or trailing spaces
dat2022_spaces <- dat2022 %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # many --> apply str_trim to all character columns

dat2022 <- dat2022 %>%
  mutate(across(where(is.character), ~str_trim(., side = "both")))

dat2022_spaces <- dat2022 %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none left



################################################################################
### 2023 #######################################################################
################################################################################


dat2023 <- dat2023_raw

# check out unique values
lapply(dat2023, unique)

# fix problems
# 1) NA covers --> focal species at high site in plots where no focal species were planted, delete
# 3) CB with a trailing whitespace, some whitespaces in percentage cover


dat2023 <- dat2023 %>%
  rename("Genus_species" = "Species", "Percentage_Cover" = "Percentage", "Block_Number" = "Block_ID", "Plot_Number" = "Plot_ID") %>%
  mutate(across(where(is.character), ~str_trim(., side = "both"))) %>% # get rid of any trailing/ leading spaces
  filter(Percentage_Cover != "NA") %>%
  mutate(Date = dmy(Date)) # make actual date


# check for NAs in important columns 
dat2023_na <- dat2023 %>% 
  filter(is.na(Date) | is.na(Site) | is.na(Block_Number) | is.na(Plot_Number) | is.na(Genus_species) | is.na(Percentage_Cover) | is.na(Collector)) # none

# check for empty cells which shouldn't be empty
dat2023_empty <- dat2023 %>% 
  filter(is.empty(Date) | is.empty(Site) | is.empty(Block_Number) | is.empty(Plot_Number) | is.empty(Genus_species) | is.empty(Percentage_Cover) | is.empty(Collector)) # none

# check for duplicates (complete and only in ID columns)
dat2023_dubli <- dat2023 %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # 8 rows - all exact duplicates, can be deleted

dat2023_dubli <- dat2023 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # same 8 rows

dat2023 <- dat2023 %>%
  filter(!duplicated(.)) # delete true duplicates

dat2023_dubli <- dat2023 %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # none
dat2023_dubli <- dat2023 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # none

# check for leading or trailing spaces
dat2023_spaces <- dat2023 %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none


################################################################################
### 2024 #######################################################################
################################################################################


dat2024 <- dat2024_raw

# check out unique values
lapply(dat2024, unique)

# no obvious problems (deal with NAs later)

# rename columns and create date, deal with spaces
dat2024 <- dat2024 %>%
  rename("Genus_species" = "Species", "Percentage_Cover" = "Percentage", "Block_Number" = "Block_ID", "Plot_Number" = "Plot_ID") %>%
  mutate(across(where(is.character), ~str_trim(., side = "both"))) %>% # get rid of any trailing/ leading spaces
  filter(Percentage_Cover != "NA") %>% # delete empty rows
  mutate(Date = dmy(Date)) # make actual date


# check for NAs in important columns 
dat2024_na <- dat2024 %>% 
  filter(is.na(Date) | is.na(Site) | is.na(Block_Number) | is.na(Plot_Number) | is.na(Genus_species) | is.na(Percentage_Cover) | is.na(Collector)) # none

# check for empty cells which shouldn't be empty
dat2024_empty <- dat2024 %>% 
  filter(is.empty(Date) | is.empty(Site) | is.empty(Block_Number) | is.empty(Plot_Number) | is.empty(Genus_species) | is.empty(Percentage_Cover) | is.empty(Collector)) # none

# check for duplicates (complete and only in ID columns)
dat2024_dubli <- dat2024 %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # none

dat2024_dubli <- dat2024 %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number), fromLast = TRUE)) # none

# check for leading or trailing spaces
dat2024_spaces <- dat2024 %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none


# ready


################################################################################
### COMBINE AND FIX PROBLEMS ###################################################
################################################################################

# explicitly select columns because apparently the world is weird
dat2021 <- dat2021 %>%
  select(Date, Site, Block_Number, Plot_Number, Genus_species, Percentage_Cover, Only_Focals, Comment, Group_Species, Collector)
dat2022 <- dat2022 %>%
  select(Date, Site, Block_Number, Plot_Number, Genus_species, Percentage_Cover, Only_Focals, Comment, Group_Species, Collector)
dat2023 <- dat2023 %>%
  rename("Comment" = "Comments") %>%
  select(Date, Site, Block_Number, Plot_Number, Genus_species, Percentage_Cover, Only_Focals, Comment, Group_Species, Collector)
dat2024 <- dat2024 %>%
  rename("Comment" = "Comments") %>%
  select(Date, Site, Block_Number, Plot_Number, Genus_species, Percentage_Cover, Only_Focals, Comment, Group_Species, Collector)

# combine them all
dat_comb <- bind_rows(dat2021, dat2022, dat2023, dat2024)

# last checks
dat_comb_na <- dat_comb %>% 
  filter(is.na(Date) | is.na(Site) | is.na(Block_Number) | is.na(Plot_Number) | is.na(Genus_species) | is.na(Percentage_Cover) | is.na(Collector)) # none

# check for empty cells which shouldn't be empty
dat_comb_empty <- dat_comb %>% 
  filter(is.empty(Date) | is.empty(Site) | is.empty(Block_Number) | is.empty(Plot_Number) | is.empty(Genus_species) | is.empty(Percentage_Cover) | is.empty(Collector)) # none

# check for duplicates (complete and only in ID columns)
dat_comb_dubli <- dat_comb %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # none

dat_comb_dubli <- dat_comb %>% 
  filter(duplicated(select(., Site, Genus_species, Block_Number, Plot_Number, Date)) | 
           duplicated(select(., Site, Genus_species, Block_Number, Plot_Number, Date), fromLast = TRUE)) # none

# check for leading or trailing spaces
dat_comb_spaces <- dat_comb %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none

# prepare data frame for adding treatments etc., then add on treatments and make plot ID, add year for checks
dat_comb <- dat_comb %>%
  rename("block_ID_original" = "Block_Number", "plot_ID_original" = "Plot_Number", "site" = "Site", "date_measurement" = "Date", 
         "species" = "Genus_species", "collector" = "Collector", "only_focals" = "Only_Focals", "cover" = "Percentage_Cover") %>%
  mutate(region = "CHE",
         site = ifelse(site == "Nes", "lo", "hi")) %>%
  left_join(plot_key, by = c("site", "block_ID_original", "plot_ID_original", "region")) %>% # 30 rows only in plot_key --> 30 bare plots, that's ok
  mutate(year = year(date_measurement))

  
## now plot in the turf-mapper-quarto (make_turfmaps_RangeX.qmd) --> save combined data
#write_csv(dat_comb, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/RangeX_raw_VegSurv_combined.csv")


# check whether all plots have data in all years
dat_comb_sum <- dat_comb %>%
  group_by(unique_plot_ID, block_ID_original, plot_ID_original) %>%
  summarize(no_years = list(unique(year))) # FIXED (2 plots with data missing in one year (CHE.lo.ambi.vege.wf.02 and CHE.hi.ambi.vege.wf.05 in 2022))

# is alright, in 2024 only plots with no focals surveyed

# now correct spelling
dat_comb <- dat_comb %>%
  mutate(species = case_when(grepl("Androsace cham", species) ~ "Androsace chamaejasme",
                             grepl("Helianthemum al", species) ~ "Helianthemum alpestre",
                             grepl("Hieracium lac", species) ~ "Hieracium lactucella",
                             grepl("Hieracium pi", species) ~ "Hieracium pilosella",
                             grepl("Galium an", species) ~ "Galium anisophyllon",
                             grepl("Vaccinium vi", species) ~ "Vaccinium vitis-idaea",
                             grepl("Flechte braun", species) ~ "Cetraria islandica",
                             grepl("Flechte weiss", species) ~ "Cladonia rangiferina",
                             grepl("Agrostis cap", species) ~ "Agrostis capillaris",
                             grepl("Bart", species) ~ "Bartsia alpina",
                             grepl("Campanula rapun", species) ~ "Campanula rapunculoides",
                             grepl("Carex car", species) ~ "Carex caryophyllea",
                             grepl("Carastium fon", species) ~ "Cerastium fontanum",
                             grepl("Certraria", species) ~ "Cetraria islandica",
                             grepl("Heriacium pil", species) ~ "Hieracium pilosella",
                             grepl("Lotus corn", species) ~ "Lotus corniculatus",
                             grepl("Moss", species) ~ "moss",
                             grepl("Phyteuma arbi", species) ~ "Phyteuma orbiculare",
                             grepl("Polygomun viviparum", species) ~ "Polygonum viviparum",
                             grepl("Trifolium mon", species) ~ "Trifolium montanum",
                             grepl("Vaccinium gault", species) ~ "Vaccinium gaultherioides",
                             grepl("Veronica cham", species) ~ "Veronica chamaedrys",
                             grepl("Andorsace cham", species) ~ "Androsace chamaejasme",
                             grepl("Anthoxanthum alpina", species) ~ "Anthoxanthum alpinum",
                             grepl("Phleum alpina", species) ~ "Phleum alpinum",
                             grepl("Ranunculus mon", species) ~ "Ranunculus montanus",
                             TRUE ~ species))

# check species list
species <- dat_comb %>%
  distinct(species)

# species seem to be alright

# make some turf plots to see whether some species match over time within the same plot (Agrostis spp. --> Agrostis stolonifera etc.)
unique_plots <- unique(dat_comb$unique_plot_ID) # get all plots
unique_siteblock <- unique(paste(dat_comb$site, dat_comb$block_ID, sep = "."))
dat_comb_mapper <- dat_comb %>% # make cover numeric
  mutate(cover_num = case_when(cover == "<1" ~ "0.8",
                               cover == "<0.5" ~ "0.3",
                               TRUE ~ cover),
         cover_num = as.numeric(cover_num))

# make some turf plots to look at data
#pdf("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/turf_plots.pdf", width = 8, height = 13)
#
#for(i in 1:length(unique_siteblock)) {
#  
#  site_block <- unique_siteblock[i]
#  site_ID <- str_sub(site_block, 1, 2)
#  block_ID <- str_sub(site_block, 4, 5) %>% as.numeric()
#  block_data <- dat_comb_mapper[dat_comb_mapper$block_ID_original == block_ID & dat_comb_mapper$site == site_ID, ]
#  
#  plot <- ggplot(block_data, aes(x = factor(year), y = species, fill = cover_num)) +
#    geom_tile() +
#    scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) +
#    labs(x = "", y = "", title = site_block) +
#    facet_wrap(added_focals ~ treat_warming, scales = "free_y") +
#    theme_bw()
#  
#  print(plot)
#  
#}
#
#dev.off()

# solve group problems and species changing over time etc. - double check with data sheets, general comments, comments in yearly data etc.

# 1) grass group: only few grasses were distinguished in 2021, then every year the surveys got more detailed

# species recorded in 2021: Luzula spp., Nardus stricta, Poa alpina, Poa pratensis, Briza media, Phleum pratense, Elymus repens, Dactylis glomerata, Fetsuca pratensis, Festuca spp.,
# Sesleria caerulea
sp2021 <- dat_comb %>%
  filter(year == 2021) %>%
  distinct(species)
  
# species recorded in 2022: Luzula spp., Nardus stricta, Poa alpina, Poa pratensis, Briza media, Phleum pratense, Dactylis glomerata, Fetsuca pratensis, Festuca spp., Elymus repens (deleted later),
# Sesleria caerulea, Festuca rubra, Festuca arundinaceae, Helictotrichon pubescens, Agrostis spp., Agrostis alpina, Agrostis capillaris, Agrostis stolonifera, hairy grass (deleted later)
sp2022 <- dat_comb %>%
  filter(year == 2022) %>%
  distinct(species)

# species recorded in 2023: Luzula spp., Nardus stricta, Poa alpina, Poa pratensis, Briza media, Phleum pratense, Dactylis glomerata, Fetsuca pratensis, Festuca spp., 
# Sesleria caerulea, Festuca rubra agg., Festuca arundinaceae, Helictotrichon pubescens, Agrostis spp., Agrostis alpina, Agrostis capillaris, Agrostis stolonifera, hairy grass,
# Phleum alpinum, Anthoxanthum spp., Helictrotrichon versicolor, Anthoxanthum alpinum
sp2023 <- dat_comb %>%
  filter(year == 2023) %>%
  distinct(species)

# species recorded in 2024: Luzula spp., Nardus stricta, Poa alpina, Poa pratensis, Briza media, Phleum pratense, Dactylis glomerata, Fetsuca pratensis, Festuca spp., 
# Sesleria caerulea, Festuca rubra agg., Festuca arundinaceae, Helictotrichon pubescens, Agrostis spp., Agrostis alpina, Agrostis capillaris, Agrostis stolonifera, hairy grass,
# Phleum alpinum, Anthoxanthum spp., Helictrotrichon versicolor, Anthoxanthum alpinum
sp2024 <- dat_comb %>%
  filter(year == 2024) %>%
  distinct(species)

# make plots of only grass species
grasses <- c("Luzula spp.", "Nardus stricta", "Poa alpina", "Poa pratensis", "Briza media", "Phleum pratense", "Dactylis glomerata", "Festuca pratensis", "Festuca spp.", 
             "Sesleria caerulea", "Festuca rubra agg.", "Festuca arundinaceae", "Helictotrichon pubescens", "Agrostis spp.", "Agrostis alpina", "Agrostis capillaris", 
             "Agrostis stolonifera", "hairy grass", "Phleum alpinum", "Anthoxanthum spp.", "Helictrotrichon versicolor", "Anthoxanthum alpinum")

dat_grasses <- dat_comb %>%
  filter(species %in% grasses | grepl("Festuca", species) == TRUE | grepl("Agrostis", species) == TRUE | grepl("Poa", species) == TRUE | grepl("Helicto", species) == TRUE | 
         grepl("grass", species) == TRUE) 
dat_grasses_mapper <- dat_grasses %>% # make cover numeric
  mutate(cover_num = case_when(cover == "<1" ~ "0.8",
                               cover == "<0.5" ~ "0.3",
                               TRUE ~ cover),
         cover_num = as.numeric(cover_num))

# make turf plots only for grasses
#pdf("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/grass_plots.pdf", width = 8, height = 6)
#
#for(i in 1:length(unique_siteblock)) {
#  
#  site_block <- unique_siteblock[i]
#  site_ID <- str_sub(site_block, 1, 2)
#  block_ID <- str_sub(site_block, 4, 5) %>% as.numeric()
#  block_data <- dat_grasses_mapper[dat_grasses_mapper$block_ID_original == block_ID & dat_grasses_mapper$site == site_ID, ]
#  
#  plot <- ggplot(block_data, aes(x = factor(year), y = species, fill = cover_num)) +
#    geom_tile() +
#    scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) +
#    labs(x = "", y = "", title = site_block) +
#    facet_wrap(added_focals ~ treat_warming, scales = "free_y") +
#    theme_bw()
#  
#  print(plot)
#  
#}
#
#dev.off()

# get data frame with grass group and the group species for each plot
dat_grassgroup_hi <- dat_comb %>%
  filter(species == "grass group" & site == "hi") 
dat_grassgroup_lo <- dat_comb %>%
  filter(species == "grass group" & site == "lo")

# get data frame with cover of grass group for each plot
dat_grassgroup_hi2022 <- dat_comb %>%
  filter(species == "grass group" & site == "hi" & year == 2022) %>%
  dplyr::select(treat_warming, treat_competition, added_focals, site, cover, species, Group_Species, Comment, block_ID_original, plot_ID_original)


# things to fix manually before adding on group and comment species via the correction file (for HI site):
# - delete Elymus repens in 2021 (never found again)
# - Anthoxanthum at hi site is all Anthixanthum alpinum (never overlapping)
# - Festuca spp. at hi site is always Festuca rubra aggr.
# - delete hairy grass in hi.02 warm nf (is added as Anthoxanthum manually in correction file as it would be if in comments)
# - delete Phleum pratense and Phleum alpinum in hi.03 warm nf (was manually added together and added to the correction file)
# - delete Nardus stricta in hi.09 ambi nf (mistake)
# - delete Agrostis stolonifera in hi.08 warm wf (Agrostis stolonifera was added to grass group in all other cases in field)
# - hairy grass in Cal_2.5 is Helictotrichon pubescens (see corrections on print outs for 2022)
# - Nardus stricta in Cal_7.2 2022 is Festuca (see corrections on print outs for 2022)

dat_comb <- dat_comb %>%
  filter(!(grepl("Elym", species) == TRUE) &
         !(species %in% c("Phleum pratense", "Phleum alpinum") & site == "hi" & block_ID == "03" & treat_warming == "warm" & added_focals == "nf") &
         !(species == "Nardus stricta" & site == "hi" & block_ID == "09" & treat_warming == "ambi" & added_focals == "nf") &
         !(species == "Agrostis stolonifera" & site == "hi" & block_ID == "08" & treat_warming == "warm" & added_focals == "wf"))  %>% # 6 rows removed, great
  mutate(species = case_when(grepl("Festuca", species) == TRUE & site == "hi" ~ "Festuca rubra aggr.",
                             grepl("Festuca rubra", species) == TRUE ~ "Festuca rubra aggr.",
                             grepl("Festuca rubra agg.", species) == TRUE ~ "Festuca rubra aggr.",
                             grepl("Anthoxanthum", species) == TRUE & site == "hi" ~ "Anthoxanthum alpinum",
                             grepl("hairy grass", species) == TRUE & site == "hi" ~ "Helictotrichon pubescens",
                             species == "Nardus stricta" & site == "hi" & block_ID_original == 7 & plot_ID_original == 2 ~ "Festuca rubra aggr.", 
                             TRUE ~ species)) %>%
  ungroup() %>%
  #distinct(species) %>%
  group_by(region, site, block_ID, block_ID_original, plot_ID_original, species, treat_warming, treat_competition, added_focals, Comment, Group_Species, collector, unique_plot_ID,
           year, date_measurement, only_focals) %>%
  summarize(cover = first(cover)) %>% # check whether cover changed: cover2 = max(cover), cover_changed = any(cover != max(cover))
  #mutate(changed_rows = ifelse(cover_changed, 1, 0)) %>%
  ungroup()


# things to fix manually before adding on group and comment species via the correction file (for LO site):
# - change Festuca pratensis in lo.02 ambi wf to Festuca arundinaceae (is the same)
# - change Festuca pratensis in lo.09 ambi wf to Festuca arundinaceae (is the same)
# - change Festuca rubra to Festuca rubra aggr.
# change Agrostis spp. to Agrostis sp.

dat_comb <- dat_comb %>%
  mutate(species = case_when(grepl("Festuca prat", species) == TRUE & site == "lo" & block_ID == "02" & treat_warming == "ambi" & added_focals == "wf" ~ "Festuca arundinacea",
                             grepl("Festuca prat", species) == TRUE & site == "lo" & block_ID == "09" & treat_warming == "ambi" & added_focals == "wf" ~ "Festuca arundinacea",
                             grepl("Festuca rubra", species) == TRUE ~ "Festuca rubra aggr.",
                             grepl("Festuca rubra agg.", species) == TRUE ~ "Festuca rubra aggr.",
                             grepl("Agrostis s", species) == TRUE ~ "Agrostis sp.",
                             TRUE ~ species))

# 2) Carex group: not all Carex were recorded separately in each year

dat_carex <- dat_comb %>%
  filter(grepl("Carex", species) == TRUE) 
dat_carex_mapper <- dat_carex %>% # make cover numeric
  mutate(cover_num = case_when(cover == "<1" ~ "0.8",
                               cover == "<0.5" ~ "0.3",
                               TRUE ~ cover),
         cover_num = as.numeric(cover_num))

# make turf plots only for Carex
#pdf("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/carex_plots.pdf", width = 8, height = 6)
#
#for(i in 1:length(unique_siteblock)) {
#  
#  site_block <- unique_siteblock[i]
#  site_ID <- str_sub(site_block, 1, 2)
#  block_ID <- str_sub(site_block, 4, 5) %>% as.numeric()
#  block_data <- dat_carex_mapper[dat_carex_mapper$block_ID_original == block_ID & dat_carex_mapper$site == site_ID, ]
#  
#  plot <- ggplot(block_data, aes(x = factor(year), y = species, fill = cover_num)) +
#    geom_tile() +
#    scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) +
#    labs(x = "", y = "", title = site_block) +
#    facet_wrap(added_focals ~ treat_warming, scales = "free_y") +
#    theme_bw()
#  
#  print(plot)
#  
#}
#
#dev.off()


# get data frame with grass group and the group species for each plot
dat_carexgroup_hi <- dat_comb %>%
  filter(species == "Carex group" & site == "hi") 
dat_carexgroup_lo <- dat_comb %>%
  filter(species == "Carex group" & site == "lo")

# things to fix manually before adding on group and comment species via the correction file (for BOTH site):
# - Carex ornithopoda: let as is, remark in data paper
# - Carex montana vs. Carex caryophyllea at lo site: use Carex caryophyllea (as this is what we had in 2023 & confirmed by discussion) for Carex montana
#   except in plot where both occur (block 6, confirmed in 2023 notes)

dat_comb <- dat_comb %>%
  mutate(species = case_when(species == "Carex montana" & site == "lo" & block_ID != "06" ~ "Carex caryophyllea",
                             TRUE ~ species))

# add the corrections and additions on
dat_combcorr <- bind_rows(dat_comb, corrections)

# now the corrections don't have any collector, date, plot_ID_original or block_ID yet (the plot and time point they belong to are defined via block_ID_original, treatments and year)  
dat_combcorr <- dat_combcorr %>%
  group_by(region, site, block_ID_original, treat_warming, treat_competition, added_focals, year) %>%
  mutate(block_ID = if_else(is.na(block_ID), first(block_ID), block_ID), # fill in missing values by copying from the first row in each group
         plot_ID_original = if_else(is.na(plot_ID_original), first(plot_ID_original), plot_ID_original),
         date_measurement = if_else(is.na(date_measurement), first(date_measurement), date_measurement),
         collector = if_else(is.na(collector), first(collector), collector),
         unique_plot_ID = if_else(is.na(unique_plot_ID), first(unique_plot_ID), unique_plot_ID)) %>%
  ungroup()  


# 3) species not keyed to the species level & other problems from 2021 - 2023 and from notes

# If species were not identified to the species level but consistently recorded over the years, give them a unique name based on the most accurate 
# level of identification (Arabis 1, Brassicaceae 1 etc.). For not plot specific species never keyed to species level but consistent over multiple plots,
# use sp. or cf. etc. If numbered species are reported only once and have a cover <0.5%, delete. Use the turf maps to check this.


# fix hi site:
# - Luzula spp. to Luzula sp. | done
# - Leontodon hispidus at hi site in 2021 to Leontodon helveticus (except in hi.06 nf warm, where both are confirmed to be present) | done
# - grasses should be already fixed now (Festuca, Anthoxanthum etc.) | done
# - Thymus spp. to Thymus sp. | done
# - fuse small Gentiana and small Gentiana group (to small Gentiana group) for further processing | done
# - small Gentiana group to Gentiana verna | done
# - Gentiana spp. in 2021 in hi.02 wf ambi | notes say tenella or nivalis, deleted (cover <0.5)
# - Gentiana spp. in 2022 in hi.04 nf ambi is at same time as Gentiana campestris and Gentiana verna | no specifications, deleted (cover <0.5)
# - Gentiana tenella in hi.03 nf warm is Gentiana verna (see corrections on print outs for 2021) | done
# - AEG in hi.01 wf warm (constant) - rename | renamed to Asteraceae 1
# - AEG in hi.04 wf ambi (2022/ 23) - rename | renamed to Asteraceae 2
# - AEG in 2021 in hi.05 wf ambi, prob. just delete (check cover) | deleted
# - AEG in in 2021 in hi.06 wf warm, prob. just delete (check cover) | deleted
# - AEG in 2021 in hi.06 nf warm is Aster bellidiastrum (see corrections on print outs for 2021)
# - AEG in hi.09 nf warm (constant) is Primula integrifolia (see general comments from 2024) | renamed to Primula integrifolia
# - unknown 1/AEG or just unknown 1 in hi.05 nf warm (constant) | renamed to Primula sp. (based on comments)
# - Trifolium campestre vs. Trifolium repens | confusing notes, left as is
# - Clinopodium vulgare in 2021 in hi.04 nf warm is Prunella grandiflora in other years(see corrections on print outs for 2021) | done
# - Leucanthemum adustum in 2022 hi.04 wf warm just Aster bellidiastrum? | left as is, no notes/ hints
# - Trollius europaeus in 2022 in hi.05 nf warm is Anemone narcissiflora in 2023/2024 | done
# - Ranunculaceae 1 in 2021 in hi.05 wf warm, prob. just delete (check cover) | cover <0.5, deleted
# - unknown 2 in 2021 in hi.06 nf warm is Leontodon helveticus (see corrections on print outs for 2021) | done
# - Thistle Cal 1 in hi.06 nf warm (constant) | renamed to Carduoideae 1
# - Thistle Cal 1 in 2021/22 in hi.06 wf warm | renamed to Carduoideae 1
# - Thistle Cal 1 in hi.07 wf warm in 2021 | renamed to Carduoideae 1
# - Leontodon helveticus (2022) & Crepis aurea (2023) in hi.06 wf ambi the same? | trust later data, changed to Crepis aurea
# - Leontodon spp. (2021) & Crepis aurea (2022) in hi.08 wf ambi the same? | trust later data, changed to Crepis aurea
# - Lontodon spp. in 2021 in hi.06 wf warm | changed to Leontodon helveticus 
# - Hieracium pilosella in 2021 in hi.10 nf ambi is Hieracium villosum (2022 - 2024) (see corrections on print outs for 2021) | done
# - Pimpinella spp. (2021) in hi.10 wf ambi is Pimpinella major in 2022 - 2023 | done
# - Pimpinella spp. (2021) in hi.08 wf ambi is Pimpinella major in 2022 - 2023 | done
# - Vaccinium spp. (2021) in hi.07 wf ambi is Polygala chamaebuxus in 2022/23 (see corrections on print outs for 2021) | done
# - Ranunculus acris in 2021 in hi.09 nf warm Trollius europaeus (2022 - 2024) (see corrections on print outs for 2021) | done
# - Ranunculus acris in 2021 in hi.09 wf ambi is Trollius europaeus (2022 - 2024) (see corrections on print outs for 2021) | done
# - Ranunculus acris in 2021 in hi.09 wf warm is Anemone narcissiflora (2022 - 2024) (see corrections on print outs for 2022) | done
# - Ranunculus acris in 2021/22 in hi.08 wf ambi prob. Anemone narcissiflora (2022 - 2023) (see corrections on print outs for 2021) | done
# - Ranunculus acris (2021) in hi.10 nf warm is Anemone narcissiflora (2022 - 2024) (see corrections on print outs for 2021) | done
# - Arabis spp. in 2021 in hi.08 wf ambi and Arabis bellidifolia (2021/22) are Veronica aphylla (see corrections on print outs for 2022) | done
# - Prunella spp. (2021) in hi.09 nf warm | renamed to P. grandiflora as in hi.04 nf warm 
# - Trifolium montanum (2021) in hi.05 wf warm is Trifolium repens (see corrections on print outs for 2021) | done
# - Veronica chamaedrys (2021) in hi.09 wf warm is Bartsia alpina (see corrections on print outs for 2021) | done
# - Antennaria in 2022 in hi.08 nf ambi should have cover 11 and Androsace cover 1 (switched - see corrections on print outs for 2022) | done
# - Plantago atrata in 2021 in hi.08 wf ambi should have cover 12 and Plantago media cover <1 (switched - see corrections on print outs for 2021) | done
# - Hippocrepis and Lotus have cover switched (general comments in 2024 downloaded table) | done


dat_combcorrhi <- dat_combcorr %>%
  mutate(species = case_when(grepl("Luzula", species) == TRUE ~ "Luzula sp.",
                             grepl("Leontodon hispidus", species) == TRUE & site == "hi" & unique_plot_ID != "CHE.hi.warm.vege.nf.06" ~ "Leontodon helveticus",
                             grepl("Thymus", species) == TRUE ~ "Thymus sp.",
                             grepl("small Gentiana", species) == TRUE ~ "Gentiana verna",
                             species == "small Gentiana group" ~ "Gentiana verna",
                             species == "Gentiana tenella" & site == "hi" & block_ID == "03" ~ "Gentiana verna",
                             grepl("AEG", species) == TRUE & site == "hi" & block_ID == "01" ~ "Asteraceae 1",
                             grepl("AEG", species) == TRUE & site == "hi" & block_ID == "04" ~ "Asteraceae 2",
                             grepl("AEG", species) == TRUE & site == "hi" & block_ID == "09" ~ "Primula integrifolia",
                             grepl("AEG", species) == TRUE & site == "hi" & block_ID == "06" ~ "Aster bellidiastrum",
                             species == "unknown 1/AEG" & site == "hi" & block_ID == "05" ~ "Primula sp.",
                             species == "unknown 1" & site == "hi" & block_ID == "05" ~ "Primula sp.",
                             grepl("Clinopodium", species) == TRUE & site == "hi" ~ "Prunella grandiflora",
                             grepl("Trollius", species) == TRUE & site == "hi" & block_ID == "05" & treat_warming == "warm" & added_focals == "nf" ~ "Anemone narcissiflora",
                             grepl("unknwon 2", species) == TRUE & site == "hi" & block_ID == "06" ~ "Leontodon helveticus",
                             grepl("Thistle Cal 1", species) == TRUE ~ "Carduoideae 1",
                             grepl("Leontodon helveticus", species) == TRUE & site == "hi" & block_ID == "06" & treat_warming == "ambi" & added_focals == "wf" ~ "Crepis aurea",
                             grepl("Leontodon spp", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "wf" ~ "Crepis aurea",
                             grepl("Leontodon spp", species) == TRUE & site == "hi" & block_ID == "06" & treat_warming == "warm" & added_focals == "wf" ~ "Leontodon helveticus",
                             grepl("Hieracium pilo", species) == TRUE & site == "hi" & block_ID == "10" & treat_warming == "ambi" & added_focals == "nf" ~ "Hieracium villosum",
                             grepl("Pimpinella spp.", species) == TRUE & site == "hi" ~ "Pimpinella major",
                             grepl("Vaccinium spp.", species) == TRUE & site == "hi" ~ "Polygala chamaebuxus",
                             grepl("Ranunculus acris", species) == TRUE & site == "hi" & block_ID == "09" & treat_warming == "warm" & added_focals == "nf" ~ "Trollius europaeus",
                             grepl("Ranunculus acris", species) == TRUE & site == "hi" & block_ID == "09" & treat_warming == "ambi" & added_focals == "wf" ~ "Trollius europaeus",
                             grepl("Ranunculus acris", species) == TRUE & site == "hi" & block_ID == "09" & treat_warming == "warm" & added_focals == "wf" ~ "Anemone narcissiflora",
                             grepl("Ranunculus acris", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "wf" ~ "Anemone narcissiflora",
                             grepl("Ranunculus acris", species) == TRUE & site == "hi" & block_ID == "10" & treat_warming == "warm" & added_focals == "nf" ~ "Anemone narcissiflora",
                             grepl("Arabis", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "wf" ~ "Veronica aphylla",
                             grepl("Prunella spp.", species) == TRUE & site == "hi" ~ "Prunella grandiflora",
                             grepl("Trifolium mon", species) == TRUE & site == "hi" & block_ID == "05" & treat_warming == "warm" & added_focals == "wf" ~ "Trifolium repens",
                             grepl("Veronica cham", species) == TRUE & site == "hi" & block_ID == "09" & treat_warming == "warm" & added_focals == "wf" ~ "Bartsia alpina",
                             TRUE ~ species),
         cover = case_when(grepl("Antennaria", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "nf" & year == 2022 ~ "11",
                           grepl("Androsace", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "nf" & year == 2022 ~ "1",
                           grepl("Plantago atr", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "wf" & year == 2022 ~ "12",
                           grepl("Plantago med", species) == TRUE & site == "hi" & block_ID == "08" & treat_warming == "ambi" & added_focals == "wf" & year == 2022 ~ "<1",
                           grepl("Hippocrepis", species) == TRUE & site == "hi" & block_ID == "06" & treat_warming == "ambi" & added_focals == "nf" & year == 2023 ~ "11",
                           grepl("Lotus", species) == TRUE & site == "hi" & block_ID == "06" & treat_warming == "ambi" & added_focals == "nf" & year == 2023 ~ "<1",
                           TRUE ~ cover)) %>%
  filter(!(species == "Gentiana spp." & site == "hi"),
         !(grepl("AEG", species) == TRUE & site == "hi" & block_ID == "05"),
         !(grepl("Ranunculaceae 1", species) == TRUE & site == "hi"))


# fix lo site:
# - Prunella spp. (2022 - 2023) in lo.10 wf ambi - rename (there's Prunella vulgaris in lo.09 wf ambi, for example) | renamed to P. vulgaris
# - Prunella spp. (2021 - 2023) in lo.06 wf ambi - rename | renamed to P. vulgaris
# - Prunella spp. (2023) in lo.03 wf ambi - rename | renamed to P. vulgaris
# - Lamiaceae 1 (2021) in lo.10 wf ambi could be Prunella spp. of 2022/23  | renamed to P. vulgaris
# - Lamiaceae 2 in 2021 in lo.04 wf ambi is prob. Ajuga reptans in 2022 - 2023 | renamed to Ajuga reptans
# - Arabis cf. (2023) in lo.10 wf ambi | renamed to Arabis sp.
# - Arabis cf. (2023) in lo.08 wf ambi | renamed to Arabis sp.
# - Arabis cf. (2022/23) in lo.06 wf ambi | renamed to Arabis sp.
# - Arabis spp. (2021) is Arabis cf. (20222/23) in lo.04 wf ambi | renamed to Arabis sp.
# - Thistle Nes 1 (2021) in lo.07 wf ambi | renamed to Carduoideae 2
# - Thistle Nes 1 (2021) in lo.05 wf ambi | renamed to Carduoideae 2
# - Hypericum perforatum (2021 - 2023) vs. Hypericum maculatum (2023) in lo.06 wf ambi | leave as Hypericum perforatum, add cover of maculatum & perforatum together (note in data paper)
# - Hypericum perforatum (2021 - 2023) vs. Hypericum maculatum (2023) in lo.04 wf ambi | leave as Hypericum perforatum, add cover of maculatum & perforatum together (note in data paper)
# - Hypericum perforatum (2021 - 2023) vs. Hypericum maculatum (2023) in lo.02 wf ambi | leave as Hypericum perforatum, add cover of maculatum & perforatum together (note in data paper)
# - Hypericum perforatum (2021 - 2023) vs. Hypericum maculatum (2023) in lo.01 wf ambi | leave as Hypericum perforatum, add cover of maculatum & perforatum together (note in data paper)
# - Potentilla spp. (2021) in lo.03 wf ambi is Potentilla verna (2022/23) | done
# - Brassicaceae 1 (2021) in lo.03 wf ambi is Arabis sp. (2022/23) | done
# - Brassica sp. 1 (2022/23) in lo.01 wf ambi  | renamed to Brassicaceae 1
# - Centaurea jaceae 2022 in lo.06 wf ambi can be split in <1 jacea and 1 jacea angustifolia (see notes on print outs) | already done in 2022 preparation
 

dat_combcorrhilo <- dat_combcorrhi %>%
  mutate(species = case_when(species == "Prunella spp." & site == "lo" ~ "Prunella vulgaris",
                             species == "Lamiaceae 1" & site == "lo" ~ "Prunella vulgaris",
                             species == "Lamiaceae 2" & site == "lo" ~ "Ajuga reptans",
                             grepl("Arabis", species) == TRUE & site == "lo" ~ "Arabis sp.",
                             grepl("Thistle Nes 1", species) == TRUE & site == "lo" ~ "Carduoideae 2",
                             grepl("Potentilla spp.", species) == TRUE & site == "lo" & block_ID == "03" ~ "Potentilla verna",
                             grepl("Brassicaceae 1", species) == TRUE & site == "lo" & block_ID == "03" ~ "Arabis sp.",
                             grepl("Brassica sp. 1", species) == TRUE & site == "lo"  & block_ID == "01" ~ "Brassicaceae 1",
                             TRUE ~ species),
         cover = case_when(species == "Hypericum perforatum" & site == "lo" & block_ID == "01" & year == 2023 ~ "<1",
                           species == "Hypericum perforatum" & site == "lo" & block_ID == "02" & year == 2023 ~ "4",
                           species == "Hypericum perforatum" & site == "lo" & block_ID == "04" & year == 2023 ~ "<1",
                           species == "Hypericum perforatum" & site == "lo" & block_ID == "06" & year == 2023 ~ "12",
                           TRUE ~ cover),
         only_focals = case_when(species == "Hypericum perforatum" & site == "lo" & block_ID == "01" & year == 2023 ~ "no",
                                 species == "Hypericum perforatum" & site == "lo" & block_ID == "02" & year == 2023 ~ "no",
                                 species == "Hypericum perforatum" & site == "lo" & block_ID == "04" & year == 2023 ~ "no",
                                 species == "Hypericum perforatum" & site == "lo" & block_ID == "06" & year == 2023 ~ "no",
                                 TRUE ~ only_focals)) %>%
  filter(!(species == "Hypericum maculatum"))




# in the end: group again by plot and year and look for dublicates (be have some new ones as for example in 1 plot there's both Leontodon hispidus and Loentodon helveticus)

# 4) notes from 2024


################################################################################
### FINAL CHECK ################################################################
################################################################################

# check for NAs in important columns 
dat_combcorrhilo_na <- dat_combcorrhilo %>% 
  filter(is.na(date_measurement) | is.na(region) | is.na(site) | is.na(block_ID) | is.na(block_ID_original) | is.na(plot_ID_original) | is.na(cover) | is.na(collector) |
         is.na(treat_warming) | is.na(treat_competition) | is.na(added_focals) | is.na(unique_plot_ID)) # none

# check for empty cells which shouldn't be empty
dat_combcorrhilo_empty <- dat_combcorrhilo %>% 
  filter(is.empty(date_measurement) | is.empty(region) | is.empty(site) | is.empty(block_ID) | is.empty(block_ID_original) | is.empty(plot_ID_original) | is.empty(cover) | is.empty(collector) |
           is.empty(treat_warming) | is.empty(treat_competition) | is.empty(added_focals) | is.empty(unique_plot_ID)) # none

# check for duplicates (complete and only in ID columns)
dat_combcorrhilo_dubli <- dat_combcorrhilo %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) # none

dat_combcorrhilo_dubli <- dat_combcorrhilo %>% 
  filter(duplicated(select(., site, species, block_ID_original, plot_ID_original, unique_plot_ID, year)) | 
           duplicated(select(., site, species, block_ID_original, plot_ID_original, unique_plot_ID, year), fromLast = TRUE)) # 10 left

# fix:
# - one row for Festuca rubra in CHE.hi.warm.vege.nf.03 can just be dropped
# - Leontodon case is fixed higher up
# - Veronica aphylla in CHE.hi.ambi.vege.wf.08 is created as a duplicated in corrections, one row can just be dropped
# - dito for Anemone narcissiflora in CHE.hi.ambi.vege.wf.08

dat_combcorrhilo <- dat_combcorrhilo %>%
  filter(!(unique_plot_ID == "CHE.hi.warm.vege.nf.03" & species == "Festuca rubra aggr." & is.na(Comment) == FALSE & year == 2022)) %>% 
  filter(!(unique_plot_ID == "CHE.hi.ambi.vege.wf.08" & species == "Veronica aphylla" & is.na(Comment) == FALSE  & year == 2021)) %>%
  filter(!(unique_plot_ID == "CHE.hi.ambi.vege.wf.08" & species == "Anemone narcissiflora" & cover == "<0.5" & year == 2022))

dat_combcorrhilo_dubli <- dat_combcorrhilo %>% 
  filter(duplicated(select(., site, species, block_ID_original, plot_ID_original, unique_plot_ID, year)) | 
           duplicated(select(., site, species, block_ID_original, plot_ID_original, unique_plot_ID, year), fromLast = TRUE)) # none

# check for leading or trailing spaces
dat_combcorrhilo_spaces <- dat_combcorrhilo %>% 
  filter(apply(across(everything(), lead.trail.spaces), 1, any)) # none

# double-check the species list
species <- dat_combcorrhilo %>%
  distinct(species)


# plot the turfs again
dat_combcorrhilo_mapper <- dat_combcorrhilo %>% # make cover numeric
  mutate(cover_num = case_when(cover == "<1" ~ "0.8",
                               cover == "<0.5" ~ "0.3",
                               cover == "9999" ~ "NA",
                               TRUE ~ cover),
         cover_num = as.numeric(cover_num))

#pdf("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/turf_plots_aftercleaning.pdf", width = 8, height = 13)
#
#for(i in 1:length(unique_siteblock)) {
#  
#  site_block <- unique_siteblock[i]
#  site_ID <- str_sub(site_block, 1, 2)
#  block_ID <- str_sub(site_block, 4, 5) %>% as.numeric()
#  block_data <- dat_combcorrhilo_mapper[dat_combcorrhilo_mapper$block_ID_original == block_ID & dat_combcorrhilo_mapper$site == site_ID, ]
#  
#  plot <- ggplot(block_data, aes(x = factor(year), y = species, fill = cover_num)) +
#    geom_tile() +
#    scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) +
#    labs(x = "", y = "", title = site_block) +
#    facet_wrap(added_focals ~ treat_warming, scales = "free_y") +
#    theme_bw()
#  
#  print(plot)
#  
#}
#
#dev.off()

# check whether all focal species have a yes/ no for only_focals and all other species an NA
focals <- c("Brachipodium pinnatum", "Bromus erectus", "Daucus carota", "Hypericum perforatum", "Silene vulgaris", "Scabiosa columbaria", "Centaurea jacea", "Plantago media", "Medicago lupulina", "Salvia pratensis")
focal_check <- dat_combcorrhilo %>%
  mutate(focal_check = case_when(species %in% focals & only_focals %in% c("yes", "no") ~ "good",
                                 species %nin% focals & is.na(only_focals) == TRUE ~ "good", 
                                 TRUE ~ "check"))

# there's NAs for focals mainly at hi site - they can just be changed to no (there are no naturally growing focal species within the hi site)
dat_combcorrhilo <- dat_combcorrhilo %>%
  mutate(only_focals = if_else(species %in% focals & site == "hi" & is.na(only_focals) == TRUE, "yes", only_focals))

# check again
focal_check <- dat_combcorrhilo %>%
  mutate(focal_check = case_when(species %in% focals & only_focals %in% c("yes", "no") ~ "good",
                                 species %nin% focals & is.na(only_focals) == TRUE ~ "good", 
                                 TRUE ~ "check"))

# one plot (CHE.lo.ambi.vege.wf.03) with no indications in 2023 and some missing in other years --> use what was recorded in 2021/22
dat_combcorrhilo %>% 
  filter(unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & year != 2023 & species %in% focals)

dat_combcorrhilo <- dat_combcorrhilo %>%
  mutate(only_focals = case_when(unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Brachipodium pinnatum" ~ "no",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Bromus erectus" ~ "no",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Centaurea jacea" ~ "yes",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Daucus carota" ~ "yes",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Hypericum perforatum" ~ "yes",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Medicago lupulina" ~ "no",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Plantago media" ~ "no",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Salvia pratensis" ~ "no",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Scabiosa columbaria" ~ "yes",
                                 unique_plot_ID == "CHE.lo.ambi.vege.wf.03" & species == "Silene vulgaris" ~ "no",
                                 TRUE ~ only_focals))

# check again
focal_check <- dat_combcorrhilo %>%
  mutate(focal_check = case_when(species %in% focals & only_focals %in% c("yes", "no") ~ "good",
                                 species %nin% focals & is.na(only_focals) == TRUE ~ "good", 
                                 TRUE ~ "check")) # all good

# also check whether the yes/ no changes over the years
# check again
focal_check <- dat_combcorrhilo %>%
  filter(species %in% focals) %>%
  group_by(unique_plot_ID, species) %>%
  summarize(unique_focals = paste(unique(only_focals), collapse = ", "))  %>% # a few cases where there's both yes and no at lo site - check them out
  filter(unique_focals == "yes, no") %>%
  ungroup()

focal_mixed <- dat_combcorrhilo %>%
  semi_join(focal_check, by = c("unique_plot_ID", "species")) # in most cases the 2021 data point is the odd one out --> use the majority (exception: Centaurea jacea in CHE.lo.ambi.vege.wf.07 in 2021: contains jacea angustifolia...)

# try disentagle Centaurea jacea from jacea angustifolia in CHE.lo.ambi.vege.wf.07 in 2021
dat_comb %>%
  filter(unique_plot_ID == "CHE.lo.ambi.vege.wf.07" & grepl("Centaurea", species)) # in the other two years, the planted Centaurea was <0.5 - use the same for 2021

cenangi_7.2 <- dat_combcorrhilo %>%
  filter(unique_plot_ID == "CHE.lo.ambi.vege.wf.07" & species == "Centaurea jacea" & year == 2021) %>%
  mutate(cover = "<0.5")

dat_combcorrhilo <- dat_combcorrhilo %>% # add on cenangi new row
  mutate(species = if_else(unique_plot_ID == "CHE.lo.ambi.vege.wf.07" & species == "Centaurea jacea" & year == 2021, "Centaurea jacea angustifolia", species),
         only_focals = if_else(unique_plot_ID == "CHE.lo.ambi.vege.wf.07" & species == "Centaurea jacea angustifolia" & year == 2021, NA, only_focals)) %>%
  bind_rows(cenangi_7.2)

dat_combcorrhilo %>%
  filter(unique_plot_ID == "CHE.lo.ambi.vege.wf.07" & grepl("Centaurea", species)) # in the other two years, the planted Centaurea was <0.5 - use the same for 2021

# now use majority of yes/ no for the cases where both were recorded
dat_combcorrhilo <- dat_combcorrhilo %>% 
  group_by(unique_plot_ID, species) %>%
  mutate(only_focals = if_else(sum(only_focals == "yes") >= 2, "yes", "no")) # if two yes, all yes - if two no, all no

dat_combcorrhilo %>%
  filter(species %in% focals) %>%
  group_by(unique_plot_ID, species) %>%
  summarize(unique_focals = paste(unique(only_focals), collapse = ", "))  %>% # all good now
  filter(unique_focals == "yes, no") %>%
  ungroup()



################################################################################
### SAVE FINAL FILE ############################################################
################################################################################

# last minute adjustements
dat_combcorrhilo <- dat_combcorrhilo %>%
  mutate(collector = if_else(collector %in% c("CB & EI", "CB &  EI", "EI & CB"), "CB&EI", collector))

# now select all the wanted columns and set them into the right data type
dat_final <- dat_combcorrhilo %>%
  dplyr::select(all_of(final_columns)) %>%
  ungroup() %>%
  mutate(date_measurement = as.Date(date_measurement),
         across(all_of(c("unique_plot_ID", "species", "cover", "collector", "only_focals")), as.character))

str(dat_final)

# exclude data from 2024 for data paper
dat_final_2123 <- dat_final %>%
  filter(year(date_measurement) != 2024)

# save complete 2021 - 2024 data
write_csv(dat_final_2123, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/RangeX_clean_VegSurveyGeneral_2021_2023_CHE.csv")
write_csv(dat_final, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Vegetation/RangeX_clean_VegSurveyGeneral_2021_2024_CHE.csv")















