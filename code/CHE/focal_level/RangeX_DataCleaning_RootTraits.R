################################################################################
### DATA CLEANING SCRIPT: ROOT TRAITS ##########################################
################################################################################

################################################################################

### Data used           : RangeX_raw_RootBiomass_2023.csv, RangeX_raw_RootScandata_2023.TXT
### Date last modified  : 05.12.2024
### Purpose             : Clean both the roots biomass data and data acquired by WinRhizo and combine them.

################################################################################

rm(list = ls())


### packages etc. ##############################################################

# basic packages
library(tidyverse); library(tidylog) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library(lubridate) # working with dates

# useful functions

# function to negate %in%
`%nin%` <- Negate(`%in%`)



### LOAD DATA SET ##############################################################

# load demographic data
dat_biomass_original <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Roots/root data/RangeX_raw_RootBiomass_2023.csv")
dat_scan_original <- read_delim("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Roots/root data/RangeX_raw_RootScandata_2023.TXT")


# load treatment key
meta_plant <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/metadata/RangeX_clean_MetadataFocal_CHE.csv")


# define useful vectors
species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")
species <- c("Brapin", "Broere", "Daucar", "Hypper", "Medlup", "Plamed", "Silvul", "Scacol", "Cenjac", "Salpra")


### PREPARATION: COLUMNS, DATA CLASSES #########################################

### SCAN DATA 

dat_scan <- dat_scan_original

# only keep wanted columns
dat_scan <- dat_scan %>%
  dplyr::select(`RHIZO 2009`,              # sample name 
                Operator,                  # operator
                `Analysis Date Time`,      # analysis date-time
                `AnalysedRegionArea(cm2)`, # analysed region area
                `Length(cm)`,              # length
                `ProjArea(cm2)`,           # projected area (area as looked at from above)
                `SurfArea(cm2)`,           # surface area (area of the complete root surface)
                `AvgDiam(mm)`,             # average diameter
                `RootVolume(cm3)`,         # root volume
                Tips,                      # number of tips
                Forks,                     # number of forks
                Crossings,                 # number of crossings
                contains("<.L.<"))         # all columns for the different length ranges (e.g., 0<.L.<=0.5000000)

# separate inconsistent ID column, fix mistakes in naming etc.
dat_scan <- dat_scan %>%
  mutate(original_id = if_else(grepl("NoName", `RHIZO 2009`) == TRUE | grepl("Cenjac30", `RHIZO 2009`) == TRUE | grepl("_", `RHIZO 2009`) == FALSE, `RHIZO 2009`, str_replace(`RHIZO 2009`, "_.*", "")),
         site = str_extract(`RHIZO 2009`, "^[A-Za-z]+"), 
         site = if_else(grepl("es", site) == TRUE, "lo", "hi"),
         plot = str_extract(`RHIZO 2009`, "\\d+\\.\\d+"),
         species = str_extract(`RHIZO 2009`, paste(species, collapse = "|")),
         roottype = str_sub(original_id, -5, -1),
         roottype = if_else(grepl("ine", roottype) == TRUE, "fine", "total"),
         species = if_else(grepl("hypper", original_id) == TRUE, "Hypper", species),
         species = if_else(grepl("cenjac", original_id) == TRUE, "Cenjac", species),
         position_ID_original = str_extract(original_id, paste0(species, "(\\d{1,2})")),
         position_ID_original = as.numeric(str_extract(position_ID_original, "[[:digit:]]{1,2}")),
         position_ID_original = if_else(grepl("hypper", original_id) == TRUE, 18, position_ID_original),
         position_ID_original = if_else(grepl("cenjac", original_id) == TRUE, 14, position_ID_original),
         subsets = str_extract(`RHIZO 2009`, "(?<=_).*"),
         subsets = if_else(grepl("22[A-Za-z]+B", original_id) == TRUE, "subsetb", subsets),
         subsets = if_else(grepl("subset", subsets) == TRUE, subsets, "none")) %>%
  separate(plot, c("block_ID_original", "plot_ID_original"), sep ="\\.") %>%
  mutate(block_ID_original = as.numeric(block_ID_original),
         plot_ID_original = as.numeric(plot_ID_original),
         region = "CHE",
         species = tolower(species)) %>%
  filter(is.na(Crossings) == FALSE & is.na(`Length(cm)`) == FALSE &
           grepl("NoName", `RHIZO 2009`) == FALSE) %>%
  rename("id_comments" = "RHIZO 2009") %>%
  dplyr::select(-c(Operator, `Analysis Date Time`, Tips, Forks, Crossings))#,
                #-contains("<.L.<"))

# delete measurements where the measured area isn't 1074.2413 (error scans), fix a position for a scacol (checked on scanned labels)
dat_scan <- dat_scan %>%
  filter(`AnalysedRegionArea(cm2)` > 1000) %>%
  dplyr::select(-`AnalysedRegionArea(cm2)`) %>%
  mutate(position_ID_original = if_else(site == "hi" & block_ID_original == 1 & plot_ID_original == 3 & position_ID_original == 16 & species == "scacol", 6, position_ID_original))

# now make sure there's only one scan per individual and roottype (if the proportion of coarse roots was to high after the first scan, it was repeated, dito if the first scan of the total roots was wonky)
dat_scan_check <- dat_scan %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets) %>%
  summarize(count_scans = n_distinct(`Length(cm)`), .groups = "drop")

# there's 6 individuals (7 rows because 1 individual has two subsets) which have more than 1 scan for the total roots - check them out
filtered_check <- dat_scan_check %>%
  filter(roottype == "total" & count_scans > 1)
double_scan_total <- dat_scan %>%
  semi_join(filtered_check, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "roottype", "subsets"))
double_scan_check <- as.vector(double_scan_total$id_comments)

# check them out in dat_scan_original to see when the scans were taken
filtered_check_original <- dat_scan_original %>%
  filter(`RHIZO 2009` %in% double_scan_check) %>% 
  dplyr::select(`RHIZO 2009`, `Analysis Date Time`, `Length(cm)`) # always take the second scan, this is the one which was worked with for fine roots later as well
double_scan_length <- filtered_check_original %>%
  group_by(`RHIZO 2009`) %>%
  mutate(date_time = dmy_hm(`Analysis Date Time`)) %>%
  filter(date_time == max(date_time)) %>%
  ungroup() %>%
  dplyr::select(`Length(cm)`, `RHIZO 2009`) %>%
  filter(!(`RHIZO 2009` %in% c("Cal2.3Salpra19total", "Nes9.1Salpra26total"))) # take the two salpras out, the duplicate is from wrongly labelled roottype and will be fixed when check for completeness of scans below

#double_scan_length <- as.vector(double_scan_length)

# check out whether for each individual there's a fine and a total scan
fine <- dat_scan %>% filter(roottype == "fine") %>% distinct(id_comments, .keep_all = TRUE)
total <- dat_scan %>% filter(roottype == "total") %>% distinct(id_comments, .keep_all = TRUE)

# 2 fine scans?
check <- anti_join(fine, total, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

dat_scan[grepl("al4.1Hypper18", dat_scan$id_comments) == TRUE, ] # two fine scans, probably no total as only fine roots
#[grepl("es4.2Plamed17", dat_scan$id_comments) == TRUE, ] # typo in site name in id_comments - should be alright when merging (fixed)

# 2 total scans?
check <- anti_join(total, fine, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

dat_scan[grepl("al2.3Salpra19", dat_scan$id_comments) == TRUE, ]$id_comments # two total scans, the one with zero in `5.0000000<.L.<=10.0000000` is the fine scan
dat_scan[grepl("al8.5Brapin16", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es5.2Scacol23", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es6.2Broere7", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine (this is true for all Bromus here included in this subsample --> Kevin's notes)
dat_scan[grepl("es9.1Brapin1", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es4.2Brapin1", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("s2.2Brapin16", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es5.2Brapin8", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es10.1Brapin1", dat_scan$id_comments) == TRUE, ] # only total --> no substantial coarse roots; total = fine
dat_scan[grepl("es9.1Salpra26", dat_scan$id_comments) == TRUE, ]$id_comments # two total scans, the one with zero in `5.0000000<.L.<=10.0000000` is the fine scan

# fix this: All individuals need two rows (one for fine and one for total roots) - if there is only one scan because all roots are fine, create a total column filled with NAs.
# - al4.1Hypper18: keep only scan with longer roots and keep roottype == "fine", plus add duplicate ro for roottype == "total" and NAs for all measurement columns
# - al2.3Salpra19: change roottype for row with `5.0000000<.L.<=10.0000000` == 0 to "fine"
# - es9.1Salpra26: change roottype for row with `5.0000000<.L.<=10.0000000` == 0 to "fine"
# - rest: duplicate row, label as "fine" and set all measurements for "total" to NA

measurement_cols <- c("Length(cm)", "ProjArea(cm2)", "SurfArea(cm2)", "AvgDiam(mm)", "RootVolume(cm3)", "0<.L.<=0.5000000", "0.5000000<.L.<=2.0000000", 
                      "2.0000000<.L.<=5.0000000", "5.0000000<.L.<=10.0000000", "10.0000000<.L.<=20.0000000")

# fix the salpras and hypper
dat_scan <- dat_scan %>%
  mutate(across(all_of(measurement_cols), ~ ifelse(
    (grepl("al4.1Hypper18", id_comments) == TRUE & `Length(cm)` == 6100.6618), # change all values to NA for shorter scan to directly use the row as total root scan
    NA, .)))  %>%
  mutate(roottype = if_else(grepl("al4.1Hypper18", id_comments) == TRUE & is.na(`Length(cm)`), "total", roottype),
         roottype = if_else(grepl("al2.3Salpra19", id_comments) == TRUE & `5.0000000<.L.<=10.0000000` == 0, "fine", roottype),
         roottype = if_else(grepl("es9.1Salpra26", id_comments) == TRUE & `5.0000000<.L.<=10.0000000` == 0, "fine", roottype))

# fix the rows to duplicate
rows_to_duplicate <- as.vector(check$id_comments)
no_duplicates <- c("Cal2.3Salpra19total", "Nes9.1Salpra26total")
rows_to_duplicate <- setdiff(rows_to_duplicate, no_duplicates)

dat_scan_newrows <- dat_scan %>%
  filter(id_comments %in% rows_to_duplicate) %>%
  mutate(across(all_of(measurement_cols), ~ NA),
         roottype = "total")

dat_scan <- dat_scan %>%
  mutate(roottype = if_else(id_comments %in% rows_to_duplicate, "fine", roottype)) %>%
  bind_rows(dat_scan_newrows)

# check out again whether for each individual there's a fine and a total scan
fine <- dat_scan %>% filter(roottype == "fine") %>% distinct(id_comments, .keep_all = TRUE) # 123 rows
total <- dat_scan %>% filter(roottype == "total") %>% distinct(id_comments, .keep_all = TRUE) # 122 rows

check <- anti_join(total, fine, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets")) # nice

# one duplicated row in fine
fine[duplicated(fine[, c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets")]), ]$id_comments # Nes4.2Plamed17fine
dupli <- dat_scan[grepl("es4.2Plamed17", dat_scan$id_comments) == TRUE,] # it's the Plamed where Nes is once capitalized and once not --> keep the one where `Length(cm)` != 0



# now reduce scans to two per individual: 1 for total roots and 1 for fine roots
# 1) total roots: use later scan ("Length(cm)" is saved in double_scan_length for filtering)
# 2) fine roots: use scan with lowest lengths of roots >2mm (i.e. lowest value when adding up columns "2.0000000<.L.<=5.0000000" and "5.0000000<.L.<=10.0000000")

# 1) prepare fine root scans
dat_scanfine <- dat_scan %>%
  filter(!(id_comments == "nes4.2Plamed17fine" & `Length(cm)` == 0)) %>% # get rid of the 0-length-plamed
  mutate(count_scans = n_distinct(`Length(cm)`), .groups = "drop",
         coarse_length = `2.0000000<.L.<=5.0000000` + `5.0000000<.L.<=10.0000000`,
         coarse_length = if_else(is.na(coarse_length) == TRUE, 0, coarse_length)) %>% # just for filtering
  filter(roottype == "fine") %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets) %>%
  filter(coarse_length == min(coarse_length, na.rm = TRUE))  %>% # if roottype == "fine", filter for scan with smallest length of coarse roots
  ungroup()

length(unique(dat_scanfine$id_comments)) # seems to have 122 distinct individuals (1 row less than before filtering?)

# check why there is one individual less now
inds_scan_fine <- dat_scan %>%
  filter(roottype == "fine") %>%
  group_by(id_comments) %>%
  summarize(count_inds = n_distinct(id_comments), .groups = "drop") # 123 individuals
inds_scanfine_fine <- dat_scanfine %>%
  filter(roottype == "fine") %>%
  group_by(id_comments) %>%
  summarize(count_inds = n_distinct(id_comments), .groups = "drop") # 122 individuals

anti_join(inds_scan_fine, inds_scanfine_fine, by = "id_comments") # it's the nes4.2Plamed17fine which is now deleted in dat_scanfine

# extract info on individuals to later compare to indiviuals of total roots
inds_scanfine_fine <- dat_scanfine %>%
  filter(roottype == "fine") %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets) %>%
  summarize(count_inds = n_distinct(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets), .groups = "drop")
  
# 2) prepare total root scans
dat_scantotal <- dat_scan %>%
  mutate(count_scans = n_distinct(`Length(cm)`), .groups = "drop") %>% 
  filter(roottype == "total") %>%
  filter((id_comments %nin% as.vector(double_scan_length$`RHIZO 2009`)) | # individuals not in double_scan_length are alright (no double scans)
        (id_comments %in% as.vector(double_scan_length$`RHIZO 2009`) & `Length(cm)` %in% as.vector(double_scan_length$`Length(cm)`)))  # if there are double scans, keep the scans which correspond to the saved lengths (it's the scan later in time)


length(unique(dat_scantotal$id_comments)) # seems to have 122 distinct individuals

# find out which individuals don't have a corresponding fine root scan --> fixed above
#inds_scan_total <- dat_scan %>%
#  filter(roottype == "total") %>%
#  group_by(id_comments) %>%
#  summarize(count_inds = n_distinct(id_comments), .groups = "drop") # 122 individuals in dat_scan 
#inds_scantotal_total <- dat_scantotal %>%
#  filter(roottype == "total") %>%
#  group_by(id_comments) %>%
#  summarize(count_inds = n_distinct(id_comments), .groups = "drop") # 120 individuals in dat_scantotal
#
#anti_join(inds_scan_total, inds_scantotal_total, by = "id_comments") # it's the two salpras whcih were counted as duplicated scans but was wrong labelling of fine vs. total scan


# extract info on individuals to  compare to indiviuals of fine roots
inds_scantotal_total <- dat_scantotal %>%
  filter(roottype == "total") %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets) %>%
  summarize(count_inds = n_distinct(region, site, block_ID_original, plot_ID_original, position_ID_original, species, roottype, subsets), .groups = "drop")


# compare individuals in fine and total root data
anti_join(inds_scanfine_fine, inds_scantotal_total, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species", "subsets")) 

# perfect fit!

# add back together
dat_scan_corr <- bind_rows(dat_scanfine, dat_scantotal) 


# check whether fine roots always have a smaller section than 0.5% of total roots of course roots still in them
dat_scan_check <- dat_scan_corr %>%
  filter(roottype == "fine") %>%
  mutate(`Length(cm)` = as.numeric(`Length(cm)`),
         coarse_length_leftover = `2.0000000<.L.<=5.0000000` + `5.0000000<.L.<=10.0000000`) %>%
  mutate(fine_percentage = (coarse_length_leftover / `Length(cm)`) * 100)

# for 122 individuals, 119 have less than 0.5% of coarse roots (>2mm) still in the fine roots sample --> 3 Silvuls have >1%

# create data frame with infos on al the collected individuals
dat_scan_individuals <- dat_scan_corr %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets) %>%
  summarize(count_inds = n_distinct(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets), .groups = "drop")

  
### BIOMASS DATA 

dat_biomass <- dat_biomass_original

# separate ID columns, clean up, rename
dat_biomass <- dat_biomass %>%
  mutate(site = if_else(grepl("es", Site) == TRUE, "lo", "hi"),
         species = tolower(Specie),
         date_collection = as.Date(`Collection date`, format = "%d.%m.%Y"),
         position_ID_original = as.numeric(`Plant ID`)) %>%
  separate(`Plot ID`, c("block_ID_original", "plot_ID_original"), sep ="\\.") %>%
  mutate(block_ID_original = as.numeric(block_ID_original),
         plot_ID_original = as.numeric(plot_ID_original),
         roottype = if_else(grepl("ine", `Sample type`) == TRUE, "fine", "total"),
         region = "CHE",
         comments = tolower(comments),
         subsets = str_extract(comments, "subset\\s?[[:alnum:]]{1}"),
         subsets = gsub(" ", "", subsets),
         subsets = if_else(subsets == "subseta", NA, subsets),
         subsets = if_else(is.na(subsets) == TRUE, "none", subsets)) %>%
  rename("dry_mass_roots" = "weight") %>%
  dplyr::select(region, site, block_ID_original, plot_ID_original, position_ID_original, species, date_collection, roottype, dry_mass_roots, comments, subsets)

# clean up some mistakes with dates based on collection dates and other samples individuals, plus fix a wrong position of a scacol (checked on scans of labels)
dat_biomass <- dat_biomass %>%
  mutate(date_collection = case_when(site == "hi" & block_ID_original == 1 & plot_ID_original == 3 & species == "medlup" ~ ymd("2023-08-31"), 
                                     site == "lo" & block_ID_original == 10 & plot_ID_original == 1 & position_ID_original == 23 &species == "scacol" ~ ymd("2023-08-16"), 
                                     site == "lo" & block_ID_original == 1 & plot_ID_original == 1 & position_ID_original == 18 & species == "hypper" ~ ymd("2023-08-16"), 
                                     site == "lo" & block_ID_original == 8 & plot_ID_original == 1 & position_ID_original == 11 & species == "silvul" ~ ymd("2023-08-09"), 
                                     TRUE ~ date_collection),
         position_ID_original = if_else(site == "hi" & block_ID_original == 1 & plot_ID_original == 3 & position_ID_original == 16 & species == "scacol", 6, position_ID_original))

# how many individuals?
dat_biomass_unique <- dat_biomass %>%
  distinct(region, site, block_ID_original, plot_ID_original, species, position_ID_original, subsets) #%>%
  #count() # 126 rows 
dat_biomass_original %>%
  distinct(Site,`Plot ID`, `Plant ID`, Specie) %>%
  count() # 124 --> should be 122?


# errors detected when combining with scan data: rows only in scan
scan_biomass_check <- anti_join(dat_scan_corr, dat_biomass, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

dat_biomass %>%
  filter(species == "cenjac" & block_ID_original %in% c(1, 10))
dat_scan_corr %>%
  filter(species == "cenjac" & block_ID_original %in% c(1, 10)) %>%
  dplyr::select(region, site, block_ID_original, plot_ID_original, position_ID_original, roottype)

# - cenjac30 lo 1.1, fine and total: there are no cenjac in either 1.1 or 10.1 at hi site (scans of labels checked) --> biomass label of hi 10.1 cenjac 30 is wrong
# - broere 7 hi 5.4, fine and total: is alright, error in biomass data

# errors detected when combining with scan data: rows only in biomass
scan_biomass_check <- anti_join(dat_biomass, dat_scan_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

dat_scan_corr %>%
  filter(species == "salpra" & site == "hi" & block_ID_original == 2) %>%
  dplyr::select(id_comments, roottype)
dat_scan_corr %>%
  filter(species == "scacol" & site == "lo" & block_ID_original == 3) %>%
  dplyr::select(id_comments, roottype)

# - cenjac30 lo/hi 10.1, total/fine: should be the same individual (lo, as in scans)
# - broere9 hi 5.4, fine and total: should be position 7, as in scans (there is no position 9 for broere)
# - salpra15 hi 2.3, fine: collected on 04.09.2023 --> is position 19 in scan (no salpra at position 15)
# - scacol29 lo 3.2, fine: collected on 23.08.2023 --> is position 23 in scan (no scacol at position 29)
# - no name: was deleted in scans

# fix the problems
dat_biomass_corr <- dat_biomass %>%
  mutate(position_ID_original = case_when(species == "salpra" & site == "hi" & block_ID_original == 2 & plot_ID_original == 3 ~ 19,
                                          species == "scacol" & site == "lo" & block_ID_original == 3 & plot_ID_original == 2 ~ 23,
                                          species == "broere" & site == "hi" & block_ID_original == 5 & plot_ID_original == 4 ~ 7,
                                          TRUE ~ position_ID_original),
         site = if_else(species == "cenjac" & position_ID_original == 30 & block_ID_original == 10 & plot_ID_original == 1 & date_collection == ymd("2023-08-09"), "lo", site)) %>%
  filter(!is.na(species))

### TEST THE MERGE

# check again for perfect match between scans and biomass
anti_join(dat_scan_corr, dat_biomass_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))
anti_join(dat_biomass_corr, dat_scan_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

# there's still errors with the cenjacs in plots 1.1 and 10.1

dat_biomass_corr %>%
  filter(species == "cenjac" & block_ID_original %in% c(1, 10))
dat_scan_corr %>%
  filter(species == "cenjac" & block_ID_original %in% c(1, 10)) %>%
  dplyr::select(region, site, block_ID_original, plot_ID_original, position_ID_original, roottype)

# the problem is a cenjac at the lo site:
# - matching cenjacs in 1.1 14
# - matching cenjacs in 10.1 14
# - 10.1 30 in biomass, 1.1 30 in scans --> there's no lo 1.1 cenjac30 scanned labels, so this must be lo 10.1 cenjac30 instead in scan data

dat_scan_corr2 <- dat_scan_corr %>%
  mutate(block_ID_original = if_else(species == "cenjac" & block_ID_original == 1 & plot_ID_original == 1 & position_ID_original == 30, 10, block_ID_original))

# check again
anti_join(dat_scan_corr2, dat_biomass_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))
anti_join(dat_biomass_corr, dat_scan_corr2, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets"))

# perfect match - now also check for roottype
anti_join(dat_scan_corr2, dat_biomass_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))
anti_join(dat_biomass_corr, dat_scan_corr2, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))

# count how many roottypes per individual for both scans and biomass
notype_scan <- dat_scan_corr2 %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets) %>%
  summarize(no_roottypes = n_distinct(roottype)) # always two types per individual
notype_bio <- dat_biomass_corr %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets) %>%
  summarize(no_roottypes = n_distinct(roottype)) # always two types per individual

# check out scacol23 lo 5.2
dat_biomass_original %>% filter(Specie == "Scacol" & `Plot ID` == "5.2")
dat_scan_original %>% filter(grepl("5.2Scacol", `RHIZO 2009`) == TRUE)

# only 1 type for lo 5.2 scacol23 in weighting (total), while there's also 1 scan (total, but it was transformed into "fine" later as it was assumed there's only fine roots)
# for now: delete scacol23 lo 5.2 with NAs (total) in scan data and change the roottype in biomass data
dat_scan_corr3 <- dat_scan_corr2 %>%
  filter(!(species == "scacol" & site == "lo" & block_ID_original == 5 & plot_ID_original == 2 & roottype == "total")) 
# for now: change roottype in biomass for this scacol to "fine"
dat_biomass_corr <- dat_biomass_corr %>%
  mutate(roottype = if_else(site == "lo" & block_ID_original == 5 & plot_ID_original == 2 & species == "scacol" & position_ID_original == 23, "fine", roottype))

# check the roottype merge again
anti_join(dat_scan_corr3, dat_biomass_corr, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))
anti_join(dat_biomass_corr, dat_scan_corr3, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))


# count individuals for scan and biomass
dat_scan_corr3 %>%
  summarize(no_inds = n_distinct(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets))
dat_biomass_corr %>%
  summarize(no_inds = n_distinct(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets))


# now there's a perfect match, but dat_scan_corr3 had 243 rows with 122 individuals (scacol23 lo 5.2 only one row)...
# ...while dat_biomass_corr has 245 rows with 122 individuals  (scacol23 lo 5.2 only one row).


# there must be duplicates in biomass
norow_bio <- dat_biomass_corr %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, subsets, roottype) %>%
  summarize(no_roottypes_inds = n_distinct(dry_mass_roots))

# two measurements for...
# - broere7 hi 1.3 (fine)
# - scacol23 lo 3.2 (fine)

dat_biomass_original %>%
  filter(Specie == "Broere" & `Plot ID` == "1.3" & Site == "Cal") # total roots measured twice, once by Rahel and once by Kevin (keep Kevins as he measured the fine as well)
dat_biomass_original %>%
  filter(Specie == "Scacol" & `Plot ID` == "3.2" & Site == "Nes") # Kevin measured a Nes_3.2 Scacol29? --> delete

# fix the problems
dat_biomass_corr2 <- dat_biomass_corr %>%
  filter(!(species == "broere" & site == "hi" & dry_mass_roots == 12.4160)) %>%
  filter(!(species == "scacol" & site == "lo" & dry_mass_roots == 0.5717))

# try the merge again
anti_join(dat_scan_corr3, dat_biomass_corr2, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))
anti_join(dat_biomass_corr2, dat_scan_corr3, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets", "roottype"))

# now we're good

### PREPARATION: COMBINE & CALCULATE ###########################################

### COMBINE

# What columns are required?
# - totlength_roots: total length of scanned root sample
# - totlength_fineroots: total length of scanned fine roots
# - dry_mass_roots: total dry mass of all roots (fine roots of subsample plus rest)
# - dry_mass_fineroots: dry mass of scanned fine roots
# - volume_fineroots, diameter_fineroots, projarea_fineroots, surfarea_fineroots: volume, diameter, projected and surface area of fine root subsample

# make a data frame with one row for each individual and a column for scan_total, scan_fine, weight_total and weight_fine
datscan_wide <- dat_scan_corr3 %>%
  dplyr::select(region, site, block_ID_original, plot_ID_original, species, position_ID_original, subsets, roottype, `Length(cm)`, 
                `ProjArea(cm2)`, `SurfArea(cm2)`, `AvgDiam(mm)`, `RootVolume(cm3)`) %>%
  rename("length" = "Length(cm)", "projarea" = "ProjArea(cm2)", "surfarea" = "SurfArea(cm2)", "avgdia" = "AvgDiam(mm)", "rootvol" = "RootVolume(cm3)") %>%
  pivot_wider(names_from = roottype, 
              values_from = c(length, projarea, surfarea, avgdia, rootvol), 
              names_glue = "{.value}_{roottype}") # 122 rows, correct

# rename according to data overview
datscan_wide <- datscan_wide %>%
  rename("length_sample_total" = "length_total", "length_sample_fine" = "length_fine", "vol_sample_fine" = "rootvol_fine",
         "dia_sample_fine" = "avgdia_fine", "parea_sample_fine" = "projarea_fine", "sarea_sample_fine" = "surfarea_fine")
  
# now make biomass data wide
datbio_wide <- dat_biomass_corr2 %>%
  dplyr::select(-comments) %>%
  pivot_wider(names_from = roottype, 
              values_from = c(dry_mass_roots), 
              names_glue = "{.value}_{roottype}") # 122 rows, correct

# check out the two NA's
# - scacol23 lo 5.2: there is only one scan and one biomass, both labelled as "fine" (complete sampe scanned with no roots > 2mm)
# - cenjac25 hi 4.1: there is only one scan and one biomass, both labelled as "fine" (complete sampe scanned with no roots > 2mm)

# rename according to data overview, create total biomass (this is both measurements added together!)
datbio_wide <- datbio_wide %>%
  rename("dry_mass_sample_fine" = "dry_mass_roots_fine") %>%
  rowwise() %>%
  mutate(dry_mass_complete = sum(dry_mass_roots_total, dry_mass_sample_fine, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(-dry_mass_roots_total)

# now add together, delete columns not in data overview
dat_roots <- datscan_wide %>%
  left_join(datbio_wide, by = c("region", "site", "block_ID_original", "plot_ID_original", "species", "position_ID_original", "subsets")) %>%
  dplyr::select(-c(projarea_total, surfarea_total, avgdia_total, rootvol_total))

# delete one subset for individuals with two subsets scanned and weighted: for the complete biomass add the two total biomasses
silvul22_lo_1.1 <- dat_roots %>%
  filter(site == "lo" & block_ID_original == 1 & plot_ID_original == 1 & position_ID_original == 22 & species == "silvul") %>%
  dplyr::select(dry_mass_complete) %>%
  summarize(dry_mass_complete = sum(dry_mass_complete)) %>%
  pull(dry_mass_complete)
cenjac14_hi_2.3 <- dat_roots %>%
  filter(site == "hi" & block_ID_original == 2 & plot_ID_original == 3 & position_ID_original == 14 & species == "cenjac") %>%
  dplyr::select(dry_mass_complete) %>%
  summarize(dry_mass_complete = sum(dry_mass_complete)) %>%
  pull(dry_mass_complete)

dat_roots <- dat_roots %>%
  filter(!(subsets %in% c("subset2", "subsetb"))) %>%
  mutate(dry_mass_complete = case_when(site == "lo" & block_ID_original == 1 & plot_ID_original == 1 & position_ID_original == 22 & 
                                         species == "silvul" ~ silvul22_lo_1.1,
                                       site == "hi" & block_ID_original == 2 & plot_ID_original == 3 & position_ID_original == 14 & 
                                         species == "cenjac" ~ cenjac14_hi_2.3,
                                       TRUE ~ dry_mass_complete))

### MAKE CALCULATIONS

# possible traits to calculate:
# - root mass fraction: root dry mass/total plant dry mass
# - fine root proportion: total length of fine roots / total root length
# - average fine root diameter
# - fine root density: dry mass of fine roots / volume of fine roots
# - specific root length: length of fine roots / dry mass of fine roots

# calculate SRL etc. (make columns numeric before)
dat_roots <- dat_roots %>%
  mutate(across(c(length_sample_fine, length_sample_total, dry_mass_sample_fine, vol_sample_fine, parea_sample_fine, sarea_sample_fine,
                  dia_sample_fine, dry_mass_complete), as.numeric)) %>%
  mutate(fineroot_prop = if_else(is.na(length_sample_total) == TRUE, 1, length_sample_fine/length_sample_total), # if there were no not-fine roots, the ration is automatically 100%
         SRL = length_sample_fine/dry_mass_sample_fine,
         fineroot_density = dry_mass_sample_fine/vol_sample_fine)

# manually set fine root proportion to max. 1
dat_roots <- dat_roots %>%
  mutate(fineroot_prop = if_else(fineroot_prop > 1, 1, fineroot_prop))

### ADD TREATMENTS #############################################################

# select oldest ind_number for all positions
meta_plant2022_23 <- meta_plant %>%
  group_by(region, site, block_ID_original, plot_ID_original, position_ID_original, species, functional_group, treat_warming, treat_competition, added_focals, block_ID,
           position_ID, unique_plot_ID) %>%
  filter(ind_number == max(ind_number)) %>%
  ungroup() # exactly 1800, nice

# merge treatments to 2022 size data frame
dat_roots_full <- full_join(dat_roots, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# 1 row only in dat_roots - which one?
anti_join(dat_roots, meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))

# it's the silvul with missing plot --> delete
dat_roots_ready <- dat_roots %>%
  filter(is.na(block_ID_original) == FALSE) %>%
  left_join(meta_plant2022_23, by = c("region", "site", "block_ID_original", "plot_ID_original", "position_ID_original", "species"))




### CONTROL PLOTTING ###########################################################

ggplot(data = dat_roots_ready, aes(x = species, y = length_sample_fine, col = species)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ site) # looks alright, without any enormous outliers
ggplot(data = dat_roots_ready, aes(x = species, y = length_sample_total, col = species)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ site) # looks alright, without any enormous outliers

ggplot(data = dat_roots_ready, aes(x = species, y = dry_mass_sample_fine, col = species)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ site) # 1 very improbable scacol!
ggplot(data = dat_roots_ready, aes(x = species, y = dry_mass_complete, col = species)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ site) # 1 quite high scacol and 1 quite high cenjac

# re-weighted: seems to be true

ggplot(data = dat_roots_ready, aes(x = species, y = fineroot_prop, col = species)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ site) # only silvul with prop. not 1 or close to 1 --> makes sense, only species with thicker roots
ggplot(data = dat_roots_ready, aes(x = species, y = fineroot_density, col = species)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ site) # again, the crazy oulier in scacol...
ggplot(data = dat_roots_ready, aes(x = species, y = SRL, col = species)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ site) # looks alright

### SAVE CLEAN VERSION #########################################################

# select and re-roder columns
cols_roots <- c("unique_plant_ID", "species", "date_collection", "length_sample_total", "length_sample_fine", "dry_mass_complete", 
                "dry_mass_sample_fine", "vol_sample_fine", "dia_sample_fine", "parea_sample_fine", "sarea_sample_fine", "fineroot_prop",
                "SRL", "fineroot_density")
dat_roots_ready <- dat_roots_ready %>%
  dplyr::select(all_of(cols_roots))

# save
write.csv(dat_roots_ready, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Traits/Roots/RangeX_clean_RootTraits_2023_CHE.csv",
          row.names = FALSE)









