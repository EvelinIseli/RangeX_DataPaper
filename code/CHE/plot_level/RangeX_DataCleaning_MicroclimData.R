################################################################################
### MICRO CLIMATE DATA RANGEX 2021 - 2023  #####################################
################################################################################

################################################################################

### Data used           : all raw environmental data read out (year by year), RangeX_clean_MetadataPlot_CHE.csv, RangeX_HOBO_metadata_20241121.csv, RangeX_TMS4_metadata_20241121.csv
### Date last modified  : 27.11.2024
### Purpose             : load all environmental data (separately for the three types of loggers) for all years, combine data over all years, get rid of unreliable data and clean everything

################################################################################

rm(list = ls())

### packages etc. ##############################################################

# basic packages
library(tidyverse); library(tidylog) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex

# task-specific packages (include short description of what it is used for)
library(plotrix) # for std.error()
library(lubridate) # date stuff
library(chron) # times independent of dates
library(myClim) # package to handle logger data (especially tms4)

### functions ##################################################################


# function to identify date time format within a loop (parse with different formats and check for success)
# first define candidate date-time formats to try
date_formats <- c("%d.%m.%Y %H:%M:%S", "%m.%d.%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%d.%m.%Y %H:%M", "%m.%d.%Y %H:%M", "%Y.%m.%d %H:%M", "%Y/%m/%d %H:%M")


detect_date_format <- function(x) {
  for (format in date_formats) {
    if (!is.na(strptime(x, format, tz = "GMT"))) {
      return(format)
    }
  }
  return(NULL)
}

### general data ###############################################################

plot_key <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Metadata/RangeX_clean_MetadataPlot_CHE.csv")

# read & prepare metadata
hobo_metadata <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/Metadata & Cleaning Scripts/RangeX_HOBO_metadata_20241121.csv")
tms_metadata <- read_csv("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/Metadata & Cleaning Scripts/RangeX_TMS4_metadata_20241121.csv")


################################################################################
# TEMPERATURE DATA: HOBOs ######################################################
################################################################################

### LOAD DATA SET ##############################################################

# prepare metadata
hobo_metadata <- hobo_metadata %>%
  rename("hobo_name" ="Name") %>%
  mutate(Reading_Date1 = as.Date(Reading_Date1, format = "%d.%m.%Y"),
         Reading_Date2 = as.Date(Reading_Date2, format = "%d.%m.%Y"),
         Reading_Date3 = as.Date(Reading_Date3, format = "%d.%m.%Y"),
         Measuring_START = as.Date(Measuring_START, format = "%d.%m.%Y"),
         Measuring_END = as.Date(Measuring_END, format = "%d.%m.%Y"),
         NotOK_When = as.Date(NotOK_When, format = "%d.%m.%Y")) %>%
  dplyr::select(-matches("X"))

# prepare for reading all the HOBO data files collected between 2021 - 2024

# save all working directories in a data frame
directories <- data.frame(c(2021, 2021, 2022, 2022, 2022, 2022, 2023, 2023, 2023, 2023, 2024), 
                          c("Autumn", "Autumn", "Spring", "Spring", "Autumn", "Autumn", "Spring", "Spring", "Autumn", "Autumn", "Spring"), 
                          c("Nes", "Cal", "Nes", "Cal", "Nes", "Cal", "Nes", "Cal", "Nes", "Cal", "Cal"), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
colnames(directories) <- c("Year", "Season", "Site", "Directory_0")
directories$Directory_0 <- c("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/HOBO Data/RangeX_HOBO_")

directories <- directories %>%
  mutate("Directory_1" = paste0(Directory_0, Season, Year, "/", "HOBO ", Site, " ", Season, " ", Year))


# save all measuring/ reading dates for all hobo's to later delete
reading_dates <- hobo_metadata %>%
  dplyr::select(hobo_name, Plot, Reading_Date1, Reading_Date2, Reading_Date3, Measuring_START, Measuring_END) %>%
  group_by(hobo_name, Plot) %>%
  gather("Type", "Dates", 3:7) %>%
  drop_na(Dates) 

# different HOBOs at different read outs have different date time formats: import one file from each season and manually define the format of this season
# the columns will be: date/time, file name, location (folder)
dateformat_check <- data.frame(date_time = character(), file_name = character(), location = character(), stringsAsFactors = FALSE)

for (m in 1:11) {
  
  setwd(directories[m, 5])  # set working directory to the folder path from directories
  
  # list all CSV files in the directory
  names <- list.files(pattern = "csv")
  
  # loop through each CSV file 
  for (i in 1:length(names)) {
    
    # read the file and format the columns
    dat <- read.csv(names[i], header = FALSE)  
    colnames(dat) <- dat[2, ]  # define the second row as column names (weird HOBO file format)
    dat <- dat[-c(1, 2), ]  # remove the first two rows, as they are not data
    
    # extract the date and time 
    date_time <- dat[1, 2]  # extract the first date/time entry from the column
    
    # get the file name
    file_name <- names[i]
    
    # extract the location (the last folder in the directory path)
    location <- basename(getwd())  # get the last part of the path (i.e., the folder name)
    
    # add the extracted data to the result data frame
    dateformat_check <- rbind(dateformat_check, data.frame(date_time = date_time, file_name = file_name, location = location, stringsAsFactors = FALSE))
  }
}


# assign a date time format to each read out (Nes/ Cal Autumn/ Spring year)
locations <- c("HOBO Nes Autumn 2021", "HOBO Cal Autumn 2021", "HOBO Nes Spring 2022", "HOBO Cal Spring 2022", 
                 "HOBO Nes Autumn 2022", "HOBO Cal Autumn 2022", "HOBO Nes Spring 2023", "HOBO Cal Spring 2023", 
                 "HOBO Nes Autumn 2023", "HOBO Cal Autumn 2023", "HOBO Cal Spring 2024")
formats <- c("%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p", "%d/%m/%y", "%d/%m/%y", 
              "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p",  "%y.%m.%d %I:%M:%S %p", "%y.%m.%d %I:%M:%S %p", 
             "%y.%m.%d %I:%M:%S %p", "%y.%m.%d %I:%M:%S %p", "%y.%m.%d %I:%M:%S %p")

dateformat_assigned <- tibble(location = locations, format = formats)


# now get all the temperature data and set up a nicely organised data frame:
# 1) loop through all folders
# 2) loop through all files in a folder
# 3) read the file
# 4) assign the correct date format, delete unnecessary columns
# 5) manually re-assign necessary information (bad battery or similar)

# prepare data frame
hobo_temp_complete <- data.frame(number = NA, temp = NA, hobo_name = NA, BadBattery = NA, CouplerAttached = NA, HostConnected = NA, Stopped = NA, EndOfFile = NA, date = NA, time = NA, timezone = NA)
hobo_temp <- data.frame(number = NA, temp = NA, hobo_name = NA, BadBattery = NA, CouplerAttached = NA, HostConnected = NA, Stopped = NA, EndOfFile = NA, date = NA, time = NA, timezone = NA)
hobo_timezone_complete <- data.frame(hobo_name = NA, timezone_info = NA, timezone_info_spring22 = NA)
hobo_timezone <- data.frame(hobo_name = NA, timezone_info = NA, timezone_info_spring22 = NA)

# start the loop
for (m in 1:11) {
  
  location_now <- directories[m, 5] # save location of current folder 
  setwd(location_now) # set working directory of one folder
  
  names <- list.files(pattern = "csv") # get name sof all files to read in that specific folder
  
  for (i in 1:length(names)) {
    dat <- read.csv(names[i], header = FALSE) # read file
    colnames(dat) <- dat[2,] # define second row as column names (based on weird format of hobo file)
    timezoneinfo <- dat[2,2] # save timezone info from metadata in first two rows
    timezoneinfo_spring22 <- dat[2,3]
    dat <- dat[-c(1,2),] # get rid of the weird rows
    
    # PROBLEM: in all read outs except spring 2022, there is ONE date-time column - in spring 2022 there is a Date and a Time column
    # --> assign different column numbers for spring 2022...
    if(grepl("Spring2022", directories[m, 5]) == TRUE) {
      dat_sm <- dat[, c(1:4)] # get rid of all weird columns (all except temp., date and #)
      colnames(dat_sm) <- c("number", "date", "time", "temp") 
      
      # get timezoneinfo & fix date
      timezone_offset <- timezoneinfo_spring22 %>% str_extract("GMT([+-]\\d{2})") %>% str_replace("0", "")
      
      # transform date & time column into real date-time for R
      dat_sm$date_time <- as.POSIXct(as.character(paste(dat_sm$date, dat_sm$time)), format="%d/%m/%y %H:%M:%S", tz = paste0("Etc/", timezone_offset)) # the timezone setting differs between loggers, i.e. use the info from each files metadata
      dat_sm <- dat_sm %>% dplyr::select(-time, -date)
      
      dat_sm <- dat_sm[, c(1, 3, 2)]
      
    } else {
      dat_sm <- dat[, c(1:3)] # get rid of all weird columns (all except temp., date and #)
      colnames(dat_sm) <- c("number", "date_time", "temp")
      
      # look up date format of current file and folder
      date_format <- dateformat_assigned %>%
        filter(location == basename(location_now)) %>%
        pull(format)
      
      # get timezoneinfo
      timezone_offset <- timezoneinfo %>% str_extract("GMT([+-]\\d{2})") %>% str_replace(":00$", "") %>% str_replace("0", "")
      
      # transform dat & time column into real date-time for R 
      dat_sm$date_time <- as.POSIXct(dat_sm$date_time, format = date_format, tz = paste0("Etc/",timezone_offset))

    }
  
   # create a date column to interact with metadata
   dat_sm$date <- as.Date(dat_sm$date_time) 
    
    
    dat_sm$hobo_name <- paste(names[i]) %>% # add column with hobo name
      str_sub(end = -5)
    dat_sm$timezone <- timezone_offset
    
    # save timezone info in separate data frame to look at later
    dat_timezone = data.frame(hobo_name = paste(names[i]), timezone_info = timezoneinfo, timezone_info_spring22 = timezoneinfo_spring22)
    
    
    # light intensity can be ignored, but add rest of weird columns step by step so all dat_sm have identical columns to be added together
    
    #add bad battery if present
    dat_sm$BadBattery = NA
    idx_1 <- which(sapply("Bad", grepl, colnames(dat))) # check whether one of the columns in dat contains "bad"
    if(length(idx_1) == 1){ # if yes (idx_1 == 1), then add the column to dat_sm
      dat_sm$BadBattery <- dat[,idx_1]
    } 
    dat_sm$BadBattery <- ifelse(dat_sm$BadBattery == "", NA, dat_sm$BadBattery) 
    # why is this necessary? if a logger is read multiple times sometimes length(idx) == 1, i.e. there's either a word in the cell or "", and sometimes it's != 1, resulting in NA
    # this makes sense, because: for example "EndOfFile" is of course only true the first time when read out, dito "CouplerAttached"
    # therefore: add NA's instead of the "" so those duplicate rows are later indeed duplicates (and not NA vs. "") and are deleted
    
    #add coupler attached if present
    dat_sm$CouplerAttached = NA
    idx_2 <- which(sapply("Coup", grepl, colnames(dat)))
    if(length(idx_2) == 1){
      dat_sm$CouplerAttached <- dat[,idx_2]
    } 
    dat_sm$CouplerAttached <- ifelse(dat_sm$CouplerAttached == "", NA, dat_sm$CouplerAttached)
    
    #add host connected if present
    dat_sm$HostConnected = NA
    idx_3 <- which(sapply("Host", grepl, colnames(dat)))
    if(length(idx_3) == 1){
      dat_sm$HostConnected <- dat[,idx_3]
    } 
    dat_sm$HostConnected <- ifelse(dat_sm$HostConnected == "", NA, dat_sm$HostConnected)
    
    # add stopped if present
    dat_sm$Stopped = NA
    idx_4 <- which(sapply("Stop", grepl, colnames(dat)))
    if(length(idx_4) == 1){
      dat_sm$Stopped <- dat[,idx_4]
    } 
    dat_sm$Stopped <- ifelse(dat_sm$Stopped == "", NA, dat_sm$Stopped)
    
    # add end of file
    dat_sm$EndOfFile= NA
    idx_5 <- which(sapply("End", grepl, colnames(dat)))
    if(length(idx_5) == 1){
      dat_sm$EndOfFile <- dat[,idx_5]
    } 
    dat_sm$EndOfFile <- ifelse(dat_sm$EndOfFile == "", NA, dat_sm$EndOfFile)
    
    # filter only useable data
    hobo_name_current <- str_sub(names[i], end = -5)
    hobo_metadata_current <- hobo_metadata[hobo_metadata$hobo_name == hobo_name_current, ] %>%
      filter(Reason_Restart != "outnow") %>% # ignore the HOBOs still out in the field
      mutate(Measuring_END = if_else(is.na(NotOK_When) == FALSE & NotOK_When < Measuring_END, NotOK_When, Measuring_END)) # if the logger stopped logging before read out, stop the data used then
    dat_sm <- dat_sm %>%
      filter(between(date, as.Date(hobo_metadata_current[1,]$Measuring_START, "%d.%m.%y") +1, as.Date(hobo_metadata_current[1,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(hobo_metadata_current[2,]$Measuring_START, "%d.%m.%y") +1, as.Date(hobo_metadata_current[2,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(hobo_metadata_current[3,]$Measuring_START, "%d.%m.%y") +1, as.Date(hobo_metadata_current[3,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(hobo_metadata_current[4,]$Measuring_START, "%d.%m.%y") +1, as.Date(hobo_metadata_current[4,]$Measuring_END, "%d.%m.%y") -1))
    
    # NOTE: add as many "between"s as there's possible cycles for a single hobo
    
    # the reading days in files containing long time periods need to be deleted! i.e. just delete all dates which occur in a specific loggers metadata (saved as ReadingDateX)
    dat_sm <- dat_sm %>%
      filter(!(date %in% reading_dates[reading_dates$hobo_name == hobo_name_current, ]$Dates))
 
    # add the data frame to the prepared one for all hobos
    if(i == 1){
      hobo_temp <- dat_sm
      hobo_timezone <- dat_timezone
    } else {
      hobo_temp <- rbind(hobo_temp, dat_sm)
      hobo_timezone <- rbind(hobo_timezone, dat_timezone)
    }
    
  }
  
  # add the data frame to the prepared one for all hobos
  if(m == 1){
    hobo_temp_complete <- hobo_temp
    hobo_timezone_complete <- hobo_timezone
  } else{
    hobo_temp_complete <- rbind(hobo_temp_complete, hobo_temp) 
    hobo_timezone_complete <- rbind(hobo_timezone_complete, hobo_timezone) 
  }
  
}

# add plot: first expand metadata to all the time periods covered
hobo_metadata_exp <- hobo_metadata %>%
  mutate(Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date3, Measuring_END), # if the HOBO is not back yet, use the last Reading date as end point for now
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date2, Measuring_END), # if Reading_Date3 was NA, use Reading_Date2 etc.
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date1, Measuring_END)) %>%
  filter(is.na(Measuring_END) == FALSE) %>% # if the HOBO was never read out and is still out in the field, delete (no data available anyway)
  rowwise() %>%
  do(data.frame(hobo_name = .$hobo_name,
                Plot = .$Plot,
                Measuring_START = .$Measuring_START,
                Measuring_END = .$Measuring_END,
                date = seq(from = .$Measuring_START, to = .$Measuring_END, by = "day"))) %>%
  ungroup()

# now merge by hobo name and date
hobo_temp_complete <- left_join(hobo_temp_complete, hobo_metadata_exp[, c(1, 2, 5)], by = c("hobo_name", "date")) %>%
  rename("plot" = "Plot") %>% # the two rows only in metadata are alright: new loggers put out in 2024
  mutate(temp = as.numeric(temp))


### TIMEZONES & TIME ###########################################################

# not all loggers were always set up in the same timezone - the loop extracts the info on timezone setting and assigns the right timezone with as.POSIXct()
# --> for saving it's better to display all the date times in UTC, so users can always transform them back to Europe/Zurich time or whatever they prefer

hobo_temp_complete <- hobo_temp_complete %>%
  mutate(date_time = with_tz(date_time, tzone = "UTC"))


# plot one day of a single logger to see whether the time is wrong 
ggplot(dat = hobo_temp_complete[hobo_temp_complete$hobo_name == "RX_Cal_05" & hobo_temp_complete$date =="2022-07-25", ], aes(x = date_time, y = temp)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) # looks like time is alright!



### CHECK AND IMPROVE ##########################################################


# delete duplicated rows - should have many as the same logger data was read out multiple time for many loggers, plus save temp. as actual number
hobo_temp_complete <- hobo_temp_complete %>%
  distinct() 

# check whether any duplicates are left
hobo_temp_complete_check <- hobo_temp_complete %>%
  group_by(plot) %>%
  filter(duplicated(date_time) | duplicated(date_time, fromLast = TRUE)) %>%
  ungroup() # none, good

# check for NAs in plot, date_time or temp
any(is.na(hobo_temp_complete$date_time) | is.na(hobo_temp_complete$plot) | is.na(hobo_temp_complete$temp)) # none, good


# quick check: daily mean
hobo_temp_daily <- hobo_temp_complete %>%
  group_by(hobo_name, date) %>%
  dplyr::summarize(mean_daily_temp = mean(as.numeric(temp)))

# plot logger specific data
ggplot(dat = hobo_temp_daily, aes(x = date, y = mean_daily_temp)) +
  geom_line() +
  facet_wrap(~hobo_name)


# quick check: daily mean, but on plot level
hobo_plottemp_daily <- hobo_temp_complete %>%
  group_by(plot, date) %>%
  dplyr::summarize(mean_daily_temp = mean(as.numeric(temp))) %>%
  ungroup() %>%
  group_by(plot) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>% # fill in NAs for data points with no data for plotting
  ungroup() %>%
  mutate(is_missing = is.na(mean_daily_temp))

missing_data <- hobo_plottemp_daily %>% # filter for missing data to highlight it
  filter(is_missing)

# plot plot specific data (with missing data indicated (possible reasons: dead battery, memory full etc.))
ggplot(dat = hobo_plottemp_daily, aes(x = date, y = mean_daily_temp)) +
  geom_line() +
  facet_wrap(~plot) +
  geom_segment(data = missing_data, aes(x = date, xend = date, y = 0, yend = 30), 
               color = "lightgray", size = 1) +
  labs(x = "Date", y = "Mean daily temperature [°C]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_blank())


# looks all good


### PREPARE FOR EXPORT #########################################################

# create identifiers to add on treatments
hobo_temp_complete <- hobo_temp_complete %>%
  separate(plot, into = c("site", "block.plot"), sep = "_") %>%  
  separate(block.plot, into = c("block_ID_original", "plot_ID_original"), sep = "\\.") %>%  
  mutate(site = if_else(site == "Nes", "lo", "hi"), 
         across(c("block_ID_original", "plot_ID_original"), as.numeric)) 

# add treatments
hobo_temp_complete <- hobo_temp_complete %>%
  left_join(plot_key, by = c("site", "block_ID_original", "plot_ID_original")) # rows only in plot_key are ok (it's plots with no HOBOs)

# delete unnecessary columns
hobo_temp_complete <- hobo_temp_complete %>%
  dplyr::select(unique_plot_ID, timezone, date_time, temp) %>%
  rename("temperature" = "temp") 

hobo_temp_complete_2123 <- hobo_temp_complete %>%
  filter(date(date_time) < ymd("2023-10-10")) # only keep data from before last read out in autumn 2023 (first loggers were collected on 09.10.2023)

### SAVE CLEAN DATA SET ########################################################

write.csv(hobo_temp_complete_2123, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/HOBO Data/RangeX_clean_EnvHOBO_2021_2023_CHE.csv", row.names = FALSE )
  

################################################################################
# TEMPERATURE & SOIL MOISTURE: TMS4 ############################################
################################################################################

### READ & PREPARE METADATA #####################################################

# prepare tms metadata
tms_metadata <- tms_metadata %>%
  mutate(across(contains(c("Measuring", "Reading")), ~ as.Date(. , format = "%d.%m.%Y")))

# save all working directories of TMS data to use in a data frame
directories_tms <- data.frame(c(2022, 2023, 2023, 2024), 
                          c("Autumn", "Spring", "Autumn", "Spring"), 
                          c(NA, NA, NA, NA))
colnames(directories_tms) <- c("Year", "Season", "Directory_0")
directories_tms$Directory_0 <- c("/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/TMS4 Data/Upload/")

directories_tms <- directories_tms %>%
  mutate("Directory_1" = paste0(Directory_0, Season, " ", Year))

# save all measuring/ reading dates for all tms's to later delete
reading_dates_tms <- tms_metadata %>%
  dplyr::select(Serial_Number, Plot, Reading_Date1, Reading_Date2, Reading_Date3, Reading_Date4, Reading_Date5, Reading_Date6, Measuring_START, Measuring_END) %>%
  group_by(Serial_Number, Plot) %>%
  gather("Type", "Dates", 3:8) %>%
  drop_na(Dates) 


### READ & PREPARE LOGGER DATA #################################################

# - until autumn 2022, the whole files were read out --> data from spring 2021 - autumn 2022 is all on autumn 2022 read out
# - in spring 2023 at hi site, only read out since autumn 2022
# - in autumn 2023 on hi site, only the last year was read out --> read all the 2023 autumn data in and cut it
# - the spring 2024 read out is again complete, but it's only a subset of loggers --> read it all in and cut

# --> read out: autumn 2022, spring & autumn 2023, spring 2024, then delete duplicated rows

# problem loggers noticed during field work:
# 94213768: could not be read out in autumn 2023
# 94213775: could not be read out in spring 2024, broken
# 94213773: read out in spring 2024, but broken (breaking date will show up in TMS_T1 if there's a problem and data deleted)
# 94213772: read out in spring 2024, but broken (breaking date will show up in TMS_T1 if there's a problem and data deleted)
# 94213767: could not be read out in spring 2024, broken
# 94213766: read out in spring 2024, but broken (breaking date will show up in TMS_T1 if there's a problem and data deleted)

# problem loggers noticed during data cleaning (dat time duplicates with differing temp and moist values):
# 94213761, 94213765, 94213767, 94213769, 94213770, 94213773, 94213780 --> will all be deleted later

# column names TMS4
col_names <- c("number", "date_time", "timezone", "TMS_T1", "TMS_T2", "TMS_T3", "TMS_moist", "shake", "err_flag")

# now get all data and set up a nicely organised data frame
tms_complete <- data.frame(number = NA, date_time = NA, timezone = NA, TMS_T1 = NA, TMS_T2 = NA, TMS_T3 = NA, TMS_moist = NA, shake = NA, err_flag = NA, serial_number = NA)
tms_curr <- data.frame(number = NA, date_time = NA, timezone = NA, TMS_T1 = NA, TMS_T2 = NA, TMS_T3 = NA, TMS_moist = NA, shake = NA, err_flag = NA, serial_number = NA)


for (m in 1:length(directories_tms$Directory_1)) {
  
  location_now <- directories_tms[m, 4] # save location of current folder 
  setwd(location_now) # set working directory of one folder
  
  file_list <- list.files(pattern = "csv") # get names of all files to read in that specific folder
  
  for (i in 1:length(file_list)) {
    
    # read in file & delete first row (sometimes the first time is missing which then messes with the date time)
    dat <- read.csv2(paste0(file_list[i]), header = FALSE)
    dat <- dat[-1,]
    
    # delete empty column
    dat <- dat[, -10]
    colnames(dat) <- col_names # only add colnames now, delete again later 
    
    # add logger serial number
    serial_number <- as.numeric(str_extract(file_list[i], "[[:digit:]]{8}"))
    dat$serial_number <- serial_number
    
    # apply the format-detection function to the date time column
    detected_format <- unique(sapply(dat$date_time, detect_date_format))[[1]]
    
    # transform date and time into something sensible using the detected format
    dat$date_time <- as.POSIXct(dat$date_time, format = detected_format, tz = "Etc/GMT+1") # detected_format
    dat$date <- date(dat$date_time) # create date column to interact with metadata
    
    # get current metadata
    tms_metadata_current <- tms_metadata[tms_metadata$Serial_Number == serial_number, ] %>%
      filter(Reason_Restart != "outnow" & is.na(Cycle) == FALSE) # ignore the TMS still out in the field or not yet properly in metadata
    dat <- dat %>%
      filter(between(date, as.Date(tms_metadata_current[1,]$Measuring_START, "%d.%m.%y") +1, as.Date(tms_metadata_current[1,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(tms_metadata_current[2,]$Measuring_START, "%d.%m.%y") +1, as.Date(tms_metadata_current[2,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(tms_metadata_current[3,]$Measuring_START, "%d.%m.%y") +1, as.Date(tms_metadata_current[3,]$Measuring_END, "%d.%m.%y") -1)
             | between(date, as.Date(tms_metadata_current[4,]$Measuring_START, "%d.%m.%y") +1, as.Date(tms_metadata_current[4,]$Measuring_END, "%d.%m.%y") -1))
    
    # NOTE: add as many "between"s as there's possible cycles for a single tms (at the moment, this would be 2)
    
    # the reading days in files containing long time periods need to be deleted! i.e. just delete all dates which occur in a specific loggers metadata (saved as ReadingDateX)
    reading_dates_current <- reading_dates_tms[reading_dates_tms$Serial_Number == serial_number, ]$Dates
    dat <- dat %>%
      filter(!(date %in% reading_dates_current))
    
    # save
    if(i == 1) {
      tms_curr <- dat
    } else {
      tms_curr <- rbind(tms_curr, dat)
    }
  }
  
  # add the data frame to the prepared one for all tms
  if(m == 1){
    tms_complete <- tms_curr
  } else{
    tms_complete <- rbind(tms_complete, tms_curr) 
  }
}


# delete duplicated rows (many - some loggers were continuously read out completely every time. i.e. the beginning is in there 4 times)
tms_complete <- tms_complete %>%
  distinct()


# add plot: first expand metadata to all the time periods covered
tms_metadata_exp <- tms_metadata %>%
  mutate(Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date5, Measuring_END), # if the TMS4 is not back yet, use the last Reading date as end point for now
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date4, Measuring_END), # if Reading_Date3 was NA, use Reading_Date2 etc.
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date3, Measuring_END),
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date2, Measuring_END),
         Measuring_END = if_else(is.na(Measuring_END) == TRUE, Reading_Date1, Measuring_END)) %>%
  filter(is.na(Measuring_END) == FALSE & is.na(Measuring_START) == FALSE) %>% # if the TMS was never read out and is still out in the field, delete (no data available anyway)
  rowwise() %>%
  do(data.frame(serial_number = .$Serial_Number,
                Plot = .$Plot,
                Measuring_START = .$Measuring_START,
                Measuring_END = .$Measuring_END,
                date = seq(from = .$Measuring_START, to = .$Measuring_END, by = "day"))) %>%
  ungroup()

# now merge by serial number and date
tms_complete <- left_join(tms_complete, tms_metadata_exp[, c(1, 2, 5)], by = c("serial_number", "date")) %>%
  rename("plot" = "Plot") # the rows only in metadata are alright: new loggers put out in 2024

# get start and end date for each logger to check whether everything is included
tms_check <- tms_complete %>%
  group_by(plot) %>%
  summarize(start = min(date_time),
            end = max(date_time)) # looks good

# delete duplicates, excluding the number
tms_complete <- tms_complete %>%
  dplyr::select(-number) %>%
  distinct()

# check whether any duplicates are left
tms_complete_check <- tms_complete %>%
  group_by(plot) %>%
  filter(duplicated(date_time) | duplicated(date_time, fromLast = TRUE)) %>%
  ungroup()


### FIX THE DUPLICATE MESS #####################################################

# still duplicate date_time - plots left, with differing measurements: SEE FILE "TheTMSMess.R"

# this is a mess: don't use any loggers which have multiple data files from one read out, they seem to be somehow currupted
delete_loggers <- c("94213761", "94213765", "94213767", "94213769", "94213770", "94213773", "94213780")

# delete all loggers ever producing more than 1 file per read out
tms_complete <- tms_complete %>%
  filter(!(serial_number %in% delete_loggers))

# check whether any duplicates are left
tms_complete_check <- tms_complete %>%
  group_by(plot) %>%
  filter(duplicated(date_time) | duplicated(date_time, fromLast = TRUE)) %>%
  ungroup()

# all the duplicates left differ in the shake column, which is either -1 or 202 --> delete those duplicates as well, ignoring the shake column
tms_complete <- tms_complete %>%
  dplyr::select(-shake) %>%
  distinct()

# check whether any duplicates are left
tms_complete_check <- tms_complete %>%
  group_by(plot) %>%
  filter(duplicated(date_time) | duplicated(date_time, fromLast = TRUE)) %>%
  ungroup() # none, great


### TIMEZONES & TIME ###########################################################


# fix the times for summer and winter time 
# - the loggers are set to timezone 4, which translates into GMT+1 hour (UTC+4 quarter hours)
# - they will be saved in UTC
# - if someone wants to set them to summer/ winter time, this has to be corrected by just setting the timezone to Europe/Zurich
# - in the data paper, all environmental data is presented in UTC

# change time to UTC
tms_complete <- tms_complete %>%
  mutate(date_time = with_tz(date_time, tzone = "UTC"))



### SOIL MOISTURE CALIBRATION ##################################################


# transform the clean data into a format useable by MyClim
tms_complete_myclim <- tms_complete %>%
  rename("locality_id" = "plot", "datetime" = "date_time") %>%
  dplyr::select(datetime, TMS_T1, TMS_T2, TMS_T3, TMS_moist, locality_id) %>%
  mutate(across(contains("TMS"), ~ as.numeric(.))) 
  

# make long for myclim functions
tms_complete_myclim_long <- pivot_longer(tms_complete_myclim, cols =  c(2:5), names_to = "sensor_name", values_to = "value")  

# put in some myclim format
tms_complete_myclim_ready <- mc_read_long(tms_complete_myclim_long, sensor_ids = list(TMS_moist = mc_const_SENSOR_TMS_moist, TMS_T1 = mc_const_SENSOR_TMS_T1))

# no more warnings or duplicates

# transform the moisture
tms_complete_myclim_trans <- mc_calc_vwc(tms_complete_myclim_ready)

# extract the VWC

# initialize an empty list to store results
vwc_list <- list()

# iterate through each locality (i.e. plot) in the object
for (locality_name in names(tms_complete_myclim_trans$localities)) {
  
  # extract the VWC values, datetime, and the locality name
  vwc_values <- tms_complete_myclim_trans$localities[[locality_name]]$loggers[[1]]$sensors$VWC_moisture$values
  datetime_values <- tms_complete_myclim_trans$localities[[locality_name]]$loggers[[1]]$datetime
  
  # combine the extracted data into a temporary data frame
  temp_df <- data.frame(VWC = vwc_values,
                        locality = locality_name,
                        datetime = datetime_values)
  
  # append the temporary data frame to the list
  vwc_list[[locality_name]] <- temp_df
}

# combine all locality data frames into one
tms_complete_vwconly <- bind_rows(vwc_list) %>%
  rename(locality_id = locality) %>%
  distinct()

# add the vwc back on the data frame
tms_complete_myclim <- left_join(tms_complete_vwconly, tms_complete_myclim, by = c("datetime", "locality_id"))

check <- tms_complete_myclim %>%
  filter(is.na(TMS_T1)) # the 9'452 rows more in tms_complete_vwconly are missing time steps in tms_complete_myclim which were added (and filled with NA's) by mc_read_long()

# delete those NA rows again, only provide the data where there is data
tms_complete_myclim <- tms_complete_myclim %>%
  filter(!is.na(TMS_T1))

### CHECK AND IMPROVE ##########################################################


# delete perfect duplicated rows - should have none left
tms_complete_myclim <- tms_complete_myclim %>%
  distinct() 

# check whether any duplicates are left
tms_complete_myclim_check <- tms_complete_myclim %>%
  group_by(locality_id) %>%
  filter(duplicated(datetime) | duplicated(datetime, fromLast = TRUE)) %>%
  ungroup() # none, good

# check for NAs in plot, date_time or temp
any(is.na(tms_complete_myclim$datetime) | is.na(tms_complete_myclim$locality_id) | is.na(tms_complete_myclim$TMS_T1) |
      is.na(tms_complete_myclim$TMS_T2) | is.na(tms_complete_myclim$TMS_T3) | is.na(tms_complete_myclim$TMS_moist)) # none, good


# quick check: daily mean
tms_temp_daily <- tms_complete_myclim %>%
  mutate(date = date(datetime)) %>%
  group_by(locality_id, date) %>%
  dplyr::summarize(mean_daily_temp1 = mean(as.numeric(TMS_T1)),
                   mean_daily_temp2 = mean(as.numeric(TMS_T2)),
                   mean_daily_temp3 = mean(as.numeric(TMS_T3)),
                   mean_daily_moistraw = mean(as.numeric(TMS_moist)),
                   mean_daily_moistvwc = mean(as.numeric(VWC))) %>%
  ungroup() 

# save period of minus temperatures for soil moisture
minus_data <- tms_temp_daily %>%
  filter(mean_daily_temp1 < 0)

# plot logger specific data
ggplot(dat = tms_temp_daily) +
  geom_line(aes(x = date, y = mean_daily_temp1), col = "darkred") +
  geom_line(aes(x = date, y = mean_daily_temp2), col = "darkgreen") +
  geom_line(aes(x = date, y = mean_daily_temp3), col = "darkblue") +
  facet_wrap(~locality_id)

ggplot(dat = tms_temp_daily) +
  geom_line(aes(x = date, y = mean_daily_moistraw), col = "darkorange") +
  facet_wrap(~locality_id)
ggplot(dat = tms_temp_daily) +
  geom_line(aes(x = date, y = mean_daily_moistvwc), col = "aquamarine4") +
  facet_wrap(~locality_id) +
  geom_segment(data = minus_data, aes(x = date, xend = date, y = 0, yend = 0.5), 
               color = "lightgray", size = 1) # fits!

# problems:
# - some of the loggers broke under snow weight in winter 2024 & this is visible for sensor T1 and moisture in two plots: delete the data from those loggers from breaking point on
# - the missing values in VWC are if the T1 temperature falls below zero --> moisture values are not reliable in minus temperatures

# detect time point of loggers breaking in winter 2024, delete data from then on
min(tms_temp_daily[tms_temp_daily$mean_daily_temp1 < -15, ]$date) # loggers broke on Jan 3rd in 2024

tms_complete_myclim <- tms_complete_myclim %>%
  filter(!(locality_id %in% c("Cal_6.3", "Cal_9.3") & datetime > "2024-01-02 23:45"))

# now it looks all alright

### PREPARE FOR EXPORT #########################################################

# create identifiers to add on treatments
tms_complete_myclim <- tms_complete_myclim %>%
  separate(locality_id, into = c("site", "block.plot"), sep = "_") %>%  
  separate(block.plot, into = c("block_ID_original", "plot_ID_original"), sep = "\\.") %>%  
  mutate(site = if_else(site == "Nes", "lo", "hi"), 
         across(c("block_ID_original", "plot_ID_original"), as.numeric)) 

# add treatments
tms_complete_myclim <- tms_complete_myclim %>%
  left_join(plot_key, by = c("site", "block_ID_original", "plot_ID_original")) # rows only in plot_key are ok (it's plots with no TMS)

# delete unnecessary columns
tms_complete_myclim <- tms_complete_myclim %>%
  dplyr::select(unique_plot_ID, datetime, TMS_T1, TMS_T2, TMS_T3, TMS_moist, VWC) %>%
  rename("date_time" = "datetime") 

tms_complete_myclim_2123 <- tms_complete_myclim %>%
  filter(date(date_time) < ymd("2023-10-10")) # only keep data until last read out in 2023 (first loggers were collected on 10.10.2024)

# re-plotted, looks alright

# last check for NAs in unique_plot_id
any(is.na(tms_complete_myclim_2123$unique_plot_ID)) # none

### SAVE CLEAN DATA SET ########################################################

write.csv(tms_complete_myclim_2123, "/Users/eviseli/Desktop/RangeX/Task 1.1 Drivers/Calanda/Data/Digitalized Raw Data/Environmental Data/TMS4 Data/RangeX_clean_EnvTMS4_2021_2023_CHE.csv", row.names = FALSE )



