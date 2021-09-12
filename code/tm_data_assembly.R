###############################################################################
### Title: TrailMaster Data Assembly
### Purpose: Assemble raw files and preliminary analyses
### Author: L. Pandori
### Date Created: 11/17/20
### Last Edited: 9/9/21
###############################################################################

##### set-up #####

# load required packages
library(readxl)
library(plyr)
library(lubridate)
library(tidyverse)


##### read in and tidy data ####

# read metadata - tells about manual QA/QC process, which files to keep, remove, etc.
tmeta <- read_excel("data/Metadata/TrailMaster_MetaData.xlsx", 
                    col_types = c("numeric", "date", "date", 
                                  "numeric", "numeric", "numeric", 
                                  "text", "text", "text", "numeric", 
                                  "numeric", "text", "text", "text", 
                                  "text", "text", "text", "text"))
# tidy metadata
  tmeta <- tmeta %>%
    # make column names lower case
    rename_all(tolower)
    
# read data files 
  tmdata <- 
    # make a list of csv files in the corrected raw data folder
    # note - corrected raw data are raw data w/ standardized col names
    list.files(path = 'data/corrected_raw_data/', pattern = '*.csv') %>%
    # read in all the files, appending the path before the filename
    map(~ read_csv(file.path('data/corrected_raw_data/', .))) %>% 
    # rbind (vertical bind) files
    reduce(rbind)

# tidy data files
  
  # 3786248 rows
  
# tidy trailmaster (tm) data
  tmdata <- tmdata %>% 
    # rename columns as lower case
    rename_all(tolower) %>%
    # trim last column and notes column
    select(event:end & !notes) %>%
    # parse dates and times
    mutate (start = mdy(start),
            # start and end as dates in month/day/year format
            end = mdy(end),
            # time-stamp from TrailMaster as datetime object
            datetime = mdy_hms(paste(date, time)),
            # time-stamp from TrailMaster date
            date = mdy(date),
            # time-stamp from TrailMaster time
            time = hms(time)) %>%
    # remove rows where date is NA
    # I looked at these, and there is no populated information - so they are empty rows
    filter(!is.na(date))
    # nrow(tmdata) should = 3781894 if parsing failures (4354) removed
  
##### correct for dst #####
# in visual qc, 2011-2013 already corrected for dst, 2014 onward not adusted
  
# load list of dst transitions (March - November, 2010-2029)
  dst <- read_csv("data/accessory/TrailMaster_DST_Start_End_2010_2029.csv", 
          # parse start and end dates as datetimes
          col_types = cols(dst_start = col_datetime(format = "%m/%d/%Y %H:%M"), 
                           dst_end = col_datetime(format = "%m/%d/%Y %H:%M")))

  # add year column to tmdata for joining
  tmdata <- mutate(tmdata, year = year(date))
  
  # join dst transitions to tmdata
  tmdata <- left_join(tmdata, dst, by = 'year')
  
  tmdata <- tmdata %>%
                # if datetime is between start and end of DST...
                mutate(datetime2 = 
                         # if in 2011 - 2013, don't adjust
                         if_else(year(datetime) %in% c(2011, 2012, 2013),
                                  datetime,
                        # if not in 2011 - 2013, adjust for dst
                         if_else(datetime > dst_start & 
                                           datetime < dst_end,
                                           # add 1 hr to datetime (PDT)
                                           datetime + minutes(60), 
                                           # if not, keep original time (PST)
                                           datetime))) %>%
              # remove dst columns no longer needed
              select(-c(dst_start:dst_end))
  
# remove dst data from environment
remove(dst)

##### sync with tides #####

  # load tide data (pulled from TboneTides XTide for July 2010 - July 2025, 10 min intervals)
  tides <- read_csv("data/accessory/TrailMaster_XTide_2010_2025_Needs_Overlap_Correction.csv", 
           # format datetime column for consistency with tmdata
           col_types = cols(datetime_round = col_datetime(format = "%m/%d/%Y %H:%M")))

  # there are short overlaps in tide data, so take unique measurements
  tides <- unique(tides)

  # round datetime to nearest 10 minutes
  tmdata <- mutate(tmdata, datetime_round = round_date(datetime, unit = "10 minutes"))

  # join datasets by common 10-min rounded datetime column
  tmdata <- left_join(tmdata, tides, by = 'datetime_round')
  
remove(tides)

##### sync with sunset and sunrise times ####

  # load data
  events <- read_excel("data/accessory/TrailMaster_Sunrise_Sunset_Tides_Time_2010_2026.xlsx", col_types = c("date", "text", "text", "text"))

  # there is some overlap in data pulled, make sure 1 sunrise and 1 sunset per date
  events <- unique(events)
  
  # tidy dataset and prep for merge
  events <- events %>%
    # limit to only sunrise and sunset events
    filter(event %in% c('Sunrise', 'Sunset')) %>%
    # make date column to match with tmdata
    mutate(date = date(event_datetime)) %>%
    # pivot wider so each date is one row with columns for sunset and sunrise datetimes
    pivot_wider(names_from = event, values_from = event_datetime) %>%
    # delete excess columns
    select(date:Sunset) %>%
    # rename columns lower case
    rename_all(tolower)
  
  # join datasets by date
  tmdata <- left_join(tmdata, events, by = 'date')
  
  # remove sunrise/sunset data from environment
  remove(events)
  
##### sync with weather data #####
  
  # load data
  weather <- read_csv("data/accessory/SD_Airport_Weather_2011_2020.csv")
  
  # note with units - 
    # temp - *F
    # wind - speed in mph
    # precip - precipitation in inches
  
  # tidy weather data
  weather <- weather %>%
    select(DATE, HourlyDryBulbTemperature, HourlyWindSpeed, HourlyPrecipitation) %>%
    rename_with(tolower) %>%
    mutate(nearesthr = round_date(date, unit = 'hour'))
  
  # get nearest hr for tmdata
  tmdata$nearesthr <- round_date(tmdata$datetime, unit = 'hour')
  
  # align w tmdata
  tmdata <- left_join(tmdata, weather, by = 'nearesthr')
  
  # remove weather data
  remove(weather)
  
##### tidy again before vis and metadata qc ####
  tmdata <- tmdata %>%
    select(event, datetime2, lot, start, end, year, tidelvl, sunrise, sunset, nearesthr, hourlydrybulbtemperature, hourlywindspeed, hourlyprecipitation)
  
  tmdata2 <- tibble(tmdata)
  
  # write_csv(tmdata2, 'tmdata_roughqc_feb21.csv')
  
##### visual qc - inspect each file for abnormal obs #####
# # warning: large, multi-panel plots ahead
# # target dates to where needed (need nov - dec 2020)
# 
# # look at accumulation over hours (if obs = across time, likely malfunction/vegetation)
# 
# 
#  # create list of lots to loop over
#    lot_list <- unique(tmdata2$lot)
# 
# 
#    # set wd
#    setwd('D:/LP_Files/TrailMaster_Data/TrailMaster/QC_Plots')
# 
# 
#  # loop across lots
#  for(i in 1:length(lot_list)) {
# 
#    # subset relevant data
#    play <- filter(tmdata2, lot == lot_list[i])
#    # get list of start dates for lot
#    start_dat <- unique(play$start)
#    # filter for ones toward the end to not re-do all dates
#    start_date <- start_dat[297:length(start_dat)]
# 
#    # loop across start dates w/in each lot
#    for(j in 1:length(start_date)) {
# 
#      # subset relevant data
#      play2 <- filter(play, start == start_date[j])
# 
#      # make plot
#      ggplot(data = play2) +
#        # bar plot
#        geom_bar(mapping = aes(
#          # plot by hour of observation
#          x = hour(round_date(datetime2, unit = 'hour')),
#          # color is dependent on hours (b/w 9 and 5 = gray, outside = green)
#          fill = hour(round_date(datetime2, unit = 'hour')) < 9 |
#            hour(round_date(datetime2, unit = 'hour')) > 17)) +
#        # set custom colors (ranger gray, ranger green hex codes from uniform site)
#        scale_fill_manual(values = setNames(c('#3E4035', '#807E7F'), c(T,F))) +
#        # x axis label
#        xlab('Hour of Day') +
#        # y axis label
#        ylab ('Raw Observations') +
#        # make title based on lot and start date
#        ggtitle(paste('Lot', lot_list[i], '- Start ', start_date[j])) +
#        # set x limits (no y lim b/c dif obs b/w files)
#        xlim(c(0, 24)) +
#        # set theme, no legend
#        theme_bw() +
#        theme(text = element_text(size = 12),
#                  # make axis text larger to match label
#                  axis.text.y = element_text(size = 12, color = 'black'),
#                  axis.text.x = element_text(size = 12, color = 'black'),
#                  # no legend
#                  legend.position = 'none')
# 
#      ggsave(paste('Lot', lot_list[i], '- Start ', start_date[j], '.png'))
#    }
#  }
# 
# # visual qc occurred after viewing these plots - notes were made in metadata
# 
# # export
#    
#   write_csv(tmdata2, 'tmdata_roughqc_feb21.csv')
# qc steps overview #
  
# in metadata...
  # 1 - generate auto-qc checks for each file in meta data
    # y/n for obs before/after sunrise/sunset
    # > 10% of obs outside park hrs (files will be excluded)
  
  
  # 2 - generate list of dates where...
    # files have no data in csv format
    # files fail qc checks (rejected in csv or hand qc phase)
    # > 10% of obs outside park hrs

# in tmdata...
  # 1 - filter by dates rejected in metadata qc
  # 2 - filter by -1 hr before sunrise and +1 hr after sunset

##### metadata qc - generate list of dates where good data NA ####
  
  # get date ranges where no data collected (or partial data collected)
  nodata <- tmeta %>%
    # get date ranges where there's no CSV, or the file was rejected in QC
    filter(csv_file == 'NO' | reject == 'YES' | hand_qc_fig == 'REJECT') %>%
    # get 3 relevant columns
    select(start, end, lot) %>%
    filter(start < end)
  
  # get all dates w/in ranges w/ no data or didnt' pass qc (list of dates and lots)
  
  # output list
  nodata_datelist <- tibble(value = NA, lot = NA)
  
  for(i in 1:nrow(nodata)) {
    
    datelist <- as_tibble(seq(date(nodata$start[i]), date(nodata$end[i]), by = 'days')) %>%
      mutate(lot = nodata$lot[i])
    
    nodata_datelist <- bind_rows(nodata_datelist, datelist)
  }

  # get rid of duplicates in output
  nodata_datelist <- distinct(nodata_datelist)

##### create and export rough qc'ed data #####
nodata_datelist <- mutate(nodata_datelist, key = paste(lot, value, sep = ','))
key <- nodata_datelist$key
  
tmdata3 <- tmdata2 %>%
    # filter out dates in the no data/ failed QC check date list
    mutate(filter_key = paste(lot,date(datetime2), sep = ',')) %>%
    filter(! filter_key %in% key) %>%
    select(-c(filter_key))

##### tidy abiotic additions #####


# data.table package fwrite() is faster than write_csv 
data.table::fwrite(tmdata3, 'QC_Data/TM_Data_QC_Sept21.csv')
