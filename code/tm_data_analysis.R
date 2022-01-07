###############################################################################
### Title: Trail Counter Report 2022
### Purpose: Analysis of trail counter data 2011-2020 for 2022 manuscript
### Author: L. Pandori
### Date Created: 12/21/21
### Last Edited: 1/7/22
###############################################################################

##### load packages #####

library(agricolae)  # hsd test letters
library(readxl)     # read excel files
library(janitor)    # clean up datasets
library(lubridate)  # math w dates and times
library(calecopal)    # color palette
library(ggdark)     # dark field gg themes
library(gt)         # gg tables
library(tidyverse)  # tidy everything

##### presets #####

lltheme_light <- theme_bw() + theme(text = element_text(size = 12),
                              # add more space between panels
                              panel.spacing = unit(1, 'lines'),
                              # no background to wrap panels
                              strip.background = element_blank(),
                              strip.text = element_text(size = 12, hjust = 0),
                              # panel labels outside x axis labels
                              strip.placement = 'outside',
                              # adjust x axis labels
                              axis.text.y = element_text(size = 11),
                              axis.text.x = element_text(size = 11, angle = 45,
                                                         hjust = 1))

lltheme_dark <- dark_theme_bw() + theme(text = element_text(size = 12),
                              # add more space between panels
                              panel.spacing = unit(1, 'lines'),
                              # no background to wrap panels
                              strip.background = element_blank(),
                              strip.text = element_text(size = 12, hjust = 0),
                              # panel labels outside x axis labels
                              strip.placement = 'outside',
                              # adjust x axis labels
                              axis.text.y = element_text(size = 11),
                              axis.text.x = element_text(size = 11, angle = 45,
                                                         hjust = 1))


##### load + tidy data #####

# trail counter 'event' data
tmdata <- read_csv("data/TM_Data_QC.csv", 
         col_types = cols(hourlydrybulbtemperature = col_double(),
                          hourlywindspeed = col_double(), 
                          hourlyprecipitation = col_double()))

tmdata <- tmdata %>%
  # get data 2011-2020, b/w sunrise and sunset
  filter(year > 2010 & year < 2021) %>%
  filter(datetime2 > sunrise & datetime2 < sunset) %>%
  # remove some extra columns + tide data (will redo in next step)
  select(-c(event, start, end, tidelvl, sunrise, sunset)) %>%
  # rename w nice names
  rename(temp = hourlydrybulbtemperature, 
         wind = hourlywindspeed, precip = hourlyprecipitation,
         dtime = nearesthr) %>%
  ungroup() %>%
  group_by(lot, dtime) %>%
  summarize(temp = mean(temp, na.rm = TRUE),
         wind = mean(wind, na.rm = TRUE),
         precip = mean(precip, na.rm = TRUE),
         events = length(dtime)) %>%
  # add date column to join tide data
  ungroup() %>%
  mutate(dte = date(dtime))

# tide level data + lowest tide of day

  # continuous tide data
  tide_cont <- read_csv(
    "data/accessory/TrailMaster_XTide_2010_2025_Needs_Overlap_Correction.csv", 
    col_types = cols(datetime_round = col_datetime(format = "%m/%d/%Y %H:%M")))
  
  # get data only on the hour
  tide_cont <- tide_cont %>%
    # overlap correction
    distinct() %>%
    # all data in PST
    select(-timezone) %>%
    # get tide lvl on the hr + w/in event range
    rename(dtime = datetime_round) %>%
    filter(as.numeric(minute(dtime)) == 0,
           year(dtime) > 2010 & year(dtime) < 2021) %>%
    mutate(dte = date(dtime))
  
  # lowest tide of the day
  low_tide <- read_excel("data/accessory/TrailMaster_Sunrise_Sunset_Tides_Time_2010_2026.xlsx")
  
  # get only low tide data
  low_tide2 <- low_tide %>%
    # get 2011-2020 and only low tide events
    rename(dtime = event_datetime) %>%
    filter(year(dtime) > 2010 & year(dtime) < 2021) %>%
    filter(event %in% c('Sunrise', 'Sunset') | tide_note == 'Low Tide') %>%
    mutate(tide_note = if_else(is.na(tide_note), event, tide_note),
           dte = date(dtime)) %>%
    select(-event_tz) %>%
    distinct()
  
  low_tide3 <- low_tide2 %>%
    filter(tide_note == 'Low Tide') %>%
    mutate(event = as.numeric(word(event, 1))) %>%
    rename(low_tide_lvl = event, low_tide_time = dtime) %>%
    select(-tide_note)
  
  sunrise_sunset <- low_tide2 %>%
    filter(tide_note %in% c('Sunrise', 'Sunset')) %>%
    select(-event) %>%
    pivot_wider(names_from = tide_note, values_from = dtime) %>%
    rename(sunrise_dtime = Sunrise, sunset_dtime = Sunset)
  
  low_tide4 <- left_join(sunrise_sunset, low_tide3, by = 'dte') %>%
    # filter for low tides < 0.7 ft (0.21336m) between sunrise and sunset
    filter(low_tide_lvl < 0.21336) %>%
    filter(low_tide_time > sunrise_dtime & low_tide_time < sunset_dtime) %>%
    select(-c(sunrise_dtime, sunset_dtime)) 
    # results in no dates with 2 tides
  
# join with trail counter data 
  tmdata <- left_join(tmdata, low_tide4, by = 'dte') %>%
    left_join(., select(tide_cont, -dte), by = 'dtime')
  
# remove extra dfs 
  remove(low_tide, low_tide2, low_tide3, low_tide4, sunrise_sunset, tide_cont)
  
##### question 1a - has visitation increased over time #####
  
# Lot 1 remains similar over time
# Lot 2 increases over time
# Perhaps b/c Lot 1 is full a lot of the time and Lot 2 isn't
  
visit_est <- ungroup(tmdata) %>%
    mutate(yr = year(dtime)) %>%
    group_by(lot, yr) %>%
    summarize(visit_raw = sum(events),
              percent_days = (length(unique(date(dtime)))/365)) %>%
    ungroup() %>%
    # correct for missing data (percent days) 
    # and hand calibration (multiplier and div by 2)
    mutate(visit_corrected = visit_raw*(((1 - percent_days)+1)/1),
           visit_corrected = case_when(lot == 1 ~ (visit_corrected*1.53)/2,
                                       lot == 2 ~ (visit_corrected*1.27)/2),
           lot = case_when(lot == 1 ~ 'Lot 1',
                           lot == 2 ~ 'Lot 2')) %>%
    select(-visit_raw) %>%
    # rename w nicer table names
    rename(Lot = lot, Year = yr, Coverage = percent_days, Visitors = visit_corrected)
  
    visit_est %>%
    group_by(Lot) %>%
    gt() %>%
    fmt_percent(columns = Coverage, decimals = 0) %>%
    fmt_number(columns = Visitors, decimals = 0) %>%
    gtsave('./figs/visit_table.png')
  
##### question 2 - visitation comparison w/ entrance #####
  
# load entrance station visitation data from IRMA
entrance <- read_csv("data/accessory/entrance_IRMA_visitation_2011_2020.csv")
  
visit_est <- rbind(visit_est %>%
                        select(-Coverage) %>% rename(Location = Lot),
                        entrance %>%
                        mutate(Location = 'Entrance station') %>%
                        rename(Year = year, Visitors = entrance_visit))

visit_time_plot <- ggplot(data = visit_est,
  mapping = aes(x = Year, y = Visitors, group = Location, 
                color = Location)) + 
  geom_point() + 
  geom_line() + 
  # if contrast is needed, switch dudleya 1 with creek 5
  scale_color_manual(values = c(cal_palette('dudleya')[2], cal_palette('dudleya')[1], cal_palette('dudleya')[4])) + 
  xlab('Year') + 
  ylab('Number of visitors') +
  scale_y_continuous(labels = scales::label_number(big.mark = ',')) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  facet_wrap(~Location) 


ggsave(filename = './figs/visitation_time_dark.png',
       plot = visit_time_plot + lltheme_dark + theme(legend.position = 'none'),
       width = 7, height = 3)

ggsave(filename = './figs/visitation_time_light.png',
       plot = visit_time_plot + lltheme_light + theme(legend.position = 'none'),
       width = 7, height = 3)

### is there a correlation b/w entrance visitation and lot1/2
visit_est <- pivot_wider(visit_est, names_from = Location, values_from = Visitors)%>%
  clean_names()

# entrance vs lot 1 (NS relationship, p = 0.30, r2 = 0.13)
summary(lm(lot_1 ~ entrance_station, data = visit_est))

# entrance vs lot 2(NS relationship, p = 0.90, r2 = -0.12)
summary(lm(lot_2 ~ entrance_station, data = visit_est))

remove(visit_time_plot, visit_est, entrance)

##### question 3 - visitation across days of the week #####

# tidy data
weekday <- tmdata %>%
  # get # of events per day
  group_by(lot, dte) %>%
  summarize(events = sum(events)) %>%
  ungroup() %>%
  # convert date to weekday, calibrate event values, make nicer names for lots
  mutate(dow = weekdays(dte),
         events = case_when(lot == 1 ~ (events*1.53)/2,
                            lot == 2 ~ (events*1.27)/2),
         lot = case_when(lot == 1 ~ 'Lot 1',
                         lot == 2 ~ 'Lot 2')) %>%
  # make day of week an ordered factor
  mutate(dow = ordered(dow, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')))

# are sample sizes + variances relatively equal? - yes
weekday_n <- weekday %>%
  group_by(dow, lot) %>%
  summarize(max_n = quantile(events, 0.95),
            n = length(dow),
            var = sd(events)
            )

weekday_n

# function to print anova and tukey hsd
## lot name is 'Lot 1' or 'Lot 2' from data
## save name is cleaner version of lot name (lot1 or lot2)

fn_aov_hsd <- function(lot_name, save_name) {
  # select data 
  data <- filter(weekday, lot == lot_name)
  # do aov and save output
  capture.output(summary(aov(events ~ dow, data = data)),
                 file = paste('./figs/visitation_dow_anova', save_name, '.doc',
                              sep = ''))
  # do tukey hsd and save output 
  hsd <- agricolae::HSD.test(aov(events ~ dow, data = data),
                             trt = 'dow', group = FALSE)
  capture.output(hsd, file = paste('./figs/visitation_dow_hsd', save_name, '.doc',
                                   sep = ''))
}

# run function for lot 1 and lot 2
fn_aov_hsd('Lot 1', 'lot1')
fn_aov_hsd('Lot 2', 'lot2')

# run model and get output for figures
hsd <- rbind(agricolae::HSD.test(aov(events ~ dow, 
                                     data = filter(weekday, lot == 'Lot 1')), 
                                 trt = 'dow', group = TRUE)$groups %>%
               mutate(dow = c('Saturday', 'Sunday', 'Friday', 'Monday', 'Thursday', 'Wednesday', 'Tuesday'),
                      lot = 'Lot 1') %>%
               as_tibble() %>%
               select(dow, lot, groups),
             agricolae::HSD.test(aov(events ~ dow, 
                                     data = filter(weekday, lot == 'Lot 2')), 
                                 trt = 'dow', group = TRUE)$groups %>%
               mutate(dow = c('Sunday', 'Saturday', 'Monday', 'Friday', 'Thursday', 'Wednesday', 'Tuesday'),
                      lot = 'Lot 2') %>%
               as_tibble() %>%
               select(dow, lot, groups)
             ) %>%
  left_join(., select(weekday_n, dow:max_n), by = c('dow', 'lot'))
  
# plot weekday data with hsd labels
dow_box <- ggplot(data = weekday,
                   mapping = aes(x = dow, y = events, fill = dow, 
                                 color = dow, alpha = 0.2)) +
              geom_jitter(alpha = 0.5, size = 0.7) + 

              coord_cartesian(ylim = c(0,2500)) +
              scale_y_continuous(labels = scales::label_number(big.mark = ',')) +
              scale_color_manual(values = cal_palette(name = 'dudleya', n = 7,
                                                      type = 'continuous')) + 
              scale_fill_manual(values = cal_palette(name = 'dudleya', n = 7, 
                                                      type = 'continuous')) + 
              xlab('Day of week') + 
              ylab('Visitors per day') + 
              facet_wrap(~lot) 

# save light field and dark field versions
ggsave(filename = './figs/vistation_dow_light.png',
       plot = dow_box + geom_boxplot(color = 'black', outlier.shape = NA) + 
            geom_text(data = hsd, mapping = aes(x = dow, y = -40, 
                      label = groups), color = 'black', alpha = 1) +
         lltheme_light + 
         theme(legend.position = 'none',
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank()) ,
       width = 7, height = 5)

ggsave(filename = './figs/vistation_dow_dark.png',
       plot = dow_box + geom_boxplot(color = 'white', outlier.shape = NA) + 
         geom_text(data = hsd, mapping = aes(x = dow, y = -40, 
                                             label = groups), color = 'white', alpha = 1) +
         lltheme_dark + 
         theme(legend.position = 'none',
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank()) ,
       width = 7, height = 5)

remove(dow_box, hsd, weekday_n, fn_aov_hsd)

##### Visitation by holiday/non holiday? #####

# get dates of holidays from OPM
holidates <- read_excel("data/accessory/OPM_Holidays_2010_2020.xlsx")

# get difference in visitation for holidays and +14 days from holidays to compare
## chosen b/c same dow and tide cycle stage
holidates <- rbind(mutate(holidates, lot = 'Lot 1'),
                    mutate(holidates, lot = 'Lot 2')) %>%
  mutate(dte = date(date), holiday = event) %>%
  # get number of visitors for holidays
  left_join(., select(weekday, -dow), by = c('lot', 'dte')) %>%
  rename(dte0 = dte, visit0 = events) %>%
  mutate(dte = date(date) + days(14)) %>%
  # get number of visitors for holidays + 14 days
  left_join(., select(weekday, -dow), by = c('lot', 'dte')) %>%
  rename(dte14 = dte, visit14 = events) %>%
  select(lot, date, holiday, dte0, visit0, dte14, visit14) %>%
  # get complete cases (holidays w data for both the day of and +14 days)
  na.omit() %>%
  # get difference between holiday and +14 days
  mutate(dif = visit0 - visit14)

holidates2 <- select(holidates, lot:holiday, dif, visit0, visit14)

holiday_test <- holidates2 %>%
  group_by(holiday, lot) %>%
  nest() %>%
  mutate(fit =  map(data, ~ t.test(.x$visit0, .x$visit14, paired = TRUE)),
         result = map(fit, broom::glance)) %>%
  unnest(c(data, result)) %>%
  select(lot, holiday, visit0, visit14, statistic:parameter) %>%
  clean_names() %>%
  rename(f_value = statistic, df = parameter) %>%
  # get dif
  mutate(dif = visit0-visit14) %>%
  # summarize by lot + holiday
  ungroup() %>%
  group_by(lot, holiday, p_value, df, f_value) %>%
  summarize(mean_dif = mean(dif), 
            sd_dif = sd(dif),
            pval_sig = p_value <= 0.05)

# make Holiday an ordered factor (by day of year)
day_order <- holidates %>%
  arrange(month(date), day(date)) %>%
  distinct(holiday)

holiday_test$holiday <- fct_relevel(holiday_test$holiday, day_order$holiday)

# plot results
ggplot() +
  geom_segment(data = holiday_test, 
               mapping = aes(x = fct_rev(holiday), xend = fct_rev(holiday),
                             y = 0, yend = mean_dif),
               color = if_else(holiday_test$p_value <= 0.05, 
                               cal_palette('dudleya')[1], 'gray80')) +
  geom_point(data = holiday_test, 
             mapping = aes(x = fct_rev(holiday), y = mean_dif),
                           color = if_else(holiday_test$p_value <= 0.05, 
                                           cal_palette('dudleya')[1], 'gray80'), 
                      size = 3, alpha = 0.6) +
  geom_text(data = holiday_test,
            mapping = aes(x = 14, y = -100, label = '← Fewer visitors'), 
            color = 'black', hjust = 1) + 
  geom_text(data = holiday_test,
            mapping = aes(x = 14, y = 100, label = 'More visitors →'), 
            color = 'black', hjust = 0) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') + 
  coord_flip(xlim = c(0,14), ylim = c(-1000, 1000)) +
  scale_y_continuous(breaks = seq(-1000,1000, by = 500)) +
  ylab('Difference in Visitation') + 
  xlab('Holiday') + 
  facet_wrap(~lot) + 
  lltheme_light + 
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        text = element_text(color = 'black', size = 12),
        axis.text = element_text(color = 'black'))

ggsave('./figs/visitation_holiday_lollipop_light.png',
       width = 8)

##### Visitation by time of day (separated by Tues-Th, Mon/Fri and Sat/Sun) #####


##### Is visitation higher on days with "good" low tides (< 0.7 ft below MLLW)? #####
# use continuous
# max low tide vs daily visitation
# do people come around low tide time on low tide days? or are we indescriminately afternoon people in soCal?






  


  
    
  






