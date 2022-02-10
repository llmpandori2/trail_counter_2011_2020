###############################################################################
### Title: Trail Counter Report 2022
### Purpose: Analysis of trail counter data 2011-2020 for 2022 manuscript
### Author: L. Pandori
### Date Created: 12/21/21
### Last Edited: 2/10/22
###############################################################################

##### load packages #####

library(agricolae)  # hsd test letters
library(readxl)     # read excel files
library(janitor)    # clean up datasets
library(lubridate)  # math w dates and times
library(calecopal)  # remarkable color palette
library(viridis)    # another color palette
library(ggdark)     # dark field gg themes
library(ggridges)   # ridgeline plot
library(gt)         # gg tables
library(tidyverse)  # tidy everything

##### presets #####
# set wd so it works in md
setwd('D:/LP_Files/RStudio_Files/trail_counter_2011_2020')

# 'not in' operator
`%notin%` <- Negate(`%in%`)

# custom theme w light field
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
                              axis.text.x = element_text(size = 11),
                              panel.grid = element_blank())

# custom theme w dark field
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
                              axis.text.x = element_text(size = 11),
                              panel.grid = element_blank())

##### tidy data #####
# trail counter 'event' data
tmdata <- read_csv("./data/TM_Data_QC.csv", 
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
    "./data/accessory/TrailMaster_XTide_2010_2025_Needs_Overlap_Correction.csv", 
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
  low_tide <- read_excel("./data/accessory/TrailMaster_Sunrise_Sunset_Tides_Time_2010_2026.xlsx")
  
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
  
##### question 1a - visitation patterns over years #####
  
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
  
##### question 1b - visitation figure w/ entrance over years #####
  
# load entrance station visitation data from IRMA
entrance <- read_csv("./data/accessory/entrance_IRMA_visitation_2011_2020.csv")
  
visit_est <- rbind(visit_est %>%
                        select(-Coverage) %>% rename(Location = Lot),
                        entrance %>%
                        mutate(Location = 'Entrance station') %>%
                        rename(Year = year, Visitors = entrance_visit))

# base plot of visitation over time at entrance, lot 1 and lot 2
visit_time_plot <- ggplot(data = visit_est,
  mapping = aes(x = Year, y = Visitors, group = Location, 
                color = Location)) + 
  geom_point() + 
  geom_line() + 
  xlab('Year') + 
  ylab('Number of visitors') +
  scale_y_continuous(labels = scales::label_number(big.mark = ',')) + 
  scale_x_continuous(breaks = scales::pretty_breaks())

# save dark theme version
ggsave(filename = paste('./figs/visitation_year_dark_.png', sep = ''),
       plot = visit_time_plot + 
              scale_color_manual(values = c(cal_palette('tidepool')[1],
                                            cal_palette('tidepool')[4],
                                            cal_palette('tidepool')[5])) +
              lltheme_dark, 
       height = 5)

# save light theme version
ggsave(filename = paste('./figs/visitation_year_light_.png', sep = ''),
       plot = visit_time_plot + 
         scale_color_manual(values = c(cal_palette('tidepool')[1],
                                       cal_palette('tidepool')[2],
                                       cal_palette('tidepool')[5])) +
         lltheme_light, 
       height = 5)

remove(entrance)

##### question 2 - visitation across days of the week (holidays excluded) #####

# list of OPM holidays
holidays <- read_excel("./data/accessory/OPM_Holidays_2010_2020.xlsx") %>%
  mutate(dte = date(date)) %>%
  select(dte)

# tidy data
weekday <- tmdata %>%
  # exclude holidays 
  filter(dte %notin% c(holidays)) %>%
  # get # of events per day
  group_by(lot, dte, low_tide_time, low_tide_lvl) %>%
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
  summarize(med_n = median(events),
            mean_n = mean(events),
            n = length(dow),
            var = sd(events)
            )

write_csv(weekday_n, './stats/weekday_stat_table.csv')

# function to print anova and tukey hsd
## lot name is 'Lot 1' or 'Lot 2' from data
## save name is cleaner version of lot name (lot1 or lot2)

fn_aov_hsd <- function(lot_name, save_name) {
  # select data 
  data <- filter(weekday, lot == lot_name)
  # do aov and save output
  capture.output(summary(aov(events ~ dow, data = data)),
                 file = paste('./stats/visitation_dow_anova', save_name, '.doc',
                              sep = ''))
  # do tukey hsd and save output 
  hsd <- agricolae::HSD.test(aov(events ~ dow, data = data),
                             trt = 'dow', group = FALSE)
  capture.output(hsd, file = paste('./stats/visitation_dow_hsd', save_name, '.doc',
                                   sep = ''))
}

# run function for lot 1 and lot 2
fn_aov_hsd('Lot 1', 'lot1')
fn_aov_hsd('Lot 2', 'lot2')

# run model and get output for figures
hsd <- rbind(agricolae::HSD.test(aov(events ~ dow, 
              data = filter(weekday, lot == 'Lot 1')), 
              trt = 'dow', group = TRUE)$groups %>%
       mutate(dow = c('Saturday', 'Sunday', 'Friday', 'Monday', 'Thursday', 
                      'Wednesday', 'Tuesday'),
                      lot = 'Lot 1') %>%
       as_tibble() %>%
       select(dow, lot, groups),
       agricolae::HSD.test(aov(events ~ dow, 
              data = filter(weekday, lot == 'Lot 2')), 
              trt = 'dow', group = TRUE)$groups %>%
       mutate(dow = c('Sunday', 'Saturday', 'Monday', 'Friday', 'Thursday', 
                      'Wednesday', 'Tuesday'),
                      lot = 'Lot 2') %>%
       as_tibble() %>%
       select(dow, lot, groups)
             ) %>%
       left_join(., select(weekday_n, dow:med_n), by = c('dow', 'lot'))
  
# plot weekday data with hsd labels
dow_box <- ggplot(data = weekday,
                   mapping = aes(x = dow, y = events, fill = dow, 
                                 color = dow, alpha = 0.2)) +
              geom_jitter(alpha = 0.3, size = 0.7) + 

              coord_cartesian(ylim = c(0,2500)) +
              scale_y_continuous(labels = scales::label_number(big.mark = ',')) +
              scale_color_manual(values = cal_palette(name = 'tidepool', n = 7,
                                                      type = 'continuous')) + 
              scale_fill_manual(values = cal_palette(name = 'tidepool', n = 7, 
                                                      type = 'continuous')) + 
              xlab('Day of week') + 
              ylab('Visitors per day') + 
              facet_wrap(~lot) 

# save light field and dark field versions
ggsave(filename = './figs/vistation_dow_light.png',
       plot = dow_box + geom_boxplot(color = 'black', outlier.shape = NA) + 
        geom_text(data = filter(hsd, lot == 'Lot 1'),
                  mapping = aes(x = dow, y = (med_n + 1200), 
                      label = groups), color = 'black', alpha = 1) +
        geom_text(data = filter(hsd, lot == 'Lot 2'),
                  mapping = aes(x = dow, y = (med_n + 600), 
                       label = groups), color = 'black', alpha = 1) +
         lltheme_light + 
         theme(legend.position = 'none',
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) ,
       width = 7, height = 5)

ggsave(filename = './figs/vistation_dow_dark.png',
       plot = dow_box + geom_boxplot(color = 'white', outlier.shape = NA) + 
         geom_text(data = filter(hsd, lot == 'Lot 1'),
                   mapping = aes(x = dow, y = (med_n + 1200), 
                                 label = groups), color = 'white', alpha = 1) +
         geom_text(data = filter(hsd, lot == 'Lot 2'),
                   mapping = aes(x = dow, y = (med_n + 600), 
                                 label = groups), color = 'white', alpha = 1) +
         lltheme_dark + 
         theme(legend.position = 'none',
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) ,
       width = 7, height = 5)

remove(hsd, weekday_n, fn_aov_hsd)

##### question 3 - Visitation on holidays/non-holidays? #####

# get dates of holidays from OPM
holidates <- read_excel("./data/accessory/OPM_Holidays_2010_2020.xlsx")

# get difference in visitation for holidays and +14 days from holidays to compare
## chosen b/c same dow and similar tide cycle stage
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
  mutate(dif = visit0 - visit14) %>%
  select(lot:holiday, dif, visit0, visit14)

# pr_t(dataset) = paired t-test function (use for holidays, tides, fee free day analyses)
# dataset for holiday data = holidates2
# compare vectors named visit0 and visit14

pr_t <- function(dataset) {
  
pr_t_test <- dataset %>%
  # group by "holiday" and lot
  group_by(holiday, lot) %>%
  nest() %>%
  # perform paired t-test between visit0 and visit14
  mutate(fit =  map(data, ~ t.test(.x$visit0, .x$visit14, paired = TRUE)),
         result = map(fit, broom::glance)) %>%
  unnest(c(data, result)) %>%
  select(lot, holiday, visit0, visit14, dif, statistic:parameter) %>%
  clean_names() %>%
  rename(f_value = statistic, df = parameter) %>%
  # summarize by lot + holiday, and add column for pvalue significance
  ungroup() %>%
  group_by(lot, holiday, p_value, df, f_value) %>%
  summarize(mean_dif = mean(dif),
            sd_dif = sd(dif),
            se_dif = sd(dif)/length(dif),
            mean_visit0 = mean(visit0),
            mean_visit14 = mean(visit14),
            pct_change = ((mean_visit0 - mean_visit14)/mean_visit14)*100,
            pval_sig = p_value <= 0.05) %>%
  distinct()

return(pr_t_test)
}

# run paired t-test function
holiday_test <- pr_t(holidates)

# write csv of results
write_csv(holiday_test, './stats/holiday_t_test.csv')

# make Holiday an ordered factor (by day of year)
day_order <- holidates %>%
  arrange(month(date), day(date)) %>%
  distinct(holiday)

holiday_test$holiday <- fct_relevel(holiday_test$holiday, day_order$holiday)

# plot results as barplot with se and * if significant
ggplot(data = holiday_test) + 
  geom_col(mapping = aes(y = mean_dif, x = fct_rev(holiday), fill = pval_sig)) + 
  geom_errorbar(mapping = aes(x = fct_rev(holiday), ymin = (mean_dif - se_dif), 
                              ymax = (mean_dif + se_dif), width = 0.3),
                color = if_else(holiday_test$pval_sig == 'TRUE', 'gray48', 
                                cal_palette('tidepool')[4])) + 
  scale_fill_manual(values = c(cal_palette('tidepool')[4], cal_palette('tidepool')[1])) + 
  geom_text(mapping = aes(x = 14, y = -100, label = 'Fewer visitors'), 
            color = 'black', hjust = 0.95, size = 3.5) + 
  geom_text(mapping = aes(x = 14, y = 100, label = 'More visitors'), 
            color = 'black', hjust = 0, size = 3.5) + 
  geom_text(data = filter(holiday_test, pval_sig == TRUE),
            mapping = aes(x = fct_rev(holiday), y = (mean_dif + se_dif + 100), 
                          label = '*'), color = 'black') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') + 
  coord_flip(xlim = c(1,14)) +
  scale_y_continuous(breaks = seq(-1000,1000, by = 500), limits = c(-1200,1200)) +
  ylab('Difference in visitation') + 
  xlab('Holiday') +
  facet_wrap(~lot) + 
  lltheme_light + 
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        text = element_text(color = 'black', size = 12),
        axis.text = element_text(color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./figs/visitation_holiday_light.png', width = 7, height = 5)

# dark theme plot
# plot results as barplot with se and * if significant
ggplot(data = holiday_test) + 
  geom_col(mapping = aes(y = mean_dif, x = fct_rev(holiday), fill = pval_sig)) + 
  geom_errorbar(mapping = aes(x = fct_rev(holiday), ymin = (mean_dif - se_dif), 
                              ymax = (mean_dif + se_dif), width = 0.3),
                color = if_else(holiday_test$pval_sig == 'FALSE', 'gray48', 'gray')) + 
  scale_fill_manual(values = c(cal_palette('tidepool')[2], cal_palette('tidepool')[1])) + 
  geom_text(mapping = aes(x = 14, y = -100, label = 'Fewer visitors'), 
            color = 'white', hjust = 0.95, size = 3.5) + 
  geom_text(mapping = aes(x = 14, y = 100, label = 'More visitors'), 
            color = 'white', hjust = 0, size = 3.5) + 
  geom_text(data = filter(holiday_test, pval_sig == TRUE),
            mapping = aes(x = fct_rev(holiday), y = (mean_dif + se_dif + 100), 
                          label = '*'), color = 'white') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'white') + 
  coord_flip(xlim = c(1,14)) +
  scale_y_continuous(breaks = seq(-1000,1000, by = 500), limits = c(-1200,1200)) +
  ylab('Difference in visitation') + 
  xlab('Holiday') +
  facet_wrap(~lot) + 
  lltheme_dark + 
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        text = element_text(color = 'white', size = 12),
        axis.text = element_text(color = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./figs/visitation_holiday_dark.png',  width = 7, height = 5)

remove(holidates)

##### supplement - heat map of visitors by month and year #####

visit_est <- ungroup(tmdata) %>%
  mutate(yr = as_factor(year(dtime)),
         mo = month(dtime, label = TRUE),
         dpm = case_when(mo %in% c('Sep', 'Apr', 'Jun', 'Nov') ~ 30,
                         mo == 'Feb' & yr %in% c('2012', '2016', '2020') ~ 29,
                         mo == 'Feb' & yr %notin% c('2012', '2016', '2020') ~ 28,
                         TRUE ~ 31)) %>%
  group_by(lot, yr, mo) %>%
  summarize(visit_raw = sum(events),
            percent_days = (length(unique(date(dtime)))/unique(dpm))) %>%
  ungroup() %>%
  # correct for missing data (percent days) 
  # and hand calibration (multiplier and div by 2)
  mutate(visit_corrected = visit_raw*(((1 - percent_days)+1)/1),
         visit_corrected = case_when(lot == 1 ~ (visit_corrected*1.53)/2,
                                     lot == 2 ~ (visit_corrected*1.27)/2)) %>%
  # get sums across lots
  group_by(yr, mo) %>%
  summarize(Visitors = sum(visit_corrected))

visit_heatmap <- ggplot(data = visit_est,
         mapping = aes(x = mo, y = yr, fill = Visitors)) + 
  geom_tile() +
  xlab('Month') + 
  ylab('Year') +
  scale_fill_gradientn(colors = c(cal_palette('tidepool')[3], cal_palette('tidepool')[1]))

ggsave('./figs/heatmap_visits_month_year_light.png',
       visit_heatmap + lltheme_light + theme(panel.grid = element_blank()))

ggsave('./figs/heatmap_visits_month_year_dark.png',
       visit_heatmap + lltheme_dark + theme(panel.grid = element_blank()))
  
##### question 4 - visitation by time of day (by dow, holidays excluded) #####

# tidy data
tod <- tmdata %>%
  # exclude holidays 
  filter(dte %notin% c(holidays)) %>%
  # convert date to weekday, calibrate event values, make nicer names for lots
  mutate(dow = weekdays(dte),
         events = case_when(lot == 1 ~ (events*1.53)/2,
                            lot == 2 ~ (events*1.27)/2),
         lot = case_when(lot == 1 ~ 'Lot 1',
                         lot == 2 ~ 'Lot 2'),
         hr = hour(dtime)) %>%
  # make day of week an ordered factor, add col for day type
  mutate(dow = ordered(dow, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>%
  # get avg +/- SE number of visitors per hour each dow
  group_by(lot, hr, dow) %>%
  summarize(Visitors = mean(events),
            visit_se = sd(events)/sqrt(length(events)))

# plot as distributions
tod_ridge <- ggplot(data = tod) + 
  geom_ridgeline(mapping = aes(x = hr, y = fct_rev(dow), height = Visitors/100, 
                               fill = fct_rev(dow)), alpha = 0.6) +
  scale_fill_manual(values = cal_palette(name = 'tidepool', n = 7, type = 'continuous')) + 
  scale_x_continuous(breaks = c(7,9,11,13,15,17,19), limits = c(7,19)) + 
  xlab('Hour of day') + 
  ylab('Visitor activity') +
  facet_wrap(~lot)

# save dark theme version
ggsave(filename = './figs/tod_ridgeline_dark.png',
       plot = tod_ridge + 
              geom_vline(xintercept = 9, linetype = 'dashed', color = 'gray') + 
              geom_vline(xintercept = 17, linetype = 'dashed', color = 'gray')+
              lltheme_dark + 
              theme(legend.position = 'none'),
       width = 7, height = 5)

# save light theme version
ggsave(filename = './figs/tod_ridgeline_light.png',
       plot = tod_ridge + 
         geom_vline(xintercept = 9, linetype = 'dashed', color = 'gray') + 
         geom_vline(xintercept = 17, linetype = 'dashed', color = 'gray')+
         lltheme_light + 
         theme(legend.position = 'none'),
       width = 7, height = 5)

##### question 5 - visitation by tide level ("good" tides < 0.7 ft below MLLW) #####

# first, look at net visitation 
hr_tide <- select(tmdata, lot, events, dtime, tidelvl) %>%
  # calibrate events, nicer lot names
  mutate(events = case_when(lot == 1 ~ (events*1.53)/2,
                            lot == 2 ~ (events*1.27)/2),
         lot = case_when(lot == 1 ~ 'Lot 1',
                         lot == 2 ~ 'Lot 2'))

# test if hrly visitation related to hrly tidelvl
# little variance in visitation explained by variation in tide lvl
summary(lm(events ~ tidelvl, data = filter(hr_tide, lot == 'Lot 1'))) # r2 = 0.03
summary(lm(events ~ tidelvl, data = filter(hr_tide, lot == 'Lot 2'))) # r2 = 0.04

# plot results
tide_hr_visit <- ggplot(data = hr_tide,
                     mapping = aes(x = (tidelvl*3.2808399),
                                   y = events)) +
  geom_point(color = cal_palette('tidepool')[1], alpha = 0.4) + 
  geom_vline(xintercept = 0.7, linetype = 'dashed', color = 'gray') +
  coord_cartesian(ylim = c(0,800)) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  xlab('Tide level (ft relative to MLLW)') + 
  ylab('Visiors') +
  facet_wrap(~lot) 

ggsave(filename = './figs/visit_tideht_hr_light.png',
       plot = tide_hr_visit + lltheme_light + theme(legend.position = 'none'),
       width = 7, height = 5)

ggsave(filename = './figs/visit_tideht_hr_dark.png',
       plot = tide_hr_visit + lltheme_dark + theme(legend.position = 'none'),
       width = 7, height = 5)


# calculate lowest tide lvl 7a-7p and visitation each day
day_tide <- tmdata %>%
  # limit to 7 am - 7 pm
  filter(hour(dtime) >= 7 & hour(dtime) <= 19) %>%
  # get lowest tidelvl value between 7 am and 7 pm and visitation for each day
  group_by(lot, dte) %>%
  summarize(low = min(tidelvl, na.rm = TRUE),
         events = sum(events, na.rm = TRUE)) %>%
  ungroup() %>%
  # convert date to dow, calibrate events, nicer lot names
  mutate(dow = weekdays(dte),
         events = case_when(lot == 1 ~ (events*1.53)/2,
                            lot == 2 ~ (events*1.27)/2),
         lot = case_when(lot == 1 ~ 'Lot 1',
                         lot == 2 ~ 'Lot 2')) %>%
  # make day of week an ordered factor
  mutate(dow = ordered(dow, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>%
  # remove days where events are greater than 10 (reduces 0 clutter @ bottom)
  filter(events > 10)
  
# no relationship b/w min tide lvl and visitation at either lot
tide_visit <- ggplot(data = day_tide,
       mapping = aes(x = (low*3.2808399),
                     y = events)) +
  geom_point(color = cal_palette('tidepool')[1], alpha = 0.4) + 
  geom_vline(xintercept = 0.7, linetype = 'dashed', color = 'gray') +
  coord_cartesian(ylim = c(0,3000)) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  xlab('Lowest tide level 7AM - 7PM (ft below MLLW)') + 
  ylab('Visiors') +
  facet_wrap(~lot) 

ggsave(filename = './figs/visit_tideht_day_light.png',
       plot = tide_visit + lltheme_light + theme(legend.position = 'none'),
       width = 7, height = 5)

ggsave(filename = './figs/visit_tideht_day_dark.png',
       plot = tide_visit + lltheme_dark + theme(legend.position = 'none'),
       width = 7, height = 5)

# linear relationship?
summary(lm(events ~ low, data = filter(day_tide, lot == 'Lot 1'))) # no
summary(lm(events ~ low, data = filter(day_tide, lot == 'Lot 2')))

# do hour-by-hour, tidelvl vs events
ggplot(data = tmdata,
       mapping = aes(x = tidelvl, y = events, color = lot)) + 
  geom_point() + 
  facet_wrap(~lot) + 
  lltheme_light

# since low tides are special days, use matched dow approach, offset by 1 week?
lt <- select(weekday, lot, dte, low_tide_lvl, events, dow) %>%
  # filter for days with tides < 0.7 ft (0.21 m) to get list of days with "good" low tides
  filter(low_tide_lvl < 0.21) %>%
  # rename old events column for events_0 and dte_0
  rename(dte0 = dte, events0 = events, low_tide_lvl0 = low_tide_lvl) %>%
  # make dte = dte0 + 7
  mutate(dte = dte0 + days(7)) %>%
  # join with weekday data to get events at +7 from dte0
  left_join(., select(weekday, lot, dte, low_tide_lvl, events)) %>%
  # rename new with dte7 etc.
  rename(dte7 = dte, events7 = events, low_tide_lvl7 = low_tide_lvl) %>%
  # remove events where the next week low tide (low_tide_lvl7) is < 0.21
  # note: NA means no low tide during park hrs on a given date
  filter(is.na(low_tide_lvl7)) %>%
  # now get days with events for both events0 and events7
  filter(!is.na(events0) & !is.na(events7)) %>%
  # calculate difference in visitation b/w paired events
  mutate(dif = events0 - events7)

# is difference dif from 0?
# pretty normally distributed 
# hist(lt$dif)

# run t-test
t.test(lt$dif)
# yes (t = 3.50, df = 1653, p < 0.001, mean difference is 11-39 people...not relevant)

# get dates of holidays from OPM
holidates <- read_excel("./data/accessory/OPM_Holidays_2010_2020.xlsx")

# remove holidays and test
lt_no_holiday <- lt %>%
  filter(dte0 %notin% date(holidates$date) & dte7 %notin% date(holidates$date))

# there is still a difference b/w low tide days and not
t.test(lt_no_holiday$dif)
# yes (t = 3.44, df = 1522, p < 0.001)
# but it's not that big of a difference (mean = 22.17 visitors, 95% CI 10-35 additional visitors)

# there is no difference among dow
kruskal.test(dif ~ dow, data = filter(lt_no_holiday, lot == 'Lot 1'))
# chi2 = 4.75, df = 6, p = 0.58
# no

##### supplement - fee free day #####

# get dates of holidays from OPM
fee_free <- read_excel("./data/accessory/NPS_Fee_Free_Days_2011_2020.xlsx")

# get difference in visitation for fee free days and + 14 days to compare
fee_free <- rbind(mutate(fee_free, lot = 'Lot 1'),
                  mutate(fee_free, lot = 'Lot 2')) %>%
  mutate(dte = date(date)) %>%
  rename(holiday = event) %>%
  select(-date) %>%
  # get number of visitors on fee free days 
  left_join(., select(weekday, lot, dte, events), by = c('lot', 'dte')) %>%
  # rename joined values
  rename(dte0 = dte, visit0 = events) %>%
  # get + 14 days from fee free date and match w events data
  mutate(dte = dte0 + days(14)) %>%
  left_join(., select(weekday, lot, dte, events), by = c('lot', 'dte')) %>%
  # rename new columns and get difference in events b/w fee free and not, get dow
  rename(dte14 = dte, visit14 = events) %>%
  mutate(dif = visit0 - visit14) %>%
  # only include complete cases
  na.omit()

# get sample sizes
fee_n <- fee_free %>%
  group_by(holiday, lot) %>%
  tally() %>%
  filter(n > 4) %>%
  select(holiday) %>%
  distinct()


# pr_t(dataset) = paired t-test function (use for holidays, tides, fee free day analyses)
# dataset for holiday data = holidates2
# compare vectors named visit0 and visit14
# test if difference in visitation is significantly different from 0 - for holidays with n .=5

fee_test <- pr_t(filter(fee_free, holiday %in% fee_n$holiday))

# put back in data for "holidays"/fee free days without 5 samples
fee_free2 <- full_join(fee_test, 
                       fee_free %>%
                          filter(holiday %notin% fee_n$holiday) %>%
                          group_by(lot, holiday) %>%
                          summarize(mean_dif = mean(dif),
                                    n = length(dif))) %>%
            # add column with significant, not significant or no test
            mutate(`T-test` = case_when(p_value < 0.05 ~ 'Significant',
                                        p_value > 0.05 ~ 'Not significant',
                                        TRUE ~ 'n < 5'))

# make day an ordered factor (by day of year)
day_order <- fee_free %>%
  arrange(month(dte0), day(dte0)) %>%
  distinct(holiday)

fee_free2$holiday <- fct_relevel(fee_free2$holiday, day_order$holiday)

# plot results as barplot with se and * if significant
ggplot(data = fee_free2) + 
  geom_col(mapping = aes(y = mean_dif, x = fct_rev(holiday), fill = fct_rev(`T-test`))) + 
  geom_errorbar(data = filter(fee_free2, `T-test` != 'No test'),
                mapping = aes(x = fct_rev(holiday), ymin = (mean_dif - sd_dif), 
                              ymax = (mean_dif + sd_dif), width = 0.3),
          color = if_else(filter(fee_free2, `T-test` != 'No test')$pval_sig == 'TRUE',
                                'gray48', cal_palette('tidepool')[4])) + 
  scale_fill_manual(values = c(cal_palette('tidepool')[1], cal_palette('tidepool')[4],
                               'gray90')) + 
  geom_text(mapping = aes(x = 14, y = -100, label = 'Fewer visitors'), 
            color = 'black', hjust = 0.95, size = 3.5) + 
  geom_text(mapping = aes(x = 14, y = 100, label = 'More visitors'), 
            color = 'black', hjust = 0, size = 3.5) + 
  geom_text(data = filter(fee_free2, pval_sig == TRUE),
            mapping = aes(x = fct_rev(holiday), y = (mean_dif + sd_dif + 100), 
                          label = '*'), color = 'black') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') +
  coord_flip(xlim = c(1,14), ylim = c(-1300,1300)) +
  scale_y_continuous(breaks = seq(-1000,1000, by = 500)) +
  ylab('Difference in visitation') + 
  xlab('Fee free day') +
  labs(fill = 'T-test result') + 
  facet_wrap(~lot) + 
  lltheme_light + 
  theme(panel.grid = element_blank(),
        text = element_text(color = 'black', size = 12),
        axis.text = element_text(color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./figs/visitation_feefree_light.png', width = 8, height = 4)


# plot results as barplot with se and * if significant
ggplot(data = fee_free2) + 
  geom_col(mapping = aes(y = mean_dif, x = fct_rev(holiday), fill = fct_rev(`T-test`))) + 
  geom_errorbar(data = filter(fee_free2, `T-test` != 'No test'),
                mapping = aes(x = fct_rev(holiday), ymin = (mean_dif - sd_dif), 
                              ymax = (mean_dif + sd_dif), width = 0.3),
                color = if_else(filter(fee_free2, `T-test` != 'No test')$pval_sig == 'TRUE',
                                cal_palette('tidepool')[4], 'gray48')) + 
  scale_fill_manual(values = c(cal_palette('tidepool')[1], cal_palette('tidepool')[2],
                               'gray20')) + 
  geom_text(mapping = aes(x = 14, y = -100, label = 'Fewer visitors'), 
            color = 'white', hjust = 0.95, size = 3.5) + 
  geom_text(mapping = aes(x = 14, y = 100, label = 'More visitors'), 
            color = 'white', hjust = 0, size = 3.5) + 
  geom_text(data = filter(fee_free2, pval_sig == TRUE),
            mapping = aes(x = fct_rev(holiday), y = (mean_dif + sd_dif + 100), 
                          label = '*'), color = 'white') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'white') +
  coord_flip(xlim = c(1,14), ylim = c(-1300,1300)) +
  scale_y_continuous(breaks = seq(-1000,1000, by = 500)) +
  ylab('Difference in visitation') + 
  xlab('Fee free day') +
  labs(fill = 'T-test result') + 
  facet_wrap(~lot) + 
  lltheme_dark + 
  theme(panel.grid = element_blank(),
        text = element_text(color = 'white', size = 12),
        axis.text = element_text(color = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./figs/visitation_feefree_dark.png', width = 8, height = 4)

# write test results
write_csv(fee_test, './stats/fee_free_day_t_test.csv')
