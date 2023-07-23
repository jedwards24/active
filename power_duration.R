# Power-duration curves
#
# Process:
# 1. Filter records to those that have power data.
# 2. Calculate power duration for each session.
# 3. Combine (by date) and plot.

library(FITfileR)
library(tidyverse)
library(edwards)
library(slider)
source("FUNC.R")
theme_set(theme_minimal())

rec_list <- readRDS("data_processed/records_2023-07-11.RDS")
sess <- readRDS("data_processed/sessions_2023-07-11.RDS")

# Which records have power --------------
power_ids <- sess %>%
  mutate(id = row_number()) %>%
  filter(!is.na(avg_power)) %>%
  pull(id)
names(sess)
count(sess, is.na(avg_power))
rec_list <- rec_list[power_ids]
sess <- sess %>% filter(!is.na(avg_power))
bad_power <- map_lgl(rec_list, ~max(pull(., power), na.rm = TRUE) > 1100)
rec_list <- rec_list[!bad_power]
sess <- sess %>% filter(!bad_power)

# One power curve ----------
times <- pwr_time_range()

rec <- rec_list[[2]]
bench::system_time(pc <- pwr_duration(rec, times))
pc
sess
qplot(timestamp, power, data = rec, geom = "line")
mean(rec$power, na.rm = T)

# Multiple power curves ------------
pwr_list <- readRDS("data_processed/power_duration.RDS")
if(F){
  pwr_list <- map(rec_list, ~pwr_bests(., times), .progress = T)
  saveRDS(pwr_list, "data_processed/power_duration.RDS")
}

pwr_list[1:3]
sess <- mutate(sess, power_bests = unname(pwr_list)) %>%
  select(-left_right_balance, -event, -event_type, -sport, -threshold_power, -workout_type)

pwr_combine_bests(pwr_list)

sess %>%
  pwr_duration(year) %>%
  pwr_duration_plot()
