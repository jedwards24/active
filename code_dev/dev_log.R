# Joining log data on to summary data
# Is tricky in cases where there are multiple rides in same day
# These do not match due to either combining data into one ride or
# if a ride was on Coros, not Wahoo
# The below method is ok though.
#
# Add processing script and save a log_session RDS to data_processed
library(tidyverse)
library(edwards)

dts <- readRDS("data_processed/sessions.RDS")
dts
fs::dir_ls("../activity_data/data_processed")
dt <- readRDS("../activity_data/data_processed/log_full.RDS") %>%
  filter(!week_data) %>%
  select(-week_data)

dts2 <- dts %>%
  filter(!is.na(start_time)) %>%
  group_by(date = as_date(start_time)) %>%
  mutate(act_no = row_number(),
         n_acts = n()) %>%
  select(date, act_no, start_time, n_acts, total_elapsed_time) %>%
  ungroup()

dt2 <- dt %>%
  filter(type %in% c("B", "T")) %>%
  filter(between(date, min(dts2$date),  max(dts2$date))) %>%
  group_by(date) %>%
  mutate(act_no = row_number(),
         n_acts = n()) %>%
  ungroup()

# compare
count(dt2, n_acts)
dt2 %>%
  filter(n_acts > 1) %>%
  pull(date) %>% unique
dts2 %>%
  filter(n_acts > 1) %>%
  pull(date) %>% unique
dts2 %>%
  filter(n_acts > 1)
dt2 %>%
  filter(n_acts > 1)

# join
dtc <- dts2 %>%
  left_join(dt2, by = c("date", "act_no"))
dtc %>%
  filter(n_acts.x > 1 | n_acts.y > 1) %>%
  fill(everything())
dtc2 <- dtc %>%
  group_by(date) %>%
  fill(everything()) %>%
  ungroup()
dtc2 %>%
  filter(n_acts.x > 1 | n_acts.y > 1) %>%
  glimpse()

glimpse(dtc2)
count_nas(dtc2)
filter(dtc2, is.na(type))
count(dtc2, type)
count(dts, sport, sub_sport)

dtc2 %>%
  select(!c("strides", "sam", "workout_type", "time_hard", "time_mod", "density", "hilly", "total_work", "recovery")) %>%
  saveRDS("data_processed/sess_log.RDS")
