# Joining log data on to summary data
# Is tricky in cases where there are multiple rides in same day
# These do not match due to either combining data into one ride or
# if a ride was on Coros, not Wahoo
# The below method is ok though.
library(tidyverse)

dts <- readRDS("data_processed/sessions.RDS") %>%
  filter(!is.na(start_time)) %>%
  group_by(date = as_date(start_time)) %>%
  mutate(act_no = row_number(),
         n_acts = n()) %>%
  select(-event, -event_type, -`NA`, -workout_type) %>%
  ungroup()

dt <- readRDS("../activity_data/data_processed/log_full.RDS") %>%
  filter(!week_data) %>%
  select(!c("strides", "sam", "workout_type", "time_hard", "time_mod", "density",
            "hilly", "total_work", "recovery", "week_data")) %>%
  filter(type %in% c("B", "T")) %>%
  filter(between(date, min(dts$date),  max(dts$date))) %>%
  group_by(date) %>%
  mutate(act_no = row_number(),
         n_acts = n()) %>%
  ungroup()

dtc <- dts %>%
  left_join(dt, by = c("date", "act_no")) %>%
  group_by(date) %>%
  fill(everything()) %>%
  ungroup()

saveRDS(dtc, "data_processed/sess_log.RDS")
names(dtc)
