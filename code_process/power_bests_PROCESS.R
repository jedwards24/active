library(FITfileR)
library(tidyverse)
library(edwards)
library(slider)
source("FUNC.R")
theme_set(theme_minimal())

rec_list <- readRDS("data_processed/records.RDS")
sess <- readRDS("data_processed/sessions.RDS")
stopifnot(all(names(rec_list) == sess$id))
glimpse(sess)

# Filter records with good power data
dt_combine <- mutate(sess, record = rec_list) %>%
  select(-left_right_balance, -event, -event_type, -sport, -threshold_power, -workout_type) %>%
  filter(!is.na(avg_power)) %>%
  mutate(power_peak = map_dbl(record, ~max(pull(., power), na.rm = TRUE))) %>%
  filter(power_peak < 1100)

count(dt_combine, power_peak > 1000)
max(dt_combine$max_power)

# Get power bests for each record and save
already_done <- fs::dir_ls("data_processed/power_bests") %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  str_remove("^pwr_bests_")

dt_todo <- filter(dt_combine, !id %in% already_done)

walk2(dt_todo$record, dt_todo$id, pwr_best_save, .progress = TRUE)

# Bad power files-----------
dtb <- mutate(sess, record = rec_list) %>%
  select(-left_right_balance, -event, -event_type, -sport, -threshold_power, -workout_type) %>%
  filter(!is.na(avg_power)) %>%
  mutate(power_peak = map_dbl(record, ~max(pull(., power), na.rm = TRUE))) %>%
  filter(power_peak >= 1100)

dtb
