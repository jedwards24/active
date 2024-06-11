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

rec_list <- readRDS("data_processed/records.RDS")
sess <- readRDS("data_processed/sessions.RDS")

# One power curve ----------
rec <- rec_list[[2]]
bench::system_time(pc <- pwr_bests(rec))
pc
sess
qplot(timestamp, power, data = rec, geom = "line")
mean(rec$power, na.rm = T)

# Multiple power curves ------------
# Need to handle times but for now assume they all the same

bad_power <- c("eb_2024-05-01-132119", "eb_2024-06-02-083050") # remove these
pwr_tbl_list <- map(fs::dir_ls("data_processed/power_bests"), readRDS)
head(pwr_tbl_list)
ids <- map_chr(pwr_tbl_list, ~attr(., "id")) %>%
  unname()
pwr_list <- map(pwr_tbl_list, ~pull(., watts)) %>%
  set_names(ids)

sess_power <- sess %>%
  filter(id %in% ids) %>%
  mutate(power_bests = unname(pwr_list)) %>%
  filter(! id %in% bad_power) %>%
  select(-left_right_balance, -event, -event_type, -sport, -threshold_power, -workout_type)

pwr_combine_bests(pwr_list) %>%
  tibble(duration = seconds_to_period(pwr_time_range()), power = .) %>%
  prinf()
year_tbl <- pwr_duration(sess_power, year) %>%
  mutate(times = seconds_to_period(times)) %>%
  pivot_wider(names_from = year, values_from = power)
prinf(year_tbl)


sess_power %>%
  pwr_duration(year) %>%
  pwr_duration_plot()

sess_power %>%
  arrange(desc(max_power)) %>%
  select(id, max_power)
