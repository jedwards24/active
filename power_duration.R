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

# One power curve ----------
peak_power <- function(dat, seconds) {
  slide_dbl(dat$power, ~sum(., na.rm = TRUE) / seconds, .before = seconds - 1) %>%
    max()
}

times <- c(1:4, seq(5, 30, by = 5),
           40, 50, 60,
           seq(120, 600, by = 60),
           seq(900, 1800, by = 300),
           seq(2100, 7200, by = 600))

power_duration <- function(record, times) {
  map_dbl(times, ~peak_power(record, .))
}

rec <- rec_list[[2]]
nrow(rec) / 60
peak_power(rec, 6000)
bench::system_time(pc <- power_duration(rec, times))
pc
?slide_dbl
mean(rec$power, na.rm = T)
sess
qplot(timestamp, power, data = rec, geom = "line")
mean(rec$power, na.rm = T)


# Multiple power curves ------------
pwr_list <- map(rec_list, ~power_duration(., times), .progress = T)
pwr_list[1:3]
power_ids
rec_list[[1]] %>% pull(power)
object_size_all()
unname(pwr_list)
sess <- mutate(sess, power_bests = unname(pwr_list))
sess <- sess %>%
  select(-left_right_balance, -event, -event_type, -sport, -threshold_power, -workout_type)
glimpse(sess)
var_summary(sess) %>% prinf
count(sess, sub_sport)

sess$power_bests[1:5] %>%
  list_c() %>%
  matrix(nrow = length(times)) %>%
  apply(1, max)
sess$power_bests[1:5] %>% pmax()

power_year <- function(dat, year) {
  dat %>%
    filter(year(start_time) == year) %>%
    pull(power_bests) %>%
    best_power()
}

# input is power list
best_power <- function(x) {
  list_c(x) %>%
    matrix(nrow = length(times)) %>%
    apply(1, max)
}
best_power(pwr_list)
power_year(sess, 2023)

# bad data --------
# replace large values with 0
# Add col to sess to identify very bad files (which I'll ignore)
# Some are just small bits of bad data, others are blocks and I stop recording
# ok 23, 108
# don't use 139, 207
# maybe salvage something 209, 233
maxes <- map_dbl(unname(pwr_list), max)
big <- which(maxes == 65535)
vcount(maxes) %>% arrange(desc(value))
sess %>% slice(big)
rec <- rec_list[[139]]
ggplot(rec, aes(timestamp, power)) +
         geom_line()
count(rec, power, sort = T)
count(rec, power)  %>% arrange(desc(power))
rec %>%
  filter(power > 2000)
which(rec$power > 2000)
slice(rec, 7400:7470) %>% prinf
rec$power[7380:7470]
rec %>%
  mutate(power = ifelse(power > 60000, 0, power)) %>%
  ggplot(aes(timestamp, power)) +
  geom_line()
map_route(rec)
mean(ifelse(rec$power > 60000, 0, rec$power), na.rm = T)

mean(rec$power, na.rm = T)

