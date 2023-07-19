# Power-duration curves

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

times <- c(1:4, seq(5, 30, by = 5),
           40, 50, 60,
           seq(120, 600, by = 60),
           seq(900, 1800, by = 300),
           seq(2100, 7200, by = 600))


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
saveRDS(pwr_list, "data_processed/power_duration.RDS")
pwr_list[1:3]
power_ids
rec_list[[1]] %>% pull(power)
object_size_all()
unname(pwr_list)
sess <- mutate(sess, power_bests = unname(pwr_list)) %>%
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

combine_power_bests(pwr_list, times)
power_year(sess, 2023)

pwr_tbl <- sess %>%
#  filter(!between(start_time, ymd("2021-01-01"), ymd("2021-03-05"))) %>%
  group_by(year = factor(year(start_time))) %>%
  summarise(power = list(combine_power_bests(power_bests, times = times)))

# plot by year
pwr_tbl %>%
  mutate(times = list(times)) %>%
  unchop(c(power, times)) %>%
  ggplot(aes(times, power, group = year, colour = year, linetype = year)) +
  geom_line() +
  scale_x_continuous(labels = c("5s", "30s", "1m", "5m", "10m", "1hr"),
                     breaks = c(5, 30, 60, 300, 600, 3600),
                     limits = c(3, NA),
                     trans = "log")

# bad data --------
# replace large values with 0
# Add col to sess to identify very bad files (which I'll ignore)
# Some are just small bits of bad data, others are blocks and I stop recording
# ok 23, 108
# don't use 139, 207
# maybe salvage something 209, 233
# 213 has high of 1200 with errors in concentrated block. Probably exclude.
maxes <- map_dbl(unname(pwr_list), max)
which(maxes > 1000)
maxes[213]
vcount(maxes) %>% arrange(desc(value))
big <- which(maxes == 65535)
vcount(maxes) %>% arrange(desc(value))
sess %>% slice(big)
rec <- rec_list[[213]]
ggplot(rec, aes(timestamp, power)) +
         geom_line()
count(rec, power, sort = T)
count(rec, power)  %>% arrange(desc(power))
rec %>%
  filter(power > 1000)
which(rec$power > 2000)
slice(rec, 7400:7470) %>% prinf
rec$power[7380:7470]
rec %>%
  mutate(power = ifelse(power > 20000, 0, power)) %>%
  ggplot(aes(timestamp, power)) +
  geom_line()
map_route(rec)
mean(ifelse(rec$power > 60000, 0, rec$power), na.rm = T)

mean(rec$power, na.rm = T)

which(maxes > 779)
pwr_list[[152]]
times
rec <- rec_list[[152]]
