# Development methods for identifying and fixing bad power data

library(FITfileR)
library(tidyverse)
library(edwards)
library(slider)
library(leaflet)
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

# Power summary--------------
# Helper
get_power <- function(x) {
  if ("power" %in% colnames(x)){
    return(pull(x, power))
  }
  NA
}

dtp <- enframe(rec_list, value = "data") %>%
  mutate(name = str_remove(name, "data_processed/eb_")) %>%
  mutate(id = row_number()) %>%
  mutate(power = map(data, get_power))

dt2 <- dtp %>%
  filter(!is.na(power)) %>%
  mutate(n_obs = map_int(power, length)) %>%
  mutate(max = map_int(power, max, na.rm = T)) %>%
  mutate(nas = map_int(power, ~sum(is.na(.)))) %>%
  mutate(n_hi = map_int(power, ~sum(. == 65535, na.rm = T))) %>%
  mutate(max_not_hi = map_int(power, ~max(na_if(., 65535), na.rm = T)))
dt2
bad <- dt2 %>%
  filter(max > 1000) %>%
  mutate(power_bests = map(data, pwr_bests))
bad

# Explore bad data --------
# ids: 32, 132, 175, 250, 252, 261, 296
# 1. Roughlee & Read. Short spike. 800W may be error but it is brief. Nothing systematic
# 2. Spike happen right at start. Ok o/w
# 3. York to home 2021. Turned off early after errors so ignore all data.
# 4. Kingsdale 2022. Turned off mid ride after errors so ignore all data.
# 5. Hebden 2022. Turned off after errors but late. Lost 10% of ride. Keep.
# 6. Clitheroe 2022. Clump of ~40 high values in 8400:8600. Exclude anything > 600W.
# 7. Ingleton 2023. Last 10% bad. Ok before that? Just use up to index 7200.
#
# So: replace 65535 with NA. Mark 3,4,6 as bad & ignore. Edit 7 or add to bad??

rec <- bad$data[[6]]
sort(rec$power, decreasing = T)
ggplot(rec, aes(timestamp, power)) +
  geom_line()
ggplot(rec, aes(timestamp, na_if(power, 65535))) +
  geom_line()

count(rec, power, sort = T)
count(rec, power)  %>% arrange(desc(power))
rec %>%
  filter(power <600) %>%
  {mean(.$power, na.rm = T)}
rec %>%
  mutate(power = ifelse(power > 600, NA, power)) %>%
  ggplot(aes(timestamp, power)) +
  geom_line()
map_route(rec)

plot(rec$power[6000:7200])
rec$power
rec$power[8490:8600]
