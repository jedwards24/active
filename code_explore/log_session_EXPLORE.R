library(tidyverse)
library(edwards)
dt <- readRDS("data_processed/sess_log.RDS")
dt
select(dt, contains("power"))

ggplot(dt, aes(avg_power, ave_power)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
count_nas(dt)
dt %>%
  filter(is.na(avg_speed)) %>% #turbo session
  glimpse

filter(dt, ave_power > 2 * avg_power) %>% glimpse()
filter(dt, ave_power == 0, avg_power> 50) %>%
  select(date, contains("power"))

filter(dt, date == ymd("2021-10-06")) %>% glimpse
