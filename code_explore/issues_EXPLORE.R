# Exploring some issues with raw data

library(FITfileR)
library(tidyverse)
library(edwards)
library(fs)
source("FUNC.R")

ff <- fs::dir_ls("data", regexp = "ELEMNT")
ff_proc <- fs::dir_ls("data_processed/fit_objs", regexp = "eb_")
ftb <- tibble(data_name = ff,
              date = str_extract(ff, "\\d{4}-\\d{2}-\\d{2}-\\d{6}"),
              proc_name = path("data_processed", "fit_objs", eb_save_name(date)),
              todo = !proc_name %in% ff_proc)
count(ftb, date, sort = T)

rec_list <- readRDS("data_processed/records.RDS")
sess <- readRDS("data_processed/sessions.RDS") %>%
  mutate(id = row_number()) %>%
  mutate(start_date = as_date(start_time))

# Multiple data files with same date-time--------------
# 2022-08-22-082903 and 2022-10-09-082903
filter(sess, as_date(start_time) == ymd("2022-08-22")) %>% select(start_time, id)
str_subset(ff2, "2022-08-22")

# 2022-08-22-082903
# the main one extracts ok
fit <- readFitFile("data_redundant/2022-08-22-084242-ELEMNT BOLT 3A07-284-0.fit")
extr <- fit_extract(fit) #error
rec <- records_to_tibble(fit) #ok
getMessagesByType(fit, "session") # no session data
map_route(rec)

fit2 <- readRDS("data_processed/fit_objs/eb_2022-08-22-084242.RDS")
extr <- fit_extract(fit2) #ok
rec2 <- records_to_tibble(fit2) #ok
map_route(rec2)

# 2022-10-09-082903
# Neither of these can be extracted
fit <- readFitFile("data_redundant/2022-10-09-082903-ELEMNT BOLT 3A07-297-0.fit")
fit_extract(fit) #error
rec <- records_to_tibble(fit) #ok
getMessagesByType(fit, "session") # no session data
laps_to_tibble(fit) #error
getMessagesByType(fit, "event") #ok
getMessagesByType(fit, "actvity") # no data
map_route(rec)
fit2 <- readRDS("data_processed/fit_objs/eb_2022-10-09-082903.RDS")
fit_extract(fit2)
rec2 <- records_to_tibble(fit2) #ok
getMessagesByType(fit2, "session") # no session data
laps_to_tibble(fit2) #ok
getMessagesByType(fit2, "event") #ok
getMessagesByType(fit2, "actvity") # no data
map_route(rec2)

ex <- fit_extract(fit2)
saveRDS(ex, "data_processed/extracts/extr_eb_2022-10-09-082903.RDS")
fit_extract_save("data_processed/fit_objs/eb_2022-10-09-082903.RDS")

# file check---------
ff <- fs::dir_ls("data", regexp = "ELEMNT")
ff2 <- fs::dir_ls("data_processed/fit_objs", regexp = "eb_")
ff3 <- fs::dir_ls("data_processed/extracts", regexp = "extr_eb_")
length(ff)
length(ff2)
length(ff3)
str_subset(ff, "2022-10-09")
