# remotes::install_github("grimbough/fitFileR")
library(FITfileR)
library(tidyverse)
library(edwards)
library(slider)
source("FUNC.R")
theme_set(theme_minimal())

# Read one of my FIT files
file <- "data/2020-05-20-11-47-35.fit"
file2 <- "data/2022-05-29-073545-ELEMNT BOLT 3A07-256-0.fit"

system.time(dt <- readFitFile(file))
system.time(eb <- readFitFile(file2))

# explore object--------
eb
is(dt)
slotNames(dt)
dt@header
dt@messages
dt@developer_msg_defs
str(dt)
listMessageTypes(dt)
laps(dt)
events(dt)
ss <- getMessagesByType(dt, "session")
glimpse(ss)
getMessagesByType(dt, "file_id")
getMessagesByType(dt, "device_info")
getMessagesByType(dt, "activity")

eb_types <- listMessageTypes(eb)
eb_list <- map(eb_types[-c(9, 13)], ~getMessagesByType(eb, .))
names(eb_list) <- eb_types[-c(9, 13)]
map(eb_list, ~dim(bind_rows(.))) %>%
  enframe() %>%
  hoist(value, nrow = 1, ncol = 2)

nm_sess <- names(eb_list$session)
nm_lap <- names(eb_list$lap)
compare_sets(nm_lap, nm_sess)
setdiff(nm_sess, nm_lap)

walk(eb_list[1:4], print)
rec <- records_to_tibble(eb)
events(eb)
getMessagesByType(eb, "device_info") #error
getMessagesByType(eb, "workout")
getMessagesByType(eb, "activity")
getMessagesByType(eb, "segment_lap")
getMessagesByType(eb, "mfg_range_min") #error

laps(eb)

x <- eb@messages[[4]]
slotNames(x)
x@header
x@definition
x@fields

# extract records --------
rec_list <- records(dt)
map_int(rec_list, nrow)
map(rec_list, ~slice_head(.)$timestamp)
rec <- rec_list %>%
  bind_rows() %>%
  arrange(timestamp)
rec <- records_to_tibble(dt)
#rec <- records_to_tibble(eb)

# Plots ------
ggplot(rec, aes(timestamp, power)) +
  geom_line() +
  geom_rug()
ggplot(rec, aes(timestamp, heart_rate)) +
  geom_line()
ggplot(rec, aes(distance, altitude)) +
  geom_line()
ggplot(rec, aes(position_long, position_lat)) +
  geom_point()

plot(x = rec$timestamp, y = rec$power, typ = 'l')
lines(rec$timestamp, rec$altitude, col = 2)

rec %>%
  mutate(lag = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))) %>%
  select(timestamp, lag, power) %>%
  count(lag)
mean(rec$power, na.rm = T) #133.8 strava = 134, garmin 136, vv 133
sum(rec$power, na.rm = T) / nrow(rec) # only 4 NAs so similar

rec %>%
  mutate(lag = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))) %>%
  ggplot(aes(timestamp, lag)) +
  geom_line()
count_nas(rec)

# Maps -------
library(sf)
sf1 <- rec %>%
  filter(!is.na(position_lat)) %>%
  st_as_sf(coords = c("position_long", "position_lat"))
ggplot(sf1) +
  geom_sf()

library(leaflet)

rec %>%
  select(position_long, position_lat) %>%
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines( )

# record list explore ------
rec_list <- readRDS("data_processed/records_2023-07-11.RDS")
rec <- rec_list[[1]]
count(rec, left_right_balance)
count(rec, battery_soc)
batt <- map(rec_list, ~pull(., battery_soc))
unlist(batt) %>% vcount()
head(batt)
dtb <- tibble(name = str_remove(names(batt), "data_processed/eb_"),
              data = batt) %>%
  mutate(id = row_number()) %>%
  mutate(min = map_int(data, min, na.rm = T))
dtb %>%
  arrange(min)

rec_list[[266]] %>% map_route()

# Coros -----------
fs::dir_ls("data", regexp = "^\\d{6}")
fs::dir_ls("data") %>% tail()

cr <- readFitFile("data/453647042718236674.fit")
listMessageTypes(cr)
rec <- records_to_tibble(cr)
ss <- getMessagesByType(cr, "session")
glimpse(rec)
