# remotes::install_github("grimbough/fitFileR")
library(FITfileR)
library(tidyverse)
library(edwards)
library(slider)
source("FUNC.R")
theme_set(theme_minimal())

fs::dir_ls()
froot <- "C:/Users/James/Dropbox/Mine/Personal/BU/garmin old files/bike_20-05-22"

# Not sure what this is:
garmin_file <- system.file("extdata/Garmin.fit", package = "fitFileR")
garmin <- readFitFile(garmin_file)
names(garmin)

?readFitFile
# Read one of my FIT files
file <- file.path(froot, "2020-05-20-11-47-35.fit")
file <- "data/2020-05-20-11-47-35.fit"
file2 <- "data/2022-05-29-073545-ELEMNT BOLT 3A07-256-0.fit"

dt <- readFitFile(file)
eb <- readFitFile(file2)

eb
names(dt)
str(dt)
map(dt, class)
map(dt, dim)

rec_list <- records(dt)
map_int(rec_list, nrow)
map(rec_list, ~slice_head(.)$timestamp)
rec <- rec_list %>%
  bind_rows() %>%
  arrange(timestamp)
rec <- records_to_tibble(dt)

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

# Power Curve-----------
rec %>%
  mutate(ma = slide_dbl(power, mean, .before = 30)) %>%
  mutate(entry = row_number()) %>%
  ggplot(aes(entry, ma)) +
  geom_line()

slide_dbl(rec$power, mean, .before = 10, .complete = T) %>% head(15)
peak_power <- function(dat, seconds) {
  max(slide_dbl(dat$power, mean, .before = seconds), na.rm = TRUE)
}
peak_power(rec, 600)
times <- c(1:4, seq(5, 30, by = 5), 40, 50, 60, seq(120, 600, by = 60), seq(900, 1800, by = 300))
pwr <- map_dbl(times, ~peak_power(rec, .))
seconds_to_period(times)
pwr_tbl <- tibble(times, pwr)
prinf(pwr_tbl)
ggplot(pwr_tbl, aes(times, pwr)) +
  geom_line() +
  scale_x_continuous(labels = seconds_to_text,
                     breaks = c(30, 300, 600, 1200))

# Write notes on this but I can just use fixed breaks and labels because the range will be fixed.
?seconds_to_period

seconds_to_text <- function(x) {
  case_when(
    x < 60 ~ paste0(x, "S"),
    x %% 60 == 0 ~ paste0(x/60, "M"),
    TRUE ~ as.character(seconds_to_period(x))
  )
}
if (x < 60) return()
if (x %% 60 == 0) return(paste0(x, "M"))
seconds_to_text(times)
