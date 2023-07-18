
library(tidyverse)
library(edwards)
library(FITfileR)
library(bench)
library(lobstr)
source("FUNC.R")
library(fs)

ff <- fs::dir_ls("data")
dir_contents("data")
ff <- dir_files("data", regexp = "ELEMNT")$file

# Info in file name
str_extract(ff, "^\\d{4}") %>% vcount()
ids <- str_extract(ff, "(?<=3A07-)\\d*") %>% as.integer()
tail(ids)
?dir_info

# Process fit files ----------
# Read fit file and save results with file name derived from the original
process_eb <- function(file, dest = "data_processed/fit_objs/") {
  date <- str_extract(fs::path_file(file), "^\\d{4}-\\d{2}-\\d{2}-\\d{6}")
  stopifnot(length(date) > 0)
  dt <- readFitFile(file)
  saveRDS(dt, fs::path(dest, paste0("eb_", date, ".RDS")))
}

ff <- fs::dir_ls("data", regexp = "ELEMNT")
head(ff)
process_eb(ff[1])
walk(ff, process_eb)
fs::dir_ls("data")[2] %>% path_file()

x <- readRDS("data_processed/2020-07-06_EB_1.RDS")
file_id(x)

# Processed files---------
# Want: record, session, lap, event?, activty?
# File 270 errors when extracting session data. Excluded for now.
ff <- fs::dir_ls("data_processed/fit_objs")
f2 <- dir_files("data_processed")
f2 %>%
  arrange(desc(size))
system_time(fit <- readRDS(ff[9]))
rec <- records_to_tibble(fit)
lap <- laps(fit)
ss <- getMessagesByType(fit, "session")
object_size_all()
#fits <- map(, readRDS)

fit_extract <- function(x) {
  list(record = records_to_tibble(x),
       lap = laps_to_tibble(x),
       session = getMessagesByType(x, "session"),
       event = getMessagesByType(x, "event"),
       activity = getMessagesByType(x, "activity"))
}
fe <- fit_extract(fit)
fe
map_int(fe, nrow)
count(fe$event, event, event_type)
fe$activity

system_time(fe_list <- map(ff[-270], ~fit_extract(readRDS(.)), .progress = T))
bench::bench_bytes(object.size(fe_list))
obj_size(fe_list)
map(fe_list, obj_size) %>% list_simplify()
rec_list <- map(fe_list, ~pluck(., "record"))
sess <- map_dfr(fe_list, ~pluck(., "session"))
lap_list <- map(fe_list, ~pluck(., "lap"))
act_list <- map(fe_list, ~pluck(., "activity"))
map_int(act_list, nrow) %>% unname %>% max
map_int(lap_list, length) %>% unname %>% vcount

acts <- map_dfr(fe_list, ~pluck(., "activity"))
event_list <- map(fe_list, ~pluck(., "event"))
saveRDS(rec_list, "data_processed/records_2023-07-11.RDS")
saveRDS(sess, "data_processed/sessions_2023-07-11.RDS")
saveRDS(lap_list, "data_processed/laps_2023-07-11.RDS")
saveRDS(event_list, "data_processed/events_2023-07-11.RDS")
saveRDS(acts, "data_processed/acts_2023-07-11.RDS")

rec_list[[1]]
fe_list[[2]][[5]]

fit <- readRDS(ff[270])
fit_extract((fit))
count_message_types(fit)
getMessagesByType(fit, "session")
lap <- getMessagesByType(fit, "lap") %>% bind_rows() %>% arrange(timestamp)
getMessagesByType(fit, "activty")
lap %>% select(avg_power, total_ascent, normalized_power)
rec <- records_to_tibble(fit)
rec

# save ------------
save_extracts <- function(x, load_path) {
  file_name <- fs::path_file(load_path)
  save_name <- paste0("extr_", file_name)
  saveRDS(x, fs::path("data_processed", "extracts", save_name))
}
save_extracts(fe_list[[1]], path_file(ff[1]))
map2(fe_list, ff[-270], save_extracts)
