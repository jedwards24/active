# Process Wahoo fit files

library(tidyverse)
library(edwards)
library(FITfileR)
library(fs)
source("FUNC.R")

# Read all fit files, as save as fit_file objects
ff <- fs::dir_ls("data", regexp = "ELEMNT")
walk(ff, process_eb)

# Extract main messages from files saved in previous step
# File 270 errors when extracting session data. Excluded for now.
exclude <- 270
ff <- fs::dir_ls("data_processed/fit_objs")[-exclude]
system.time(fe_list <- map(ff, ~fit_extract(readRDS(.)), .progress = T))

# Save extracts individually
map2(fe_list, ff, save_extracts)

# Save a collection for each message type
rec_list <- map(fe_list, ~pluck(., "record"))
sess <- map_dfr(fe_list, ~pluck(., "session"))
lap_list <- map(fe_list, ~pluck(., "lap"))
acts <- map_dfr(fe_list, ~pluck(., "activity"))
event_list <- map(fe_list, ~pluck(., "event"))

saveRDS(rec_list, "data_processed/records.RDS")
saveRDS(sess, "data_processed/sessions.RDS")
saveRDS(lap_list, "data_processed/laps.RDS")
saveRDS(event_list, "data_processed/events.RDS")
saveRDS(acts, "data_processed/acts.RDS")

