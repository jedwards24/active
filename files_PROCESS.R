# Process Wahoo fit files

library(tidyverse)
library(edwards)
library(FITfileR)
library(fs)
source("FUNC.R")

# Read all fit files, as save as fit_file objects
ff <- fs::dir_ls("data", regexp = "ELEMNT")
# Table of filenames to check what has already been processed
ff_proc <- fs::dir_ls("data_processed/fit_objs", regexp = "eb_")
ftb <- tibble(data_name = ff,
              date = str_extract(ff, "\\d{4}-\\d{2}-\\d{2}-\\d{6}"),
              proc_name = path("data_processed", "fit_objs", eb_save_name(date)),
              todo = !proc_name %in% ff_proc)
count(ftb, todo)

ff2 <- ftb %>%
  filter(todo) %>%
  pull(data_name)
walk(ff2, process_eb, .progress = TRUE)

# Extract main messages from files saved in previous step
# File 270 errors when extracting session data. Excluded for now.
#exclude <- 270
#ff <- fs::dir_ls("data_processed/fit_objs")[-exclude]
ff3 <- ftb %>%
  filter(todo) %>%
  pull(proc_name)

system.time(walk(ff3, fit_extract_save, .progress = TRUE))

# Load all extracts to a list
fe_list <- map(fs::dir_ls("data_processed/extracts", regexp = "extr_eb_"), readRDS)

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

