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
walk(ff2, process_eb, .progress = TRUE) # ~10 files per minute

# Extract main messages from files saved in previous step
ff3 <- ftb %>%
  filter(todo) %>%
  pull(proc_name)

system.time(walk(ff3, fit_extract_save, .progress = TRUE)) # ~30/min

# Load all extracts to a list
fe_list <- map(fs::dir_ls("data_processed/extracts", regexp = "extr_eb_"), readRDS) %>%
  set_names(~str_remove(path_ext_remove(path_file(.)), "^extr_"))
ids <- names(fe_list)
# Save a collection for each message type
rec_list <- map(fe_list, ~pluck(., "record"))
sess <- map_dfr(fe_list, ~pluck(., "session")) %>%
  mutate(id = ids, .before = 1)
lap_list <- map(fe_list, ~pluck(., "lap"))
acts <- map_dfr(fe_list, ~pluck(., "activity")) %>%
  mutate(id = ids, .before = 1)
event_list <- map(fe_list, ~pluck(., "event"))

saveRDS(rec_list, "data_processed/records.RDS")
saveRDS(sess, "data_processed/sessions.RDS")
saveRDS(lap_list, "data_processed/laps.RDS")
saveRDS(event_list, "data_processed/events.RDS")
saveRDS(acts, "data_processed/acts.RDS")
object_size_all()
