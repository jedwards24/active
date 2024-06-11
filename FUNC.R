
map_route <- function(record) {
  record %>%
    select(position_long, position_lat) %>%
    as.matrix() %>%
    leaflet::leaflet(  ) %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines( )
}

# File Processing -------------

# Input is a `fit_file` object.
# Extracts five types of messages, each into their own tibble and
# outputs all as a named list.
# The use of `possibly()` is to handle an error in one file where the session data is missing.
fit_extract <- function(x) {
  possible_message <- possibly(getMessagesByType, tibble(NA))
  list(record = records_to_tibble(x),
       lap = laps_to_tibble(x),
       session = possible_message(x, "session"),
       event = possible_message(x, "event"),
       activity = possible_message(x, "activity"))
}

# Extract records from a fit file, combine and sort by timestamp.
records_to_tibble <- function(fit_file) {
  records(fit_file) %>%
    bind_rows() %>%
    arrange(timestamp)
}

laps_to_tibble <- function(fit_file) {
  laps(fit_file) %>%
    bind_rows() %>%
    arrange(timestamp)
}

# Counts message types using an internal lookup table.
# Used for understanding fit file objects and messages.
count_message_types <- function(fit_file) {
  all_gmn <- map_int(FITfileR:::messages(fit_file), FITfileR:::globalMessageNumber)
  tibble(key = all_gmn) %>%
    left_join(FITfileR:::fit_data_types$mesg_num) %>%
    count(value)
}

# Read fit file and save results with file name derived from the original
process_eb <- function(file, dest = "data_processed/fit_objs/") {
  date <- str_extract(fs::path_file(file), "^\\d{4}-\\d{2}-\\d{2}-\\d{6}")
  stopifnot(length(date) > 0)
  dt <- readFitFile(file)
  saveRDS(dt, fs::path(dest, eb_save_name(date)))
}

# Helper for process_eb() but also used in files_PROCESS.R for consistent naming.
# `date` is "yyyy-mm-dd-hhmmss"
eb_save_name <- function(date) {
  paste0("eb_", date, ".RDS")
}

# Read extract from fit file and save results with file name derived from the original
fit_extract_save <- function(file, dest = "data_processed/extracts") {
  file_name <- fs::path_file(file)
  save_name <- paste0("extr_", file_name)
  fit <- fit_extract(readRDS(file))
  saveRDS(fit, fs::path(dest, save_name))
}

# Power-Duration Curves---------------

# Get `pwr_bests()` for the given record/times and save to `dest` folder.
# `id` is added to the saved tibble as an "id" attribute.
pwr_bests_save <- function(record,
                          id,
                          times = pwr_time_range(),
                          dest = "data_processed/power_bests") {
  df <- pwr_bests(record, times)
  attr(df, "id") <- id
  saveRDS(df, fs::path(dest, paste0("pwr_bests_", id, ".RDS")))
}

# A vector of durations in seconds used for
# power-duration curves
pwr_time_range <- function() {
  c(1:4, seq(5, 30, by = 5),
    40, 50, 60,
    seq(120, 600, by = 60),
    seq(900, 1800, by = 300),
    seq(2400, 7200, by = 600))
}

# Gives peak mean power for a given number of seconds for a single activity record.
# Returns a numeric scalar.
pwr_peak <- function(record, seconds) {
  slide_dbl(record$power, ~sum(., na.rm = TRUE) / seconds, .before = seconds - 1) %>%
    max()
}

# Gives peak mean power for a vector of durations for a single activity record.
# Returns a tibble with two columns: seconds and watts.
pwr_bests <- function(record, times = pwr_time_range()) {
  tibble(seconds = times, watts = map_dbl(times, ~pwr_peak(record, .)))
}

# Combine a list of power best vectors (peak power for each duration in `times`) into
# a single vector by taking the best power for each duration over all vectors.
# Returns a numeric vector
pwr_combine_bests <- function(x, times = pwr_time_range()) {
  list_c(x) %>%
    matrix(nrow = length(times)) %>%
    apply(1, max)
}


# Input: session data with list column `power_bests`.
# `fun` is unquoted, `name` is quoted
# Needs some checks to handle auto naming with anonymous functions etc.
pwr_duration <- function(session,
                          fun,
                          name = as.character(substitute(fun)),
                          times = pwr_time_range()) {
  session %>%
    mutate("{name}" := factor({{ fun }}(start_time))) %>%
    group_by(.data[[name]]) %>%
    summarise(power = list(pwr_combine_bests(power_bests, times = times))) %>%
    mutate(times = list(times)) %>%
    unchop(c(power, times))
}

# Returns power-duration plot
# Input: output from `power_duration()`.
pwr_duration_plot <- function(power_duration_data) {
  col <- colnames(power_duration_data)[1]
  power_duration_data %>%
    ggplot(aes(times, power, group = .data[[col]], colour = .data[[col]], linetype = .data[[col]])) +
    geom_line() +
    scale_x_continuous(labels = c("5s", "30s", "1m", "5m", "10m", "1hr"),
                       breaks = c(5, 30, 60, 300, 600, 3600),
                       limits = c(3, NA),
                       trans = "log")
}
