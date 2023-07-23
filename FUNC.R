
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
fit_extract <- function(x) {
  list(record = records_to_tibble(x),
       lap = laps_to_tibble(x),
       session = getMessagesByType(x, "session"),
       event = getMessagesByType(x, "event"),
       activity = getMessagesByType(x, "activity"))
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
  saveRDS(dt, fs::path(dest, paste0("eb_", date, ".RDS")))
}

# Save object `x` with name based on the the file part of `load_path`.
# used in files_PROCESS.R
save_extracts <- function(x, load_path, save_folder = "data_processed/extracts") {
  file_name <- fs::path_file(load_path)
  save_name <- paste0("extr_", file_name)
  saveRDS(x, fs::path(save_folder, save_name))
}

# Power-Duration Curves---------------

# A vector of durations in seconds used for
# power-duration curves
pwr_time_range <- function() {
  c(1:4, seq(5, 30, by = 5),
    40, 50, 60,
    seq(120, 600, by = 60),
    seq(900, 1800, by = 300),
    seq(2100, 7200, by = 600))
}

# Gives peak mean power for a given number of seconds for a single activity record.
# Returns a numeric scalar.
pwr_peak <- function(record, seconds) {
  slide_dbl(record$power, ~sum(., na.rm = TRUE) / seconds, .before = seconds - 1) %>%
    max()
}

# Gives peak mean power for a vector of durations for a single activity record.
# Returns a numeric vector.
pwr_bests <- function(record, times = pwr_time_range()) {
  map_dbl(times, ~peak_power(record, .))
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
