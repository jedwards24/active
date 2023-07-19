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

map_route <- function(record) {
  record %>%
    select(position_long, position_lat) %>%
    as.matrix() %>%
    leaflet::leaflet(  ) %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines( )
}

# Power-Duration Curves---------------

# Gives peak mean power for a given number of seconds for a single activity record.
# Returns a numeric scalar.
peak_power <- function(record, seconds) {
  slide_dbl(record$power, ~sum(., na.rm = TRUE) / seconds, .before = seconds - 1) %>%
    max()
}

# Gives peak mean power for a vector of durations for a single activity record.
# Returns a numeric vector.
power_bests <- function(record, times) {
  map_dbl(times, ~peak_power(record, .))
}

# Combine a list of power best vectors (peak power for each duration in `times`) into
# a single vector by taking the best power for each duration over all vectors.
# Returns a numeric vector
combine_power_bests <- function(x, times) {
  list_c(x) %>%
    matrix(nrow = length(times)) %>%
    apply(1, max)
}

# Group session data by start_time column.
group_by_time <- function(session, unit = "year") {
  if (unit == "year"){
    return(group_by(session, year = factor(year(start_time))))
  }
  if (unit == "month"){
    return(group_by(session, month = factor(month(start_time))))
  }
  if (unit == "week"){
    return(group_by(session, week = factor(week(start_time))))
  }
  if (unit == "day"){
    return(group_by(session, day = factor(day(start_time))))
  }
  if (unit == "hour"){
    return(group_by(session, hour = factor(hour(start_time))))
  }
}

# Input: session data with list column `power_bests`.
power_duration <- function(session, unit, times) {
  session %>%
    group_by_time(session = session, unit = unit) %>%
    summarise(power = list(combine_power_bests(power_bests, times = times))) %>%
    mutate(times = list(times)) %>%
    unchop(c(power, times))
}

# Returns power-duration plot
# Input: output from `power_duration()`.
plot_power_duration <- function(power_duration_data) {
  power_duration_data %>%
    ggplot(aes(times, power, group = year, colour = year, linetype = year)) +
    geom_line() +
    scale_x_continuous(labels = c("5s", "30s", "1m", "5m", "10m", "1hr"),
                       breaks = c(5, 30, 60, 300, 600, 3600),
                       limits = c(3, NA),
                       trans = "log")
}
