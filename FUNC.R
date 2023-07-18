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
