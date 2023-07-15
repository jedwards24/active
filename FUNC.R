records_to_tibble <- function(fit_file) {
  records(fit_file) %>%
    bind_rows() %>%
    arrange(timestamp)
}
