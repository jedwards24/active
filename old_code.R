
# Originally used to change axis text
seconds_to_text <- function(x) {
  case_when(
    x < 60 ~ paste0(x, "S"),
    x %% 60 == 0 ~ paste0(x/60, "M"),
    TRUE ~ as.character(seconds_to_period(x))
  )
}
?seconds_to_period

# It was used in the ggplot with this
scale_x_continuous(labels = seconds_to_text,
                   breaks = c(30, 300, 600, 1200))


# Original long method of grouping session by unit ----------

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
pwr_duration <- function(session, unit = "year", times = pwr_time_range()) {
  session %>%
    group_by_time(unit = unit) %>%
    summarise(power = list(pwr_combine_bests(power_bests, times = times))) %>%
    mutate(times = list(times)) %>%
    unchop(c(power, times))
}
