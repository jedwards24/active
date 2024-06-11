# Grouping by user supplied functions.
# Make full cookbook notes?

mt <- as_tibble(mtcars)
f <- function(fun, name) {
  mt %>%
    mutate(across(gear, {{fun}}, .names = name))
}

f(function(x) x ^ 2, "aaa")

f <- function(fun, name = as.character(substitute(fun))) {
  mt %>%
    mutate("{name}" := {{ fun }}(gear)) %>%
    group_by(.data[[name]])
}

f2 <- function(fun, name = fun) {
  mt %>%
    mutate("{{name}}" := {{ fun }}(gear)) %>%
    group_by({{ name }})
}

f(function(x) x ^ 2)
f(min)

f2(function(x) x ^ 2)

pwr_duration2 <- function(session,
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
x1 <- pwr_duration(sess, "hour")
x2 <- pwr_duration2(sess, hour, "hour")
x3 <- pwr_duration2(sess, hour)

identical(x1, x3)

# https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string


ff <- function(fun) {
  as.character(substitute(fun))
}
ff(max)
ff(function(x) x ^ 2)
