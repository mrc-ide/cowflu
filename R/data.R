process_data <- function(data) {
  states <- tolower(usda_data$US_States)
  start_date <- as.Date("2023-12-18")
  data$time <- as.numeric(data$Week_Beginning - start_date)
  data$State <- tolower(data$State)
  d <- unname(split(data, data$Week_Beginning))
  positive_tests <- lapply(d, function(x) {
    i <- match(states, x$State)
    stopifnot(!any(is.na(i)))
    x$Positive_Tests[i]
  })
  time <- vapply(d, function(x) x$time[[1]], numeric(1))
  data.frame(time = time, positive_tests = I(positive_tests))
}
