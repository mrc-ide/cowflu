process_data_incidence <- function(data) {
  states <- tolower(usda_data$US_States)
  start_date <- as.Date("2023-12-18")
  data$day <- as.numeric(data$Week_Beginning - start_date)
  data$State <- tolower(data$State)
  d <- unname(split(data, data$Week_Beginning))
  positive_tests <- lapply(d, function(x) {
    i <- match(states, x$State)
    stopifnot(!any(is.na(i)))
    x$Positive_Tests[i]
  })
  day <- vapply(d, function(x) x$day[[1]], numeric(1))
  data.frame(day = day,
             week = day / 7,
             positive_tests = I(positive_tests))
}


process_data_outbreak <- function(data) {
  states <- tolower(usda_data$US_States)
  start_date <- as.Date("2023-12-18")
  data$day <- as.numeric(data$Week_Beginning - start_date)
  data$State <- tolower(data$State)
  # Split the data by Week_Beginning
  d <- unname(split(data, data$Week_Beginning))
  # Initialize a cumulative state outbreak detection tracker
  cumulative_outbreak <- rep(FALSE, length(states))

  # Process positive tests per week and state
  outbreak_detected <- lapply(d, function(x) {
    # Match current week's data with the full state list
    i <- match(states, x$State)
    stopifnot(!any(is.na(i)))
    # For each state, check if there were positive tests this week
    tests_this_week <- x$Positive_Tests[i] > 0
    # Update the cumulative outbreak detection. "<<-" assigns outside of the lapply environment.
    cumulative_outbreak <<- cumulative_outbreak | tests_this_week
    # Return 1 if outbreak has been detected, 0 otherwise
    as.numeric(cumulative_outbreak)
  })

  day <- vapply(d, function(x) x$day[[1]], numeric(1))
  data.frame(day = day,
             week = day / 7,
             outbreak_detected = I(outbreak_detected))
}
