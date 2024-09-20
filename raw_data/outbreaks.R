## There is currently no API to access the USDA's confirmed outbreaks data.
## This script processes the raw data acquired from:
## https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/hpai-confirmed-cases-livestock
## Then downloading the csv of all outbreaks by state.
## Select "Download Data" in top right of widget. Then "Table details by date".
## Then ensure the file is in comma-separated form.
## This script then processes that file.

process_outbreak_data <- function(root, usda_data){
  # Define path to data
  outbreak_data_path <- file.path(root, "raw_data", "Raw_Outbreaks_Data_Sep20.csv")

  # Load the data
  outbreak_data <- read.csv(outbreak_data_path)

  # Fix date format
  outbreak_data$Confirmed <- as.Date(outbreak_data$Confirmed, format = "%d-%b-%y")
  # Set states to lowercase
  outbreak_data$State <- tolower(outbreak_data$State)

  # Filter out the llamas...
  outbreak_data <- dplyr::filter(outbreak_data, Production == "Dairy Milking Cows")
  outbreak_data <- outbreak_data[,c(1,2)]


  # Generate a complete sequence of dates from the minimum to the maximum date in the dataframe
  all_dates <- seq.Date(min(outbreak_data$Confirmed), max(outbreak_data$Confirmed), by = "day")

  # Get all unique states
  All_states <- tolower(usda_data$US_States)

  # Create a new dataframe with all combinations of dates and states
  complete_data <- expand.grid(Date = all_dates, State = All_states)

  # Count positive tests for each Date and State
  positive_tests <- dplyr::summarise(
    dplyr::group_by(
      outbreak_data,
      Confirmed, State
    ),
    Positive_Tests = dplyr::n(),
    .groups = 'drop'
  )

  # Merge the complete data with the positive tests
  daily_outbreaks <- merge(complete_data, positive_tests, by.x = c("Date", "State"), by.y = c("Confirmed", "State"), all.x = TRUE)

  # Replace NA with 0 in Positive_Tests
  daily_outbreaks$Positive_Tests[is.na(daily_outbreaks$Positive_Tests)] <- 0

  weekly_outbreaks <- dplyr::ungroup(
    dplyr::summarize(
      dplyr::group_by(
        dplyr::mutate(
          daily_outbreaks,
          Week_Beginning = lubridate::floor_date(Date, unit = "week", week_start = 1)
        ),
        Week_Beginning, State
      ),
      Positive_Tests = sum(Positive_Tests, na.rm = TRUE)
    )
  )

  # Return the dataframes
  list(daily_outbreaks_data = daily_outbreaks,
       weekly_outbreaks_data = weekly_outbreaks)

}
