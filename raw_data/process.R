real_dairy_populations <- function(root) {
  # Define the path to the data
  cows_data_path <- file.path(root, "raw_data", "US_number_of_dairy_cows.csv")
  farms_data_path <- file.path(root, "raw_data", "US_number_of_dairy_cow_farms.csv")

  # Load the data
  cows_data <- read.csv(cows_data_path)
  farms_data <- read.csv(farms_data_path)

  # Remove superfluous columns:
  cows_data <- cows_data[,c("State", "Data.Item", "Domain.Category", "Value")]
  farms_data <- farms_data[,c("State", "Data.Item", "Domain.Category", "Value")]

  # Remove Alaska and Hawaii:
  cows_data <- dplyr::filter(cows_data, !(State %in% c("ALASKA", "HAWAII")))
  farms_data <- dplyr::filter(farms_data, !(State %in% c("ALASKA", "HAWAII")))

  # Remove categories "10-49 cows" and "500 or more" cows, these are superfluous, as they include more detailed categories
  cows_data <- dplyr::filter(cows_data, !(Domain.Category %in% c("INVENTORY OF MILK COWS: (10 TO 49 HEAD)" ,
                                                                 "INVENTORY OF MILK COWS: (500 OR MORE HEAD)")))
  farms_data <- dplyr::filter(farms_data, !(Domain.Category %in% c("INVENTORY OF MILK COWS: (10 TO 49 HEAD)" ,
                                                                   "INVENTORY OF MILK COWS: (500 OR MORE HEAD)")))

  # Remove commas and convert to numeric values:
  # Also remove (D)- which is listed when value is not disclosed to preserve individuals' information.
  cows_data$Value <- gsub("\\(D\\)", "", cows_data$Value)
  cows_data$Value <- as.numeric(gsub(",", "", cows_data$Value))
  farms_data$Value <- gsub("\\(D\\)", "", farms_data$Value)
  farms_data$Value <- as.numeric(gsub(",", "", farms_data$Value))

  # How many farms in total?
  n_herds <- sum(farms_data$Value)

  # How many farms per region?
  US_States <- unique(cows_data$State)
  n_herds_per_region <- rep(NA, length(US_States))
  for(i in seq_along(US_States)){
    n_herds_per_region[i] <- sum(dplyr::filter(farms_data, State == US_States[i])$Value)
  }
  region_start <- as.integer(c(0, cumsum(n_herds_per_region)))

  # How many cows per farm?
  n_cows_per_herd <- rep(NA, n_herds)
  # For each US State we calculate:
  for (i in seq_along(US_States)) {
    state_farm_data <- dplyr::filter(farms_data, State == US_States[i])
    state_cows_data <- dplyr::filter(cows_data, State == US_States[i])
    if (nrow(state_farm_data) != nrow(state_cows_data)) {
      stop("Error: Mismatch in state farm populations.")
    }

    state_cows <- NULL
    for (j in 1:nrow(state_farm_data)) {
      if (state_farm_data$Domain.Category[j] != state_cows_data$Domain.Category[j]) {
        stop("Error: Mismatch of farm size category.")
      }
      state_cows <- c(state_cows, distribute_cows_evenly(farms = state_farm_data$Value[j],
                                                         cows = state_cows_data$Value[j],
                                                         category = state_farm_data$Domain.Category[j]))
    }

    #Add this state_cows vector into the national total:
    #Check it's the right length
    if (length(n_cows_per_herd[(region_start[i]+1):(region_start[i+1])]) != length(state_cows)) {
      stop("Error: Not the right number of herds for the state.")
    }

    n_cows_per_herd[(region_start[i]+1):(region_start[i+1])] <- state_cows
  }

  # Return the data of interest in a list
  list(US_States = US_States,
       n_herds = n_herds,
       n_herds_per_region = n_herds_per_region,
       region_start = region_start,
       n_cows_per_herd = n_cows_per_herd)
}


distribute_cows_evenly <- function(farms, cows, category) {

  if(is.na(cows)){
    #If there are NA cows, it's because there's so few farms, that they can't reveal it
    #In this instance, use the category midpoint
    cows <- floor(mean(extract_bounds(category, uncertain_no_cows = TRUE))*farms)
  }
  # Compute the base number of cows per farm
  base_cows_per_farm <- cows %/% farms
  remainder_cows <- cows %% farms

  # Create a vector with base cows per farm
  cow_distribution <- rep(base_cows_per_farm, farms)

  # Distribute the remainder cows
  if (remainder_cows > 0) {
    cow_distribution[1:remainder_cows] <- cow_distribution[1:remainder_cows] + 1
  }

  #Check these numbers are within expected range
  if(any(cow_distribution < extract_bounds(category)[1])){
    stop("Error: We should have more cows than this.")
  }
  if(any(cow_distribution > extract_bounds(category)[2])){
    stop("Error: We should have less cows than this.")
  }
  cow_distribution
}

# Function to extract the limits of the range from the Domain.Category
extract_bounds <- function(category, uncertain_no_cows = FALSE) {
  # Remove the non-numeric part and split the range
  range <- gsub("INVENTORY OF MILK COWS: |HEAD|,", "", category)
  range <- trimws(range)
  if (grepl("OR MORE", range)) {
    lower_bound <- as.numeric(gsub("[^0-9]", "", range))
    if(uncertain_no_cows){
      upper_bound <- 1.5 * lower_bound
    } else {
      upper_bound <- 1e6
    }
  } else {
    bounds <- as.numeric(gsub("[^0-9]", "",unlist(strsplit(range, " TO "))))
    lower_bound <- min(bounds)
    upper_bound <- max(bounds)
  }
  c(lower_bound, upper_bound)
}


root <- here::here()
usda_data <- real_dairy_populations(root)
save("usda_data", file = file.path(root, "R/sysdata.rda"), version = 2)
message("Wrote package data")
