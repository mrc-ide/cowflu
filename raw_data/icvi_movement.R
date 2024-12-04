## This script processes the real (and extra imputed) ICVI travel data provided by authors of:
## Spatial and network analysis of U.S. livestock movements based on Interstate Certificates of Veterinary Inspection
## https://doi.org/10.1016/j.prevetmed.2021.105391

## This script won't run if you don't have the original data (not provided as it's not mine to share)
## but we include the script within the cowflu repo
##################################################################################

process_icvi_movement <- function(root, usda_data) {

# Define path to data
movement_data_path <- file.path(root, "raw_data", "DairyMovementsUS.xlsx")
# Load the data
movement_data <- readxl::read_excel(movement_data_path, sheet = 2)
# Cut cols 4 and 6:
movement_data <- movement_data[,-c(4,6)]
# Create a named vector for abbreviation to full state name conversion
state_map <- setNames(state.name, state.abb)
# Add new columns with full state names
movement_data$OriginStateFull <- toupper(state_map[movement_data$OriginState])
movement_data$DestinationStateFull <- toupper(state_map[movement_data$DestinationState])
##################################################################################
## Three things to calculate:
## p_region_export
## movement_matrix
#################################################################################
## p_region_export
## Calculate the total number of shipments out of each state, divide that number by the number of herds in that state
## Divide by 365 to get daily
real_p_region_export <- rep(NA, 48)
for(i in 1:48){
  state_hold <- usda_data$US_States[i]
  data_hold <- dplyr::filter(movement_data, OriginStateFull == state_hold)
  total_shipments <- sum(data_hold$NumberShipmens)
  total_per_herd <- total_shipments/usda_data$n_herds_per_region[i]
  daily_total <- total_per_herd/365
  ## Convert this daily probability to a weekly probability of export:
  real_p_region_export[i] <- 1 - (1 - daily_total)^7
}

#################################################################################
## p_cow_export cannot really be correctly calculated using this dataset.
#################################################################################
##movement_matrix
## A matrix where [i,j] lists, given that there is an export from region i, what region j will it export to?
#Divide each row by the sum of the row to convert to probabilities
real_movement_matrix <- matrix(data = 0, nrow = 48, ncol = 48)

#First, populate the matrix with the total shipments from i to j, then just scale it
for(i in 1:48){
  state_hold <- usda_data$US_States[i]
  data_hold <- dplyr::filter(movement_data, OriginStateFull == state_hold)
  data_hold <- data_hold[,c(3,6,7)]
  ## Sum by NumberShipmens
  summed_data <- dplyr::summarise(dplyr::group_by(data_hold, OriginStateFull, DestinationStateFull),
              TotalShipments = sum(NumberShipmens, na.rm = TRUE), .groups = "drop")
  for(j in 1:nrow(summed_data)){
    col_index <- which(usda_data$US_States == summed_data$DestinationStateFull[j])
    real_movement_matrix[i,col_index] <- summed_data$TotalShipments[j]
  }
  }


for (i in 1:48) {
  real_movement_matrix[i,] <- real_movement_matrix[i,]/sum(real_movement_matrix[i,])
}



list(real_p_region_export = real_p_region_export,
     real_movement_matrix = real_movement_matrix)

}
