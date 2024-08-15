## A selection of helper functions for exploring herd indices within the usda_data.



##A function to return some helpful info on a given herd number.
herd_stats <- function(herd_number){
  if(herd_number > usda_data$n_herds | herd_number < 0){
    stop("Error: The herd number does not belong to any region.")
  }

    for (i in 1:(length(usda_data$region_start) - 1)) {
      # Check if the number is within the current section
      if (herd_number-1 >= usda_data$region_start[i] && herd_number-1 < usda_data$region_start[i + 1]) {
        region_number <- i
      }
    }
  region_name <- usda_data$US_States[region_number]

  cat(sprintf("Herd number %s is in region %s - %s. \nIt starts with %s cows.",
          herd_number, region_number, region_name, usda_data$n_cows_per_herd[herd_number]))

}

region_stats <- function(region){
  if(is.numeric(region)){} else if(is.character(region) && toupper(region) %in% usda_data$US_States){} else{
    stop("Unrecognised input. Give region number or name.")
  }
  if(is.character(region)){
    region_number <- which(usda_data$US_States == toupper(region))
  }else{
    region_number <- region
  }
  region_indices <- herd_indices_in_region(region_number)

  max_herd_indices <- which( usda_data$n_cows_per_herd[region_indices] == max(usda_data$n_cows_per_herd[region_indices]))
  max_herd_indices <- region_indices[max_herd_indices]
  max_herd_size <- max(usda_data$n_cows_per_herd[region_indices])

  min_herd_indices <- which( usda_data$n_cows_per_herd[region_indices] == min(usda_data$n_cows_per_herd[region_indices]))
  min_herd_indices <- region_indices[min_herd_indices]
  min_herd_size <- min(usda_data$n_cows_per_herd[region_indices])

  mean_herd_size <- mean(usda_data$n_cows_per_herd[region_indices])
  mean_herd_diff <- usda_data$n_cows_per_herd[region_indices] - mean_herd_size
  closest_mean_herd_number <- region_indices[which.min(abs(mean_herd_diff))]
  closest_mean_herd_size <- usda_data$n_cows_per_herd[closest_mean_herd_number]


  cat(sprintf("Region %s - %s. \n
              It has %s herds, from index %s to %s. \n
              The largest herd size is %s herds of %s cows. For example, herd %s. \n
              The smallest herd size is %s herds of %s cows. For example, herd %s. \n
              The average herd size is %s cows. The closest is herd %s with %s cows.",
              region_number, usda_data$US_States[region_number],
              usda_data$n_herds_per_region[region_number],
              region_indices[1], region_indices[length(region_indices)],
              length(max_herd_indices), max_herd_size, max_herd_indices[1],
              length(min_herd_indices), min_herd_size, min_herd_indices[1],
              mean_herd_size, closest_mean_herd_number, closest_mean_herd_size))

}


#Insert either a number or a name of a state to return the indices of every herd in that region
herd_indices_in_region <- function(REGION){
  if(is.numeric(REGION)){
    if(REGION < 49 & REGION > 0)
    {
      start_index <- usda_data$region_start[REGION] + 1
      end_index <- start_index + usda_data$n_herds_per_region[REGION] -1

      return(start_index:end_index)

    }else{
      stop("Unrecognised numeric input. Provide a valid region number.")
    }

  }else if (is.character(REGION)){
    if(toupper(REGION) %in% usda_data$US_States){
      region_index <- which(usda_data$US_States == toupper(REGION))
      start_index <- usda_data$region_start[region_index] + 1
      end_index <- start_index + usda_data$n_herds_per_region[region_index]

      return(start_index:end_index)
    } else{
      stop("Unrecognised character. Please input the name of a US State from usda_data$US_States.")
    }

  } else{
    stop("Unrecognised input. Provide a region number or a US State name.")
  }

}
