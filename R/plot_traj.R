#' Plot region-level SIR trajectories
#'
#' This function takes as input an array of herd SIR trajectories:
#' dim =  N_herds x 4 SEIR x n_particles x n_timepoints
#'
#' Also requires a vector of length N_herds detailing which region each herd belongs to.
#' Also requires a vector of length N_regions listing the name of each US region.
#'
#' @return A list of ggplot objects. A country total, and a region-total
#' @export
plot_trajectories <- function(input_array, herd_to_region_index, US_region_names = continental_US_regions()){

#First, check if it's just 1 particle, if so, it's simpler:
  if(length(dim(input_array)) == 3){
    print("Assuming there is only one particle run. Will not plot 95% CI.")

    # Converting the array into a long data frame
    seir_df <- reshape2::melt(input_array)
    colnames(seir_df) <- c("Population", "Compartment", "Time", "Value")
    seir_df$Region <- herd_to_region_index[seir_df$Population]

    # Mapping compartments to names
    compartment_names <- c("S", "E", "I", "R")
    seir_df$Compartment <- factor(seir_df$Compartment, levels = 1:4, labels = compartment_names)
    seir_df$Region <- factor(seir_df$Region, levels = 1:length(US_region_names), labels = US_region_names )

    # Aggregating data for each region
    region_aggregated_df <- aggregate(Value ~ Region + Compartment + Time, data = seir_df, FUN = sum)

    # Aggregating data for the entire US
    us_aggregated_df <- aggregate(Value ~ Compartment + Time, data = seir_df, FUN = sum)
    us_aggregated_df$Region <- "US"

    #Plot US total
    library(ggplot2)
    ggplot(us_aggregated_df) +
      geom_line(aes(x = Time, y = Value, color = Compartment)) +
      theme(axis.text = element_text(size = rel(1.2)),
            axis.title = element_text(size = rel(1.3)),
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.3))) +
      scale_color_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
      ggtitle("Total US Dairy Cattle") +
      theme_classic() -> tot_plot

    # Create a list to store the plots
    plot_list <- list()
    plot_list[["Total"]] <- tot_plot

    for(region in US_region_names){
      # Create the plot
      p <- ggplot(dplyr::filter(region_aggregated_df, Region == region)) +
        geom_line(aes(x = Time, y = Value, color = Compartment)) +
        theme(axis.text = element_text(size = rel(1.2)),
              axis.title = element_text(size = rel(1.3)),
              legend.text = element_text(size = rel(1.2)),
              legend.title = element_text(size = rel(1.3))) +
        labs(title = paste("SIR Trajectories for", region),
             x = "Time",
             y = "Value") +
        scale_color_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
        theme_classic()

      # Add the plot to the list
      plot_list[[region]] <- p
    }

  }else if(length(dim(input_array)) == 4){

    # Converting the array into a long data frame
    seir_df <- reshape2::melt(input_array)
    colnames(seir_df) <- c("Population", "Compartment", "Particle", "Time", "Value")
    seir_df$Region <- herd_to_region_index[seir_df$Population]

    # Mapping compartments to names
    compartment_names <- c("S", "E", "I", "R")
    seir_df$Compartment <- factor(seir_df$Compartment, levels = 1:4, labels = compartment_names)
    seir_df$Region <- factor(seir_df$Region, levels = 1:length(US_region_names), labels = US_region_names )

    # Aggregating data for each region
    region_aggregated_df <- aggregate(Value ~ Region + Compartment + Particle + Time, data = seir_df, FUN = sum)

    # Aggregating data for the entire US
    us_aggregated_df <- aggregate(Value ~ Compartment + Particle + Time, data = seir_df, FUN = sum)

    library(magrittr)
    # Calculate mean and 95% CI for each state
    region_aggregated_df <- region_aggregated_df %>%
      dplyr::group_by(Region, Compartment, Time) %>%
      dplyr::summarize(across(Value, list(mean = ~mean(.), lower = ~quantile(., 0.025), upper = ~quantile(., 0.975)), .names = "{fn}"))

    # Calculate mean and 95% CI for the entire US
    us_aggregated_df <- us_aggregated_df %>%
      dplyr::group_by(Compartment, Time) %>%
      dplyr::summarize(across(Value, list(mean = ~mean(.), lower = ~quantile(., 0.025), upper = ~quantile(., 0.975)), .names = "{fn}"))
    us_aggregated_df$Region <- "US"

    #Plot US total
    library(ggplot2)
    ggplot(dplyr::filter(us_aggregated_df, Compartment != "E")) +
      geom_line(aes(x = Time, y = mean, color = Compartment)) +
      geom_ribbon(aes(x = Time, ymin = lower, ymax = upper,
                      fill = Compartment), alpha = 0.2) +
      theme(axis.text = element_text(size = rel(1.2)),
            axis.title = element_text(size = rel(1.3)),
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_text(size = rel(1.3))) +
      scale_color_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
      scale_fill_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
      ggtitle("Total US Dairy Cattle") +
      theme_classic() -> tot_plot

    # Create a list to store the plots
    plot_list <- list()
    plot_list[["Total"]] <- tot_plot

    for(region in US_region_names){
      # Create the plot
      p <- ggplot(dplyr::filter(region_aggregated_df, Region == region, Compartment != "E")) +
        geom_line(aes(x = Time, y = mean, color = Compartment)) +
        geom_ribbon(aes(x = Time, ymin = lower, ymax = upper,
                        fill = Compartment), alpha = 0.2) +
        theme(axis.text = element_text(size = rel(1.2)),
              axis.title = element_text(size = rel(1.3)),
              legend.text = element_text(size = rel(1.2)),
              legend.title = element_text(size = rel(1.3))) +
        labs(title = paste("SIR Trajectories for", region),
             x = "Time",
             y = "Value") +
        scale_color_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
        scale_fill_manual(values = c("S" = "#4B59B4", "E" = NULL, "I" = "#b44b59", "R" = "#59B44B")) +
        theme_classic()

      # Add the plot to the list
      plot_list[[region]] <- p
    }

  }else(
    stop("Error: Expected only 3 or 4 dimensions of input array.")
  )
  return(plot_list)
}



# Function to calculate mean and 95% CI using quantiles
calc_mean_ci <- function(arr) {
  # Calculate mean
  mean_vals <- apply(arr, c(1, 2, 4), mean)
  # Calculate 95% CI using quantiles
  lower_ci <- apply(arr, c(1, 2, 4), quantile, probs = 0.025)
  upper_ci <- apply(arr, c(1, 2, 4), quantile, probs = 0.975)

  # Combine results into an array with the third dimension for mean, lower, and upper CI
  stats_array <- array(NA, dim = c(dim(arr)[1], dim(arr)[2], 3, dim(arr)[4]))
  stats_array[, , 1, ] <- mean_vals
  stats_array[, , 2, ] <- lower_ci
  stats_array[, , 3, ] <- upper_ci
  stats_array
}
