download_movement <- function(root, redownload = FALSE) {
  dest <- file.path(root, "raw_data", "movement.zip")
  if (file.exists(dest)) {
    if (redownload) {
      unlink(dest)
    } else {
      return(dest)
    }
  }
  ## it's a big old file, we need a long timeout (1h here).  Using
  ## curl, rather than download.file, as it's much more reliable.
  url <- "https://mountainscholar.org/bitstreams/e7237627-74a0-4d9d-a31d-0e6999d68d08/download"
  message("Downloading very large file, this may take a while...")
  curl::curl_download(url, dest, quiet = FALSE, mode = "wb")

  message("\nVerifying file") # curl does not leave trailing newline
  md5_found <- tools::md5sum(dest)
  md5_expected <- "092818bf9b1120da8e432d189c19fe5e"
  if (md5_found != md5_expected) {
    stop("Unexpected file download, the hash does not match")
  }
  dest
}

unzip_movement <- function(file_path){
  ## Unzip folder
  # List the contents of the zip file to find the exact path
  zip_contents <- unzip(file_path, list = TRUE)
  # Find all items under the "dairy" folder
  zip_contents <- zip_contents$Name[grep("^dairy/", zip_contents$Name)]
  zip_paths <- file.path(root, "raw_data", zip_contents)
  # Check if these files exist
  files_exist <- file.exists(zip_paths)
  # Check if all files exist
  all_files_exist <- all(files_exist)

  if (all_files_exist) {
    message("All files already unzipped.")
  } else{
    message("Unzipping the downloaded data. This will take a while.")
    missing_files <- zip_contents[!files_exist]
    unzip(file_path, files = missing_files, exdir = "raw_data")
  }

}


process_movement <- function(root, herd_size_data, redownload = FALSE) {
  ## Download files
  path <- download_movement(root, redownload)
  ## Unzip files
  unzip_movement(path)

  ## Process Data
  ## Extract list of US State names from any of the 1000 posterior realisations:
  df <- read.table(file.path(root, "raw_data", "dairy/dairy_network_0.network"), header = TRUE)
  All_states <- sort(usdata::abbr2state(unique(df$oStateAbbr)))

  ## Build 48x48 arrays to hold extracted info
  Cattle_export_shipments <- array(NA, dim = c(length(All_states), length(All_states), 1000))
  Cattle_total_heads_shipped <- array(NA, dim = c(length(All_states), length(All_states), 1000))
  Cattle_mean_shipment_size <- array(NA, dim = c(length(All_states), length(All_states), 1000))
  Cattle_shipment_proportion <- array(NA, dim = c(length(All_states), length(All_states), 1000))

  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = 1000, style = 3)
  message("\nBeginning processing, this will take a while.")
  for(k in 1:1000){
    network_number <- k-1
    # Update progress bar
    setTxtProgressBar(pb, k)

    df <- read.table(sprintf(file.path(root, "raw_data", "dairy/dairy_network_%s.network"), network_number), header = TRUE)
    # We only want the farms, not feedlots or markets:
    df <- dplyr::filter(df, oPty == "Frm" & dPty == "Frm")
    df <- df[,c(4,7,8,12)]
    df$oStateAbbr <- usdata::abbr2state(df$oStateAbbr)
    df$dStateAbbr <- usdata::abbr2state(df$dStateAbbr)

    for(i in 1:length(All_states)){
      for(j in 1:length(All_states)){
        State_i <- All_states[i]
        State_j <- All_states[j]

        df_hold <- dplyr::filter(df, oStateAbbr %in% State_i)
        df_hold <- dplyr::filter(df_hold, dStateAbbr %in% State_j)
        Cattle_export_shipments[i,j,k] <- nrow(df_hold)
        # If there are no exports, then we don't factor that into the shipment size calculations
        if(nrow(df_hold) > 0) {
        Cattle_total_heads_shipped[i,j,k] <- sum(df_hold$volume)
        Cattle_mean_shipment_size[i,j,k] <- mean(df_hold$volume)
        Cattle_shipment_proportion[i,j,k] <- mean(df_hold$volume/df_hold$oBinnedSize)
        }

      }
    }
  }
  ## Close progress bar
  close(pb)

  ## Save multiple objects - this is useful for de-bugging
  save(Cattle_export_shipments, Cattle_total_heads_shipped, Cattle_mean_shipment_size, Cattle_shipment_proportion, All_states, file = file.path(root, "raw_data","Cattle_movement_data.RData"))

  Mean_Cattle_export_shipments <- apply(Cattle_export_shipments, c(1, 2), mean)
  Mean_Cattle_mean_shipment_size <- apply(Cattle_mean_shipment_size, c(1, 2), mean, na.rm = TRUE)
  Mean_Cattle_total_heads_shipped <- apply(Cattle_total_heads_shipped, c(1, 2), mean, na.rm = TRUE)
  Mean_Cattle_shipment_proportion <- apply(Cattle_shipment_proportion, c(1, 2), mean, na.rm = TRUE)

  ## Some elements are NaN, because all of their 1000 actualisations are 0. No exports from [1 -> 37], [16 -> 37], [26 -> 37]. State 37 is Rhode Island.
  ## We manually set these to 0:
  Mean_Cattle_mean_shipment_size[is.nan(Mean_Cattle_mean_shipment_size)] <- 0
  Mean_Cattle_total_heads_shipped[is.nan(Mean_Cattle_total_heads_shipped)] <- 0
  Mean_Cattle_shipment_proportion[is.nan(Mean_Cattle_shipment_proportion)] <- 0

  ## p_region_export - a vector of length N_regions
  ## The probability that a herd in region i will export some number of cattle per day.
  ## Calculated as the mean number of annual exports, divided by total number of herds in that region, divided by 365

  p_region_export <- sapply(1:48, function(i) sum(Mean_Cattle_export_shipments[i,]) / (365 * herd_size_data$n_herds_per_region[i]))

  ## p_cow_export - a vector of length N_regions
  ## The probability that, if a herd is exporting, a single head of cattle will be exported. Thus, it's the proportion of the herd that will be exported.
  ## Two ways to do this:
  ## 1) Calculated as the mean export size from that region, divided by the average herd size in that region (the total number of cows / the total number of herds)
  ## 2) We just take the average proportion export size from above.
  ## We go with option 2.

  ## Option 1
  ## First calculate the mean herd size per region:
  #mean_herd_size_per_region <- sapply(1:48, function(i) sum(herd_size_data$n_cows_per_herd[(herd_size_data$region_start[i]+1):(herd_size_data$region_start[i+1])])/herd_size_data$n_herds_per_region[i])
  #p_cow_export <- sapply(1:48, function(i) mean(Mean_Cattle_mean_shipment_size[i,]) / mean_herd_size_per_region[i])

  ## Option 2
  p_cow_export <- sapply(1:48, function(i) mean(Mean_Cattle_shipment_proportion[i,]))

  ## movement_matrix
  ## A matrix where [i,j] lists, given that there is an export from region i, what region j will it export to?
  ## This is just the Mean_Cattle_export_shipments matrix scaled so that every row sums to 1
  #Divide each row by the sum of the row to convert to probabilities
  movement_matrix <- Mean_Cattle_export_shipments
  for(i in 1:48){
    movement_matrix[i,] <- movement_matrix[i,]/sum(movement_matrix[i,])
  }

  list(p_region_export = p_region_export,
       p_cow_export = p_cow_export,
       movement_matrix = movement_matrix)
}
