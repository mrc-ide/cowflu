cowflu_inputs <- function(alpha, beta, gamma, sigma, asc_rate, dispersion, inputs) {
  c(inputs,
    list(alpha = alpha, beta = beta, gamma = gamma, asc_rate = asc_rate, sigma = sigma, dispersion = dispersion))
}


## p_region_export is the *daily* probability of export in the args to
## this function, but we will convert it into weekly.  Probably the
## best place to do this is in processing the movement data?
cowflu_fixed_inputs <- function(p_region_export, p_cow_export,
                                movement_matrix, start_herd = 26940, #26940 is an average-sized herd in Texas
                                start_count = 5,
                                time_test = 136, n_test = 30,
                                condition_on_export = TRUE,
                                likelihood_choice = "incidence",
                                n_herds_per_region = NULL,
                                n_cows_per_herd = NULL,
                                n_seed = NULL,
                                seed_time = NULL,
                                seed_herd = NULL,
                                seed_amount = NULL) {
  n_herds_per_region <- n_herds_per_region %||% usda_data$n_herds_per_region
  n_cows_per_herd <- n_cows_per_herd %||% usda_data$n_cows_per_herd

  n_seed <- n_seed %||% 2L
  seed_time <- seed_time %||% c(10000L,10000L)
  seed_herd <- seed_herd %||% c(1L,1L)
  seed_amount <- seed_amount %||% c(0L,0L)

  n_herds <- sum(n_herds_per_region)
  n_regions <- length(n_herds_per_region)
  if (start_herd < 1 || start_herd > n_herds) {
    cli::cli_abort(
      "Expected 'start_herd' to be in range [1, {n_herds}]")
  }
  if (start_count < 1 || start_count > n_cows_per_herd[start_herd]){
    cli::cli_abort(
      "Expected 'start_count' to be in range [1, {n_cows_per_herd[start_herd]}]")
  }
  region_start <- as.integer(c(0, cumsum(n_herds_per_region)))
  if (length(p_region_export) != n_regions) {
    cli::cli_abort(
      "Expected 'p_region_export' to have length {n_regions}")
  }
  if (length(p_cow_export) != n_regions) {
    cli::cli_abort(
      "Expected 'p_cow_export' to have length {n_regions}")
  }
  if (!is.matrix(movement_matrix)) {
    cli::cli_abort(
      "Expected 'movement_matrix' to be a matrix")
  }
  if (!all(dim(movement_matrix) == c(n_regions, n_regions))) {
    cli::cli_abort(
      "Expected 'movement_matrix' to be a {n_regions} x {n_regions} matrix")
  }
  err <- abs(rowSums(movement_matrix) - 1) > 1e-8
  if (any(err)) {
    cli::cli_abort(
      "Expected rows of 'movement_matrix' to sum to 1",
      i = "Check rows {which(err)}")
  }
  if (! is.logical(condition_on_export)) {
    cli::cli_abort(
      "Expected 'condition_on_export' to be TRUE or FALSE")
  }
  if (! likelihood_choice %in% c("incidence", "survival")) {
    cli::cli_abort(
      "Expected 'likelihood choice' to be 'incidence' or 'survival'")
  }
  ## This line will transpose the matrix, and change the original rows (now columns) to a cumulative sum.
  movement_matrix_cumulative <- apply(movement_matrix, 1, cumsum)

  ## Check custom seedings:
  if(length(seed_time) != n_seed){
    cli::cli_abort(
      "Expected 'seed_time' to be of length 'n_seed'")
  }
  if(length(seed_herd) != n_seed){
    cli::cli_abort(
      "Expected 'seed_herd' to be of length 'n_seed'")
  }
  if(length(seed_amount) != n_seed){
    cli::cli_abort(
      "Expected 'seed_amount' to be of length 'n_seed'")
  }
  if(any(seed_herd > n_herds)){
    cli::cli_abort(
      "All 'seed_herd' values must be less than 'n_herds'")
  }


  list(n_herds = n_herds,
       n_regions = n_regions,
       n_herds_per_region = n_herds_per_region,
       region_start = region_start,
       p_region_export = p_region_export, #1 - exp(-p_region_export * 7), # or 1 - p_region_export^7, but that has a different error
       p_cow_export = p_cow_export,
       n_cows_per_herd = n_cows_per_herd,
       movement_matrix = movement_matrix_cumulative,
       start_herd = start_herd,
       start_count = start_count,
       index = rep(seq_along(n_herds_per_region), n_herds_per_region),
       time_test = time_test,
       n_test = n_test,
       condition_on_export = condition_on_export,
       likelihood_choice = likelihood_choice,
       n_seed = n_seed,
       seed_time = seed_time,
       seed_herd = seed_herd,
       seed_amount = seed_amount)
}
