cowflu_inputs <- function(alpha, beta, gamma, sigma, inputs) {
  c(inputs,
    list(alpha = alpha, beta = beta, gamma = gamma, sigma = sigma))
}


cowflu_fixed_inputs <- function(p_region_export, p_cow_export,
                                movement_matrix, start_region,
                                start_count = 5,
                                time_test = 30, n_test = 30,
                                n_herds_per_region = NULL,
                                n_cows_per_herd = NULL) {
  n_herds_per_region <- n_herds_per_region %||% usda_data$n_herds_per_region
  n_cows_per_herd <- n_cows_per_herd %||% usda_data$n_cows_per_herd

  n_herds <- sum(n_herds_per_region)
  n_regions <- length(n_herds_per_region)
  if (start_region < 1 || start_region > n_regions) {
    cli::cli_abort(
      "Expected 'start_region' to be in range [1, {n_herds}]")
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
  movement_matrix_cumulative <- apply(movement_matrix, 1, cumsum)

  list(n_herds = n_herds,
       n_regions = n_regions,
       region_start = region_start,
       p_region_export = p_region_export,
       p_cow_export = p_cow_export,
       n_cows_per_herd = n_cows_per_herd,
       movement_matrix = movement_matrix_cumulative,
       start_region = start_region,
       start_count = start_count,
       index = rep(seq_along(n_herds_per_region), n_herds_per_region),
       time_test = time_test,
       n_test = n_test)
}
