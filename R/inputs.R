cowflu_fixed_inputs <- function(n_herds, p_region_export, p_cow_export,
                                movement_matrix, time_test = 30, n_test = 30) {
  n_regions <- length(n_herds)
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
  list(n_herds = n_herds,
       p_region_export = p_region_export,
       p_cow_export = p_cow_export,
       movement_matrix = t(movement_matrix),
       time_test = time_test,
       n_test = n_test)
}
