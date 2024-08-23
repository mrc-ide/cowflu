test_that("Identity movement matrix is observed", {
  pars <- test_toy_inputs(movement_matrix = cbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)))
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  s <- dust2::dust_system_simulate(sys, times)
  s1 <- array_safe(s, c(pars$n_herds + pars$n_regions, 5, n_particles, length(times)))

  s1_total <- s1[22:24, , , ]
  s1_region_totals <- apply(s1_total, c(1, 3, 4), sum)

  ## Define the dimensions of s1_region_totals
  dims <- dim(s1_region_totals)
  ## Loop over the first (region) and second (particle) dimensions
  for (x in 1:dims[1]) {
    for (y in 1:dims[2]) {
      # Extract the vector for the current (x, y)
      slice_vector <- s1_region_totals[x, y, ]
      # Check if all elements in the slice_vector are identical
      expect_true(length(unique(slice_vector)) == 1,
                  info = sprintf("Values in slice s1_region_totals[%d, %d, ] are not identical", x, y))
    }
  }
})
