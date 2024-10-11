test_that("The total cow population stays the same", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  ## We subset only the model states, and not additionally added exported variables:
  end_of_core_states <- (pars$n_herds + pars$n_regions)*5
  s0 <- array_safe(dust2::dust_system_state(sys)[1:end_of_core_states,],
                   c(pars$n_herds + pars$n_regions, 5, n_particles))
  s <- dust2::dust_system_simulate(sys, times)
  ## Test only on core model states:
  s <- s[1:end_of_core_states,,]
  s1 <- array_safe(s, c(pars$n_herds + pars$n_regions, 5, n_particles, length(times)))
  s1_total <- s1[22:24,1:4 , , ] # 1:4 because the 5th element is the number of detected outbreaks.
  population_totals <- apply(s1_total, c(3, 4), sum)
  for (i in seq_len(n_particles)) {
    expect_true(length(unique(population_totals[i,])) == 1,
                sprintf("Total population in particle %s does not stay equal", i))
  }
})
##Test
