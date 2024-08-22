test_that("The total cow population stays the same", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  s0 <- array(dust2::dust_system_state(sys),
              c(pars$n_herds + pars$n_regions, 4))
  s <- dust2::dust_system_simulate(sys, times)
  s1 <- array(s, c(pars$n_herds + pars$n_regions, 4, n_particles, length(times)))

  s1_total <- s1[22:24, , , ]
  population_totals <- apply(s1_total, c(3, 4), sum)
  for(i in 1:n_particles){
    expect_true(length(unique(population_totals[i,])) == 1,
                info = sprintf("Total population in particle %s does not stay equal", i))
  }
  })
