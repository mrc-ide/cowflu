test_that("Custom seeding works correctly", {
  pars <- test_toy_inputs(time_test = 1)
  pars$start_count <- 0
  pars$p_region_export <- c(0,0,0)
  pars$n_seed <- 3L
  pars$seed_time <- as.integer(c(10,20,30))
  pars$seed_herd <- as.integer(c(1, 5, 20))
  pars$seed_amount <- as.integer(c(10, 20, 30))

  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 1)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  ## We subset only the model states, and not additionally added exported variables:
  end_of_core_states <- (pars$n_herds + pars$n_regions)*4
  s <- dust2::dust_system_simulate(sys, times)
  s_tally <- s[1:end_of_core_states,,]
  s1 <- array_safe(s[1:end_of_core_states,,],
                   c(pars$n_herds + pars$n_regions, 4, n_particles, length(times)))
  s1_infections <- s1[1:pars$n_herds,3 , , ] # 1:4 because the 5th element is the number of detected outbreaks.

  for(i in 1:n_particles){
    expect_equal(s1[1,3,i,12] , 10)
    expect_equal(s1[22,3,i,12] , 10)
    expect_equal(s1[23,3,i,12] , 0)
    expect_equal(s1[24,3,i,12] , 0)

    expect_equal(s1[5,3,i,22] , 20)
    expect_equal(s1[23,3,i,22] , 20)
    expect_equal(s1[24,3,i,22] , 0)

    expect_equal(s1[20,3,i,32] , 30)
    expect_equal(s1[24,3,i,32] , 30)
  }
})

