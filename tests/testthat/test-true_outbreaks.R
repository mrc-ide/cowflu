test_that("The `true outbreaks' tally is calculated correctly", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  ## We subset only the model states, and not additionally added exported variables:
  end_of_core_states <- (pars$n_herds + pars$n_regions)*5
  s <- dust2::dust_system_simulate(sys, times)
  s_tally <- s[(end_of_core_states+1):(end_of_core_states+pars$n_regions),,]
  s1 <- array_safe(s[1:end_of_core_states,,],
                   c(pars$n_herds + pars$n_regions, 5, n_particles, length(times)))
  s1_infections <- s1[1:pars$n_herds,3 , , ] # 1:4 because the 5th element is the number of detected outbreaks.
  manual_infection_tally <- array(0,
                                  c(pars$n_regions, n_particles, length(times)))
  for(i in seq_len(n_particles)){
    for(j in seq_len(pars$n_regions)){
      for(k in 1:length(times)){
        manual_infection_tally[j,i,k] <- sum(s1_infections[(pars$region_start[j]+1):(pars$region_start[j+1]),i,k]>0)
      }
    }
  }

  expect_equal(manual_infection_tally, s_tally)
})
