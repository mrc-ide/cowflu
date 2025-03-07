test_that("The outbreak detection parameters work as expected, with births/deaths (mu parameter) included.", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75

  ## Make the probability of declaring an outbreak very high:
  pars$N_scaling <- 1e-6
  pars$strength_scaling <- 1e-6
  pars$I_scaling <- 1e-6

  pars$mu <- 1/500

  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  ## We subset only the model states, and not additionally added exported variables:
  end_of_core_states <- (pars$n_herds + pars$n_regions)*4
  s <- dust2::dust_system_simulate(sys, times)
  ## Extract number of outbreaks
  baseline_outbreaks <- s[(end_of_core_states + pars$n_herds + 1):(end_of_core_states + pars$n_herds + pars$n_regions),,]
  ## Sum over time
  baseline_outbreaks <- apply(baseline_outbreaks, c(1,2), sum)

  ## Now repeat with parameters that greatly reduce the probability of declaring an outbreak close to 0:
  pars$N_scaling <- 1e6
  pars$strength_scaling <- 1000
  pars$I_scaling <- 1e6

  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  s <- dust2::dust_system_simulate(sys, times)

  less_outbreaks <- s[(end_of_core_states + pars$n_herds + 1):(end_of_core_states + pars$n_herds + pars$n_regions),,]
  ## Sum over time
  less_outbreaks <- apply(less_outbreaks, c(1,2), sum)

  for(i in 1:3){
    for(j in 1:3){
      expect_true(less_outbreaks[i,j] <= baseline_outbreaks[i,j],
                  "More outbreaks declared despite lower probability.")
    }
  }

})
