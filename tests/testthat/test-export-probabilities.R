test_that("The `probability of passing export test' model variable is calculated correctly", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  ## We subset only the model states, and not additionally added exported variables:
  end_of_core_states <- (pars$n_herds + pars$n_regions)*5
  s <- dust2::dust_system_simulate(sys, times)
  s_probabilities <- s[(end_of_core_states+pars$n_regions+1):(end_of_core_states+pars$n_regions+pars$n_regions),,]
  ## Manually calculate the probabilities:
  s1 <- array_safe(s[1:end_of_core_states,,],
                   c(pars$n_herds + pars$n_regions, 5, n_particles, length(times)))
  s1_infections <- s1[1:pars$n_herds,3 , , ] # 1:4 because the 5th element is the number of detected outbreaks.
  manual_prob_pass <- array(0,
                                  c(pars$n_regions, n_particles, length(times)))
  herds_in_region <- cumsum(c(0,pars$n_herds_per_region))
  for(i in seq_len(n_particles)){
    for(j in seq_len(pars$n_regions)){
      for(k in 1:length(times)){
        probability_tally <- 0
        for(l in (herds_in_region[j] + 1):herds_in_region[j+1]){
          probability_tally <- probability_tally + dhyper(0,
                                                          round(s1_infections[l,i,k]*pars$p_cow_export[j]),
                                                          round(sum(s1[l,c(1,2,4),i,k])*pars$p_cow_export[j]),
                                                          min(pars$n_test, round(sum(s1[l,c(1,2,4),i,k])*pars$p_cow_export[j]) + round(s1_infections[l,i,k]*pars$p_cow_export[j])   )
                                                          )
        }
        manual_prob_pass[j,i,k] <- probability_tally/pars$n_herds_per_region[j]
      }
    }
  }


  expect_equal(manual_prob_pass, s_probabilities)
})
