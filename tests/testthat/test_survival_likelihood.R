test_that("The outbreaks (survival analysis) likelihood is calculated correctly", {
  pars <- test_toy_inputs(time_test = 1)
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)

  ## We will simulate one timestep at a time, logging likelihood at each step.
  ## For our data, we assume that all three regions detect their first outbreak on week 50.
  sys_ll <- c(0,0,0)
  manual_ll <- c(0,0,0)
  outbreaks_detected <- array(data = 1e-6, dim = c(3,3))

  for(i in times){
    s <- dust2::dust_system_simulate(sys, i)
    end_of_core_states <- (pars$n_herds + pars$n_regions)*5
    s1 <- array_safe(s[1:end_of_core_states,,], c(pars$n_herds + pars$n_regions, 5, n_particles))
    s1_total_outbreaks <- s1[22:24,5 , ] # The 5th element is the number of detected outbreaks.

    for(j in 1:length(outbreaks_detected)){
      if(s1_total_outbreaks[j] == 0){
        s1_total_outbreaks[j] <- 1e-6
      } else{
        s1_total_outbreaks[j] <- 1 - 1e-6
      }

     if(outbreaks_detected[j] == 1e-6){
       outbreaks_detected[j] <- s1_total_outbreaks[j]
     }
    }

    if( i < 50){
      toy_data <- c(0, 0, 0)
    }else{
      toy_data <- c(1, 1, 1)
    }

    sys_ll <- sys_ll + dust2::dust_system_compare_data(sys, list(outbreak_detected = toy_data))
    manual_ll <- manual_ll + colSums(dbinom(x = toy_data, size = 1, prob = outbreaks_detected, log = TRUE))
  }

    expect_equal(manual_ll,
                 sys_ll)

})
