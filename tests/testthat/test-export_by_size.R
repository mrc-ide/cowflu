test_that("The `export_prob_depends_on_size' parameter is working", {
  pars <- test_toy_inputs2(time_test = 1)
  pars$start_count <- 0
  #pars$export_prob_depends_on_size <- TRUE
  n_particles <- 3
  times <- 0:75


  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  s <- dust2::dust_system_simulate(sys, times)
  s_unweighted_tally <- s[1:5,,]

  ## And weighted by herd population size
  pars$export_prob_depends_on_size <- TRUE
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  ## sys$packer_state$unpack(dust2::dust_system_state(sys))
  s <- dust2::dust_system_simulate(sys, times)
  s_weighted_tally <- s[1:5,,]

for(i in 1:n_particles){
  expect_true(sum(diff(s_unweighted_tally[1,i,]) != 0) >= sum(diff(s_weighted_tally[1,i,]) != 0)  )
}

})
