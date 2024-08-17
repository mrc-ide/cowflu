## Toy example for debugging.
pkgload::load_all()

## Set parameters
pars <- cowflu_inputs(
  alpha = 0.2, #The rate of herd-to-herd infection contacts within each region.
  beta = 0.9,  #The transmission rate
  gamma = 0.1, #The incubation rate
  sigma = 0.125,  #The recovery rate
  cowflu_fixed_inputs(
    n_herds_per_region = c(3, 7, 11),
    p_region_export = c(.2, .2, .2),
    p_cow_export = c(0.2, 0.2, 0.2),
    n_cows_per_herd = c(rep(200, 3), rep(1000, 7), rep(3000, 11)),
    #Note, we say that NO trades go to region 1
    #movement_matrix = cbind(c(.6, .2, .2), c(.2, .6, .2), c(.2, .2, .6)),
    movement_matrix = cbind(c(0,0,0), c(0.8,0.8,0.4), c(.2,.2,.6)),
    time_test = 10000,
    start_herd = 5,
    start_count = 5,
    condition_on_export = TRUE))

set.seed(1)
n_particles <- 1

## Initialise the system:
sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
dust2::dust_system_set_state_initial(sys)

## Export JUST the region total:
i <- seq.int(pars$n_herds + 1, length.out = pars$n_regions)
i <- c(outer(i, (pars$n_herds + pars$n_regions) * (0:3), "+"))

s <- dust2::dust_system_simulate(sys, 0:100, i) #Remember to reinitialise sys!
s2 <- array(s, c(pars$n_regions, 4, n_particles, 101))

## Expected daily exports to region 1...
sum((pars$p_region_export*c(3,7,11))*pars$movement_matrix[,1])

plot(s2[1,1,1,])
