test_that("basic epi dynamics are reasonable", {
  pars <- test_toy_inputs()
  n_particles <- 3
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
  dust2::dust_system_set_state_initial(sys)
  s0 <- array(dust2::dust_system_state(sys),
              c(pars$n_herds + pars$n_regions, 4))
  s <- dust2::dust_system_simulate(sys, times)
  s1 <- array(s, c(pars$n_herds + pars$n_regions, 4, n_particles, length(times)))

  s1_herds <- s1[-(22:24), , , ]
  s1_total <- s1[22:24, , , ]

  tot <- apply(s1_herds, 2:4, sum)
  expect_true(all(diff(t(tot[1, , ])) <= 0)) # S decreases
  expect_true(all(diff(t(tot[4, , ])) >= 0)) # R increases

  i1 <- 1:3
  i2 <- 4:10
  i3 <- 11:21
  expect_equal(apply(s1_herds[i1, , , ], 2:4, sum),
               s1_total[1, , , ])
  expect_equal(apply(s1_herds[i2, , , ], 2:4, sum),
               s1_total[2, , , ])
  expect_equal(apply(s1_herds[i3, , , ], 2:4, sum),
               s1_total[3, , , ])
})
