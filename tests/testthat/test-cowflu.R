test_that("basic epi dynamics are reasonable", {
  pars <- test_toy_inputs()
  n_particles <- 5
  times <- 0:75
  sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25, seed = 42)
  dust2::dust_system_set_state_initial(sys)
  s <- dust2::dust_system_simulate(sys, times)
  s1 <- array_safe(s, c(pars$n_herds + pars$n_regions, 5, n_particles, length(times)))

  s1_herds <- s1[-(22:24), 1:4, , ]
  s1_total <- s1[22:24, 1:4, , ]

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

  ## Test outbreaks behaviour:
  s1_outbreak <- s1[, 5, , ]
  s1_outbreak_herds <- s1[-(22:24), 5, , ]
  s1_outbreak_total <- s1[22:24, 5, , ]

  # Check all herds outbreaks are either 0 or 1
  expect_true(all(s1_outbreak_herds %in% c(0,1)))

  # Check number of outbreaks in states is correct
  expect_equal(sum(s1_outbreak_herds[i1, , length(times) ] > 0) ,
               sum(s1_outbreak_total[1,,]))
  expect_equal(sum(s1_outbreak_herds[i2, , length(times) ] > 0) ,
               sum(s1_outbreak_total[2,,]))
  expect_equal(sum(s1_outbreak_herds[i3, , length(times) ] > 0) ,
               sum(s1_outbreak_total[3,,]))


  y <- sys$packer_state$unpack(s)
  y$outbreak_region
})
