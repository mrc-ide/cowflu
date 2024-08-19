## Simple toy example, symmetric migration with three regions
test_toy_inputs <- function(alpha = 0.2, beta = 0.9, gamma = 0.1,
                            sigma = 0.125, start_count = 5,
                            movement_matrix = cbind(c(.6, .2, .2), c(.2, .6, .2), c(.2, .2, .6)),
                            time_test = 10000) {
  cowflu_inputs(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    sigma = sigma,
    cowflu_fixed_inputs(
      n_herds_per_region = c(3, 7, 11),
      p_region_export = c(.5, .5, .5),
      p_cow_export = c(0.2, 0.2, 0.2),
      n_cows_per_herd = c(rep(200, 3), rep(1000, 7), rep(3000, 11)),
      movement_matrix = movement_matrix,
      time_test = time_test,
      start_herd = 4,
      start_count = start_count))
}
