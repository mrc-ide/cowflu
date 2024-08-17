## Identifying movement matrix bug
pkgload::load_all()

## Demonstrating the bug.
## We run the "full" US model for now. Set parameters:
pars <- cowflu_inputs(alpha = 0.05, beta = 0.35, gamma = 0.2, sigma = 0.5,
                      cowflu_fixed_inputs(p_region_export = movement$p_region_export,
                                          p_cow_export = movement$p_cow_export,
                                          movement_matrix = movement$movement_matrix,
                                          #movement_matrix = test_matrix,
                                          time_test = 136,
                                          #time_test = 1360,
                                          n_herds_per_region = usda_data$n_herds_per_region,
                                          n_cows_per_herd = usda_data$n_cows_per_herd,
                                          start_herd = 26940, #26804 is where Texas starts.
                                          start_count = 5
                      ))

pars$condition_on_export <- TRUE

set.seed(1)
n_particles <- 2
sys <- dust2::dust_system_create(cows(), pars, n_particles = n_particles, dt = 0.25)
dust2::dust_system_set_state_initial(sys)

## Request back only the region totals
i <- seq.int(pars$n_herds + 1, length.out = pars$n_regions)
i <- c(outer(i, (pars$n_herds + pars$n_regions) * (0:3), "+"))
## Run the model
s <- dust2::dust_system_simulate(sys, 0:250, i)
s2 <- array(s, c(pars$n_regions, 4, n_particles, 251))

## Something strange. Look at the susceptibles over time for region 1, Alabama (very small)
plot(s2[1,1,1,],
     main = "Susceptible cows in region 1, Alabama",
     ylab = "S Cows",
     xlab = "Time")
## There's a trade coming in almost every day.
plot(s2[1,1,1,1:30])

## This is how many trades into Alabama we would EXPECT to see each timestep:
sum((movement$p_region_export*usda_data$n_herds_per_region)*movement$movement_matrix[,1])
## 0.12! i.e. roughly one every 10 days.

## Additionally, another problem, let's sum together S, E, I, R for ALL 48 regions
## Check that S isn't just going up and up and up.
## Make a new array that sums over the first dimension of s2:
s3 <- apply(s2, c(2,3,4), sum)
s4 <- apply(s3, c(2,3), sum)
plot(s4[1,],
     main = "Total Cows in the model",
     xlab = "Time",
     ylab = "Cows")  #pick particle 1

## We appear to be LOSING cows. This is not good.
## Trouble seems to start soon after the point where testing begins, t = 136

## We can demonstrate where something is clearly going wrong:

## Alter the movement matrix so that the first column is set to 0, with it's previous values moved to the fourth column, so that it still sums to 1.
test_matrix <- movement$movement_matrix
test_matrix[,4] <- test_matrix[,4] + test_matrix[,1]
test_matrix[,1] <- 0
rowSums(test_matrix)

## This now says that any trade, leaving from any state, will NEVER pick Alabama as a destination. And yet, if we run the above model again, but with test_matrix used for pars$movement_matrix, the problem persists.

## Additionally, this second bug, of losing cows after the testing date starts, disappears when we set pars$time_test to 1e6 or something silly.
