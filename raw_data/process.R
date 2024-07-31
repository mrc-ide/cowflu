root <- here::here()
source(file.path(root, "raw_data", "movement.R"))
source(file.path(root, "raw_data", "herd_sizes.R"))

usda_data <- real_dairy_populations(root)
movement <- process_movement(root, usda_data)

save(list = c("usda_data", "movement"),
     file = file.path(root, "R/sysdata.rda"),
     version = 2)
message("Wrote package data")
