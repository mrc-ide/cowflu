root <- here::here()
source(file.path(root, "raw_data", "movement.R"))
source(file.path(root, "raw_data", "herd_sizes.R"))
source(file.path(root, "raw_data", "outbreaks.R"))
#source(file.path(root, "raw_data", "icvi_movement.R"))

usda_data <- real_dairy_populations(root)
movement <- process_movement(root, usda_data)
outbreaks_data <- process_outbreak_data(root, usda_data)
#icvi_movement <- process_icvi_movement(root, usda_data)

save(list = c("usda_data", "movement", "outbreaks_data"),
     file = file.path(root, "R/sysdata.rda"),
     version = 2)
message("Wrote package data")
