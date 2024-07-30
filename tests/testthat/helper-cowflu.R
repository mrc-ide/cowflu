test_fixed_inputs <- function() {
  inputs <- readRDS("inputs.rds")
  cowflu_fixed_inputs(inputs$data$p_region_export,
                      inputs$data$p_cow_export,
                      inputs$movement_matrix,
                      which(inputs$data$name == "Texas"))
}
