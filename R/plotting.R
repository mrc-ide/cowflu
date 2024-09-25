plot_chains_ll <- function(samples) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  n_pars <- dim(samples$par)[1]
  n_samples <- dim(samples$par)[2]
  n_chains <- dim(samples$par)[3]

  plot_data <- data.frame( density = as.vector(samples$density),
                           index = rep(1:n_samples, n_chains),
                           chain = rep(1:n_chains, each = n_samples))

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = index, y = density, color = factor(chain)), size = 1) +
    ggplot2::labs(title = "Log-likelihood density by chain",
         x = "Sample index",
         y = "Log-likelihood",
         color = "Chain") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14),
          legend.title = ggplot2::element_text(size = 14),
          plot.title = ggplot2::element_text(size = 16, face = "bold"))

}

plot_param_traj <- function(samples, one_panel = FALSE){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  n_pars <- dim(samples$par)[1]
  n_samples <- dim(samples$par)[2]
  n_chains <- dim(samples$par)[3]
  param_names <- row.names(example_samples$pars)
  plot_list <- list()

  for(i in 1:length(param_names)){
    hold_param_name <- param_names[i]
    hold_samples <- samples$pars[i,,]
    plot_data <- data.frame( value = as.vector(hold_samples),
                             index = rep(1:n_samples, n_chains),
                             chain = rep(1:n_chains, each = n_samples))

    ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x = index, y = value, color = factor(chain)), linewidth = 1) +
      ggplot2::labs(title = sprintf("Posterior samples for %s", hold_param_name),
                    x = "Sample index",
                    y = "Value",
                    color = "Chain") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     axis.title = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 14),
                     plot.title = ggplot2::element_text(size = 16, face = "bold")) -> plot

    plot_list[[i]] <- plot
  }
  if(one_panel){
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop(
        "Package \"gridExtra\" must be installed to return all trajectories on one panel.",
        call. = FALSE
      )
    }
    one_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = 2)
    one_plot
  }else{
    plot_list
  }
}

plot_param_posterior <- function(samples, one_panel = FALSE){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  n_pars <- dim(samples$par)[1]
  n_samples <- dim(samples$par)[2]
  n_chains <- dim(samples$par)[3]
  param_names <- row.names(example_samples$pars)
  plot_list <- list()

  for(i in 1:length(param_names)){
    hold_param_name <- param_names[i]
    hold_samples <- samples$pars[i,,]
    plot_data <- data.frame( value = as.vector(hold_samples),
                             index = rep(1:n_samples, n_chains),
                             chain = rep(1:n_chains, each = n_samples))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = value, group = factor(chain), color = factor(chain)) ) +
      # Faint histogram in the background
      ggplot2::geom_histogram(ggplot2::aes(y = ..density.., fill = factor(chain) ), color = NA, position = "identity", alpha = 0.3, bins = 30) +
      # Density plot colored by "chain"
      ggplot2::geom_density(size = 1) +
      ggplot2::labs(title = sprintf("Posterior density for %s", hold_param_name),
                    x = "Value",
                    y = "Density",
                    color = "Chain",
                    fill = "Chain") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     axis.title = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 14),
                     plot.title = ggplot2::element_text(size = 16, face = "bold")) -> plot

    plot_list[[i]] <- plot
  }
  if(one_panel){
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop(
        "Package \"gridExtra\" must be installed to return all trajectories on one panel.",
        call. = FALSE
      )
    }

    one_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = 2)
    one_plot
  }else{
    plot_list
  }
}
