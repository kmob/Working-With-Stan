# density plot using bayesplot

post_density_bayesplot <- function(post_data, parameter_name, uncertainty_level, point_est){
  library("bayesplot")
  library(rstan)
  
  # TODO: needs axis labels
  g_title <- c("Posterior distribution using mcmc_areas")
  g_subtitle <- paste("For", point_est, "and", uncertainty_level, "uncertainty interval.")
  # mcmc_areas: Density plots computed from posterior draws with all chains merged, 
  # with uncertainty intervals shown as shaded areas under the curves.
  posterior <- as.matrix(post_data)
  graph_out <-  mcmc_areas(posterior, 
                           pars = c(parameter_name),
                           prob = uncertainty_level,
                           point_est = point_est) +
    labs(title = g_title, subtitle = g_subtitle) +
    xlim(0, 1)
}
