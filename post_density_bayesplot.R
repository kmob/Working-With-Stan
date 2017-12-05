# density plot using bayesplot

post_density_bayesplot <- function(post_data, parameter_name, uncertainty_level){
  library("bayesplot")
  library(rstan)
  
  plot_title <- ggtitle("Posterior distribution using mcmc_areas",
                        paste("with medians and", uncertainty_level, "intervals."))
  # TODO: needs axis labels
  
  # mcmc_areas: Density plots computed from posterior draws with all chains merged, 
  # with uncertainty intervals shown as shaded areas under the curves.
  posterior <- as.matrix(post_data)
  graph_out <- mcmc_areas(posterior, 
             pars = c(parameter_name), 
             prob = uncertainty_level) + plot_title
}
