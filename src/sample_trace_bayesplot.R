# trace using bayesplot

sample_trace_bayesplot <- function(fit, parameter_name){
  library("bayesplot")
  library(rstan)
  
  sample <- rstan::extract(fit, inc_warmup = TRUE, permuted = FALSE)
  
  color_scheme_set("mix-blue-pink")
  p <- mcmc_trace(sample,  pars = parameter_name, n_warmup = 300,
                  facet_args = list(nrow = 2, labeller = label_parsed))
  p + facet_text(size = 15)
}