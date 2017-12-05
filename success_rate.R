# Setup
rm(list=ls())
library(rstan)
library(bayesplot)

library(dplyr)
# library(DiagrammeR)

# Data
data <- read_rdump("data.R") 

# Stan needs data in a list
data_list <- list(n = length(data$n),
             k = length(subset(data$n, data$n == 1)))

# Visualize Data
source("bernoulli_outcomes_ggplot.R")
graph <- bernoulli_outcomes_ggplot(data)
graph

# Visualize Model
source("gm_binary_process.r")
gm_binary_process()

# Parameters
# inits via list:
#   Set inital values by providing a list equal in length to the 
#   number of chains. The elements of this list should themselves 
#   be named lists, where each of these named lists has the name 
#   of a parameter and is used to specify the initial values for 
#   that parameter for the corresponding chain.
myinits <- list(
  list(theta=.1),  # chain 1 starts at .1
  list(theta=.9))  # chain 2 starts at .9
parameters <- c("theta")

# Run Stan
samples <- stan(file="binomial_uniform_prior.stan",   
                data=data_list, 
                init=myinits,  # If not specified, gives random inits
                pars=parameters,
                iter=20000, 
                chains=2, 
                thin=1
                # warmup = 100,  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)

# Analysis Output
# The commands below are useful for a quick overview:
print(samples)  # a rough summary
print(summary(samples))  # more detailed summary

# Bayesplot: Look at rhat
rhats <- rhat(samples)
color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats) + yaxis_text(hjust = 1)

# Bayesplot: Look at effective sample size
ratios_cp <- neff_ratio(samples)
print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

# Bayesplot: Autocorrelation - centered parameterization (CP)
posterior_cp <- as.array(samples)
mcmc_acf(posterior_cp, pars = "theta", lags = 10)

# Bayesplot: Evaluate NUTS sampler
lp_cp <- log_posterior(samples)
head(lp_cp)

np_cp <- nuts_params(samples)
head(np_cp)

color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_cp, pars = "theta", np = np_cp) +
  xlab("Post-warmup iteration")

color_scheme_set("red")
mcmc_nuts_divergence(np_cp, lp_cp)

# Stan_plot: Visual posterior analysis using ggplot2.
# plot function given an object of class stanfit
plot(samples) 
# use stan_plot with piping to specify ggplot attributes
est_plot <- samples %>% 
  stan_plot(point_est = "mean", 
            ci_level = 0.8, 
            outer_level = 0.95) + 
  xlab("X label")

est_plot

##### STOPPED HERE #####

  #point estimate and variability 2 levels
stan_trace(samples) # traceplot shows
stan_hist(samples)
stan_dens(samples)
stan_scat(samples)
stan_diag(samples)
stan_rhat(samples)
stan_ess(samples)
stan_mcse(samples)
stan_ac(samples)

traceplot(samples) # traceplot shows 

as.array(samples)[1:15,,2]  # array: sample, chain, parameter 
# where parameter 1 is "theta"
# and parameter 2 is "lp__" - the log posterior density 
# convergence of log posterior density is critical to declaring convergence
# stan-reference-2.16.0.pdf pg 368-369

# Collect posterior and prior samples across all chains:
theta <- rstan::extract(samples)$theta

# Visualize Predictions
###### Visualization using r's plot function ######
source("post_density_plot.r")
post_density_plot(theta, 80)

###### USING BAYESPLOT TO PRODUCE A TRACE ##########
source("sample_trace_bayesplot.r")
graph <- sample_trace_bayesplot(samples, "theta")
graph 

###### USING BAYESPLOT TO PRODUCE A HISTOGRAM ##########
source("post_density_bayesplot.r")
graph <- post_density_bayesplot(samples, "theta", 0.8)
graph 

######## USING GGPLOT2 TO PRODUCE A HISTOGRAM #########
source("post_density_ggplot.r")
graph <- post_density_ggplot(samples)
graph

######## USING METRICSGRAPHICS JS #####################
# http://hrbrmstr.github.io/metricsgraphics/
# https://github.com/mozilla/metrics-graphics
source("post_density_mjs.r")
graph <- post_density_mjs(samples, 80)
graph
