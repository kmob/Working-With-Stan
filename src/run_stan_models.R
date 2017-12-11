# Setup
rm(list=ls())
library(rstan)
library(bayesplot)
library(dplyr)
library(feather)

#### Environment ####
code_dir <- "src/"
data_dir <- "./data/"
model_dir <- "model/"

#### Helper Functions ####
source(paste(code_dir,"sim_data.r",sep = ""))
source(paste(code_dir,"post_density_bayesplot.r",sep = ""))

##### What to run ####
## how many observations per group
obs_ct <- 10
## one or two processes and the simulated process rates
### Data for one process
# data_name <- "one_process"
# rate_list <- c(0.6)
### Data for two processes
data_name <- "two_process"
rate_list <- c(0.6, 0.5)

## current models are: 
# one_rate (Graphical Model with one process)
# common_rate (Graphical Model with one process and two outcome groups)
# two_rates (Graphical Model with two processes and a difference process)
binomial_model <- "two_rates"

###################################################################

##### Simulate Data  #####
data_file <- paste(data_dir, data_name, sep = "")
## "fixed" generates a set number of successes
## "random" uses rbinom to generate data
sim_data(data_file, obs_ct, rate_list, "fixed")

# Read Data
data <- read_feather(paste(data_file, ".feather", sep = ""))

# Stan needs data in a list
if (data_name == "one_process") {
  success_data <- data %>%
    dplyr::filter(outcome == 'success')
  stan_data <- list(
    n = nrow(data),
    k = nrow(success_data))
} else if (data_name == "two_process") {
  # Stan needs data in a list
  success_k1 <- data %>%
    filter(outcome == 'success') %>%
    filter(process == 'theta_1')
  success_k2 <- data %>%
    filter(outcome == 'success') %>%
    filter(process == 'theta_2')
  stan_data <- list(
    n1 = nrow(data)/length(rate_list),
    k1 = nrow(success_k1),
    n2 = nrow(data)/length(rate_list),
    k2 = nrow(success_k2))
}

# Visualize Data
ggplot(data, aes(x = process)) + geom_bar(aes(fill=outcome))

#### Model & Parameters ####

if (binomial_model == "one_rate") {
  model_code <- paste(model_dir, "binomial_one_rate.stan", sep = "")
  source(paste(code_dir,"gm_binary_process.r",sep = ""))
  graphical_model <- gm_binary_process()
  parameters <- c("theta")
} else if (binomial_model == "common_rate") {
  model_code <- paste(model_dir, "binomial_common_rates.stan", sep = "")
  source(paste(code_dir,"gm_common_rates.r",sep = ""))
  graphical_model <- gm_common_rates()
  parameters <- c("theta")
} else if (binomial_model == "two_rates") {
  model_code <- paste(model_dir, "binomial_two_rates.stan", sep = "")
  source(paste(code_dir,"gm_two_rates.r",sep = ""))
  graphical_model <- gm_two_rates()
  parameters <- c("theta1", "theta2", "delta")
}

graphical_model

#### STAN ####

## Run Stan with defaults
samples <- stan(file=model_code,
                data=stan_data)

## Run Stan with custom setings
# myinits <- list(
#   list(theta=.1),  # chain 1 starts at .1
#   list(theta=.9))  # chain 2 starts at .9
# samples <- stan(file=model_code,   
#                        data=stan_data, 
#                        init=myinits,  # If not specified, gives random inits
#                        pars=parameters,
#                        iter=20000, 
#                        chains=2, 
#                        thin=1
#                        # warmup = 100,  # Stands for burn-in; Default = iter/2
#                        # seed = 123  # Setting seed; Default is random seed
# )

##### STAN OUTPUT ####
# Info from a S4 object of class "stanfit"
# https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
# get_stancode extracts the model
stan_model <- get_stancode(samples)
cat(stan_model)

# print shows summary of parameter and log-posterior (lp__) 
# posterior mean, posterior standard deviation, and quantiles
# Monte Carlo standard error (se_mean)
# effective sample size (n_eff)
# R-hat statistic (Rhat)
print(samples)  # a rough summary

#### Bayesplot ####
# http://mc-stan.org/bayesplot/
# neff_ratio - Look at effective sample size
# Draws in a Markov chain are not independent if there is autocorrelation. 
# If there is autocorrelation, the effective sample size will be smaller 
# than the total sample size, N. 
# The larger the ratio of neff to N the better.
ratios_cp <- neff_ratio(samples)
print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

# rhat - potential scale reduction statistic
# If chains are at equilibrium, rhat will be 1. 
# If the chains have not converged rhat will be greater than one.
rhats <- rhat(samples)
color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(rhats) + yaxis_text(hjust = 1)

# mcmc_acf - Autocorrelation - centered parameterization (CP)
# View autocorrelation for each Markov chain separately up to a specified number of lags.
# Lag - the distance between successive samples.
# The autocorrelation function (ACF) relates correlation and lag. 
# The values of the ACF should quickly decrease with increasing lag.
# ACFs that do not decrease quickly with lag often indicate that the 
# sampler is not exploring the posterior distribution efficiently and 
# result in increased R^ values and decreased Neff values. 
# https://my.vanderbilt.edu/jeffannis/files/2016/06/AnnisMillerPalmeri2016.pdf
posterior_cp <- as.array(samples)
mcmc_acf(posterior_cp, pars = parameters, lags = 10)

# Evaluate NUTS sampler
# log posterior over iterations
lp_cp <- log_posterior(samples)
head(lp_cp)
# find iterations with divergence 
np_cp <- nuts_params(samples)
head(np_cp)

# trace plot of MCMC draws and divergence, if any, for NUTS.
color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_cp, pars = parameters, np = np_cp) +
  xlab("Post-warmup iteration")

# further info on divergence
# Divergences often indicate that some part of the 
# posterior isn’t being explored. 
color_scheme_set("red")
mcmc_nuts_divergence(np_cp, lp_cp)

#### Stan_plot ####
# Visual posterior analysis based on ggplot2.
# basic plot function for an object of class stanfit
plot(samples) 
# use stan_plot with piping to specify ggplot attributes
point_est <- "mean"
uncertainty_interval <- 0.8
outer_uncertainty_interval <- 0.95
g_title <- paste("Plot of", point_est, "for parameters:", parameters)
g_subtitle <- paste("Uncertainty intervals of", 
                    uncertainty_interval,
                    "and",
                    outer_uncertainty_interval)
graph <- samples %>% 
  stan_plot(point_est = "mean", 
            ci_level = uncertainty_interval, 
            outer_level = outer_uncertainty_interval) + 
  labs(title = g_title, 
       subtitle = g_subtitle)

graph

##### Other Diagnostics & Graphs #####

## more detailed summary
print(summary(samples))  

#point estimate and variability 2 levels
stan_trace(samples)
stan_hist(samples)
stan_dens(samples)
stan_scat(samples)
stan_diag(samples)
stan_rhat(samples)
stan_ess(samples)
stan_mcse(samples)
stan_ac(samples)

as.array(samples)[1:15,,2]  # array: sample, chain, parameter 
# where parameter 1 is "theta"
# and parameter 2 is "lp__" - the log posterior density 
# convergence of log posterior density is critical to declaring convergence
# stan-reference-2.16.0.pdf pg 368-369

# Collect posterior and prior samples across all chains:
theta <- rstan::extract(samples)$theta

# Visualize Predictions
###### USING BAYESPLOT TO PRODUCE A TRACE ##########
source(paste(code_dir,"sample_trace_bayesplot.r",sep = ""))
graph <- sample_trace_bayesplot(samples, parameters)
graph 

###### USING BAYESPLOT TO PRODUCE A HISTOGRAM ##########
source(paste(code_dir,"post_density_bayesplot.r",sep = ""))
graph <- post_density_bayesplot(samples, parameters, 0.8, "mean")
graph 

# ----GRAPHS BELOW ONLY WORK FOR ONE PROCESS-----
# ###### Visualization using r's plot function ######
# source(paste(code_dir,"post_density_plot.r",sep = ""))
# post_density_plot(theta, 80)
# 
# ######## USING GGPLOT2 TO PRODUCE A HISTOGRAM #########
# source(paste(code_dir,"post_density_ggplot.r",sep = ""))
# graph <- post_density_ggplot(samples)
# graph
# 
# ######## USING METRICSGRAPHICS JS #####################
# # http://hrbrmstr.github.io/metricsgraphics/
# # https://github.com/mozilla/metrics-graphics
# source(paste(code_dir,"post_density_mjs.r",sep = ""))
# graph <- post_density_mjs(samples, 80)
# graph
