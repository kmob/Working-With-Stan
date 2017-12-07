# Setup
rm(list=ls())
library(rstan)
library(bayesplot)
library(dplyr)
library(feather)

##### Data #####
data_name <- "two_rates"

# Simulate Data
source("sim_data.r")
sim_data(data_name, 10, c(0.9, 0.5, 0.1))

# Read Data
data <- read_feather(paste(data_name, ".feather", sep = ""))

# Stan needs data in a list
data_list <- list(n = length(data$obs),
                  k = length(subset(data$obs, data$obs == 1)))

# Visualize Data
ggplot(data, aes(x = process)) + geom_bar(aes(fill=outcome))

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

# Run Stan with defaults
samples_default <- stan(file="binomial_uniform_prior.stan",
                        data=data_list)

# Run Stan with custom setings
samples_custom <- stan(file="binomial_uniform_prior.stan",   
                data=data_list, 
                init=myinits,  # If not specified, gives random inits
                pars=parameters,
                iter=20000, 
                chains=2, 
                thin=1
                # warmup = 100,  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)

print(samples_default)
print(samples_custom)
mcmc_neff(neff_ratio(samples_default), size = 2)
mcmc_neff(neff_ratio(samples_custom), size = 2)
mcmc_acf(as.array(samples_default), pars = "theta", lags = 10)
mcmc_acf(as.array(samples_custom), pars = "theta", lags = 10)
mcmc_trace(as.array(samples_default), pars = "theta", 
           np = nuts_params(samples_default)) +
  xlab("Post-warmup iteration")
mcmc_trace(as.array(samples_custom), pars = "theta", 
           np = nuts_params(samples_custom)) +
  xlab("Post-warmup iteration")
mcmc_nuts_divergence(nuts_params(samples_default), 
                     log_posterior(samples_default))
mcmc_nuts_divergence(nuts_params(samples_custom), 
                     log_posterior(samples_custom))
source("post_density_bayesplot.r")
graph <- post_density_bayesplot(samples_default, "theta", 0.8, "mean")
graph 
graph <- post_density_bayesplot(samples_custom, "theta", 0.8, "mean")
graph 

# Stan Output

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
mcmc_acf(posterior_cp, pars = "theta", lags = 10)

# Evaluate NUTS sampler
# log posterior over iterations
lp_cp <- log_posterior(samples)
head(lp_cp)
# find iterations with divergence 
np_cp <- nuts_params(samples)
head(np_cp)

# trace plot of MCMC draws and divergence, if any, for NUTS.
color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_cp, pars = "theta", np = np_cp) +
  xlab("Post-warmup iteration")

# further info on divergence
# Divergences often indicate that some part of the 
# posterior isn’t being explored. 
color_scheme_set("red")
mcmc_nuts_divergence(np_cp, lp_cp)

#### Stan_plot ####
# Visual posterior analysis based on ggplot2.
# plot function for an object of class stanfit
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
g <- samples %>% 
  stan_plot(point_est = "mean", 
            ci_level = uncertainty_interval, 
            outer_level = outer_uncertainty_interval) + 
  labs(title = g_title, 
       subtitle = g_subtitle)

g


