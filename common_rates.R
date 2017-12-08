# Setup
rm(list=ls())
library(rstan)
library(bayesplot)
library(dplyr)
library(feather)

##### Data #####
data_name <- "common_rates"

# Simulate Data
source("sim_data.r")
rate_list <- c(0.6, 0.5)
sim_data(data_name, 10, rate_list, "fixed")

# Read Data
data <- read_feather(paste(data_name, ".feather", sep = ""))

# Visualize Data
ggplot(data, aes(x = process)) + geom_bar(aes(fill=outcome))

#### Model ####
# Visualize Model
source("gm_common_rates.r")
gm_common_rates()

# plate notation
source("gm_i_common_rates.r")
gm_i_common_rates()


#### STAN ####
# Parameters
# inits via list:
#   Set inital values by providing a list equal in length to the 
#   number of chains. The elements of this list should themselves 
#   be named lists, where each of these named lists has the name 
#   of a parameter and is used to specify the initial values for 
#   that parameter for the corresponding chain.
myinits <- list(
  list(theta=0.5),
  list(theta=0.5))

# parameters to be monitored:  
parameters <- c("theta")

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
  k2 = nrow(success_k2)
)

model_code <- "binomial_common_rates.stan"

# Run Stan with defaults
samples_default <- stan(file=model_code,
                        data=stan_data)

# Run Stan with custom setings
samples_custom <- stan(file=model_code,   
                       data=stan_data, 
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
mcmc_acf(as.array(samples_default), pars = parameters, lags = 10)
mcmc_acf(as.array(samples_custom), pars = parameters, lags = 10)
mcmc_trace(as.array(samples_default), pars = parameters, 
           np = nuts_params(samples_default)) +
  xlab("Post-warmup iteration")
mcmc_trace(as.array(samples_custom), pars = parameters, 
           np = nuts_params(samples_custom)) +
  xlab("Post-warmup iteration")
mcmc_nuts_divergence(nuts_params(samples_default), 
                     log_posterior(samples_default))
mcmc_nuts_divergence(nuts_params(samples_custom), 
                     log_posterior(samples_custom))
source("post_density_bayesplot.r")
graph <- post_density_bayesplot(samples_default, parameters, 0.8, "mean")
graph 
graph <- post_density_bayesplot(samples_custom, parameters, 0.8, "mean")
graph 

theta      <- rstan::extract(samples_default)$theta
postpredk1 <- rstan::extract(samples_default)$postpredk1
postpredk2 <- rstan::extract(samples_default)$postpredk2

# Two-panel plot. 
layout(matrix(c(1,2),1,2))
layout.show(2)
# First, a histogram for theta.
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
Nbreaks <- 80
y       <- hist(theta, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
     xlim=c(0,1), ylim=c(0,10), xlab="Theta", ylab="Density") 
# let's plot a density estimate over this:
lines(density(theta), col="red", lwd=2)

# Second plot, the data space (predictives)
plot(stan_data$k1, stan_data$k2, 
     type="p", pch=4, cex=2, lwd=2, 
     xlab="Success Count 1",
     ylab="Success Count 2", 
     xlim=c(-1, stan_data$n1+1), 
     ylim=c(-1, stan_data$n2+1))        
nsamples <- length(theta)
sc <- 10
# add a symbol at each possible outcome of sets 1 and 2
# size the symbol using freqency of the outcome in the posterior predicted
# scaled by number of samples
for (i in 0:stan_data$n1)
{
  for (j in 0:stan_data$n2) 
  {
    match.preds <- sum(postpredk1==i & postpredk2==j)/nsamples
    if (match.preds > 0)
    {
      points(i,j, pch=1, cex=sc*sqrt(match.preds)) 
    }
  }
}

# Stan Output

# Info from a S4 object of class "stanfit"
# https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
# get_stancode extracts the model
stan_model <- get_stancode(samples_default)
cat(stan_model)

# print shows summary of parameter and log-posterior (lp__) 
# posterior mean, posterior standard deviation, and quantiles
# Monte Carlo standard error (se_mean)
# effective sample size (n_eff)
# R-hat statistic (Rhat)
print(samples_default)  # a rough summary

#### Bayesplot ####
# http://mc-stan.org/bayesplot/
# neff_ratio - Look at effective sample size
# Draws in a Markov chain are not independent if there is autocorrelation. 
# If there is autocorrelation, the effective sample size will be smaller 
# than the total sample size, N. 
# The larger the ratio of neff to N the better.
ratios_cp <- neff_ratio(samples_default)
print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

# rhat - potential scale reduction statistic
# If chains are at equilibrium, rhat will be 1. 
# If the chains have not converged rhat will be greater than one.
rhats <- rhat(samples_default)
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
posterior_cp <- as.array(samples_default)
mcmc_acf(posterior_cp, pars = parameters, lags = 10)

# Evaluate NUTS sampler
# log posterior over iterations
lp_cp <- log_posterior(samples_default)
head(lp_cp)
# find iterations with divergence 
np_cp <- nuts_params(samples_default)
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
# plot function for an object of class stanfit
plot(samples_default) 
# use stan_plot with piping to specify ggplot attributes
point_est <- "mean"
uncertainty_interval <- 0.8
outer_uncertainty_interval <- 0.95
g_title <- paste("Plot of", point_est, "for parameters:", parameters)
g_subtitle <- paste("Uncertainty intervals of", 
                    uncertainty_interval,
                    "and",
                    outer_uncertainty_interval)
g <- samples_default %>% 
  stan_plot(point_est = "mean", 
            ci_level = uncertainty_interval, 
            outer_level = outer_uncertainty_interval) + 
  labs(title = g_title, 
       subtitle = g_subtitle)

g


