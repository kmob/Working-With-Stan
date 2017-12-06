# k successes out of n trials

#### Random Sample of Bernoulli Trials
# # draw random sample using rbinom(n, size, prob)
# # n    - number of observations
# # size - outcome space where 1 means {0, 1}
# # prob - number of successes
# set.seed(42) # fix the seed to replicate the sample
# n <- rbinom(10, 1, 0.6)

# # generate a data set to specific Bernoulli outcome 
# # 6 successes out of 10
# # randomize the sequence 
n <- c(rep(0,4),rep(1,6))
n <- n[sample(1:length(n))]

# # set k successes and N trials
# k <- 6
# n <- 10