# k successes out of n trials

# # draw random sample of Bernoulli trials using rbinom(n, size, prob)
# # n is the number of observations
# # size is outcome space where 1 means {0, 1}
# # prob is the number of successes
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