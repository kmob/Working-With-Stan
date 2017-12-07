# k successes out of n trials

#### Random Sample of Bernoulli Trials
# # draw random sample using rbinom(n, size, prob)
# # n    - number of observations
# # size - outcome space where 1 means {0, 1}
# # prob - number of successes
# set.seed(42) # fix the seed to replicate the sample
# n1 <- rbinom(10, 1, 0.7)
# n2 <- rbinom(10, 1, 0.5)

# # generate a data set to specific Bernoulli outcome 
# # 6 successes out of 10
# # randomize the sequence 
n1 <- c(rep(0,3),rep(1,7))
n1 <- n1[sample(1:length(n1))]

n2 <- c(rep(0,5),rep(1,5))
n2 <- n2[sample(1:length(n2))]

# # set k successes and N trials
# k1 <- 5
# n1 <- 10
# k2 <- 7
# n2 <- 10