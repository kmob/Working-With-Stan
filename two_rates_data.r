# k successes out of n trials

#### Random Sample of Bernoulli Trials
# # draw random sample using rbinom(n, size, prob)
# # n    - number of observations
# # size - outcome space where 1 means {0, 1}
# # prob - number of successes
# set.seed(42) # fix the seed to replicate the sample
# t1 <- rbinom(10, 1, 0.7)
# t2 <- rbinom(10, 1, 0.5)

# # generate a data set to specific Bernoulli outcome 
# # 6 successes out of 10
# # randomize the sequence 
t1 <- c(rep(0,3),rep(1,7))
t1 <- t1[sample(1:length(t1))]

t2 <- c(rep(0,5),rep(1,5))
t2 <- t2[sample(1:length(t2))]

# # set k successes and N trials
# k1 <- 5
# n1 <- 10
# k2 <- 7
# n2 <- 10