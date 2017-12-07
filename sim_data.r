# k successes out of n trials

sim_data <- function(data_name){
  
  library(feather)

  # How many underlying processes
  process <- c(rep("theta", 10))
  
  #### Random Sample of Bernoulli Trials
  # # draw random sample using rbinom(n, size, prob)
  # # n    - number of observations
  # # size - outcome space where 1 means {0, 1}
  # # prob - number of successes
  # set.seed(42) # fix the seed to replicate the sample
  # obs <- rbinom(10, 1, 0.6)
  
  # # generate a data set to specific Bernoulli outcome 
  # # 6 successes out of 10
  # # randomize the sequence 
  obs <- c(rep(0,4),rep(1,6))
  obs <- obs[sample(1:length(obs))]
  
  # build data frame
  df <- dplyr::data_frame(process, obs)
  df <- df %>%
    mutate(outcome = ifelse(obs == 0, "failure",
                            ifelse(obs == 1, "success", NA)))
  
  feather_filename <- paste(data_name, ".feather", sep="")
  write_feather(df, feather_filename)
  }



