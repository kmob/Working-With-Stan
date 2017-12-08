# k successes out of n trials

sim_data <- function(data_name, observations, process_list, sample){
  
  library(feather)
  
  # data_name <- "test"
  # observations <- 10
  # process_list <- c(0.9, 0.5, 0.1)
  # set.seed(42) # fix the seed to replicate the sample
  process <- character()
  obs <- integer()
  i <- length(process_list)
  
  #### Random Sample of Bernoulli Trials
  # # draw random sample using rbinom(n, size, prob)
  # # n    - number of observations
  # # size - outcome space where 1 means {0, 1}
  # # prob - number of successes
  # while (i > 0) {
  #   # add one obs vs add a set of observations???
  #   process_name <- paste("theta_", i, sep = "")
  #   process <- c(process, rep(process_name, observations))
  #   obs <- c(obs, rbinom(observations, 1, process_list[i]))
  #   i <- i-1
  #   print(i)
  # }
  
  # # generate a data set to specific Bernoulli outcome 
  # # 6 successes out of 10
  # # randomize the sequence 
  while (i > 0) {
    # add one obs vs add a set of observations???
    process_name <- paste("theta_", i, sep = "")
    process <- c(process, rep(process_name, observations))
    
    if (sample == "random") {
      obs <- c(obs, rbinom(observations, 1, process_list[i]))
    } else if (sample == "fixed") {
      success_ct <- process_list[i]*observations
      failure_ct <- observations - success_ct
      new_obs <- c(rep(0,failure_ct),rep(1,success_ct))
      new_obs <- new_obs[sample(1:length(new_obs))]
      obs <- c(obs, new_obs)      
    }
    i <- i-1
    print(i)
  }
  
  # build data frame
  df <- dplyr::data_frame(process, obs)
  df <- df %>%
    mutate(outcome = ifelse(obs == 0, "failure",
                            ifelse(obs == 1, "success", NA)))
  
  feather_filename <- paste(data_name, ".feather", sep="")
  write_feather(df, feather_filename)
}



