# convert df to list for stan

df_to_list <- function(df) {
  library(dplyr)
  
  vec <- c()
  v_1 <- character()
  v_2 <- integer()
  n_df <- data %>% count(process)
  success_df <- data %>% group_by(process) %>% count(outcome) %>% filter(outcome == "success")
  data_names <- c("k", "n")
  j <- length(data_names)
  while (j > 0) {
    name <- data_names[j]
    i <- length(rate_list)
    while (i > 0) {  
      data_name <- paste(name, i, sep = "")
      # v_1 <- c(v_1, data_name)
      # if (name == "k") {
      #   v_2 <- c(v_2, success_df$n[i])
      # }
      # else {v_2 <- c(v_2, n_df$n[i])}
      if (name == "k") {
        ct <- success_df$n[i]
      }
      else {ct <- n_df$n[i]}
      vec <- c(vec, data_name = ct)
      i <- i - 1
    }
    j <- j - 1
  }
  
  data_list <- as.list(dplyr::data_frame(v_1, v_2))
  
}