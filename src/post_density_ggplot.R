## histogram from ggplot2

post_density_ggplot <- function(post_data){

  library(tidyverse)
  
  # ggplot needs data in a dataframe
  # TODO switch to dplyr::tbl_df(post_data)
  samples_df <- as.data.frame(post_data)
  
  theta_hist <- samples_df %>%
    ggplot(aes(x = samples_df$theta)) +
    geom_histogram()
  
}