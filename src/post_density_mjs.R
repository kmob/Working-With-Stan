# plot

post_density_mjs <- function(post_data, nBins){
  library(htmltools)
  library(htmlwidgets)
  library(metricsgraphics)
  library(RColorBrewer)
  library(tidyverse)
  
  samples_df <- as.data.frame(post_data)
  
  graph_out <- mjs_plot(samples_df$theta) %>%
    mjs_histogram(bins=nBins) %>%
    mjs_labs(x_label="Rate", y_label="Posterior Density")
}
