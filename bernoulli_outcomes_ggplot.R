# function for building bar chart of bernoulli trials

bernoulli_outcomes_ggplot <- function(data){

  library(tidyverse)

  # ggplot needs data in a dataframe
  df <- dplyr::tbl_df(data) %>%
    mutate(outcome = ifelse(n == 0, "failure",
                            ifelse(n == 1, "success", NA))) %>%
    select(one_of("outcome"))
  
  counts <- dplyr::count(df, outcome)
  failure_ct <- filter(counts, outcome == "failure") %>%
    select(matches("n"))
  success_ct <- filter(counts, outcome == "success") %>%
    select(matches("n"))
  
  note_text <- paste("N =", length(df$outcome))
  failure_txt <- paste(failure_ct, "failures")
  success_txt <- paste(success_ct, "successes")
  
  graph <- df %>%
    ggplot(aes(x = outcome)) +
    geom_bar() +
    annotate("text",x=1,y=length(df$outcome),label=note_text) +
    annotate("text",x=1,y=as.numeric(failure_ct + 1),label=failure_txt) +
    annotate("text",x=2,y=as.numeric(success_ct + 1),label=success_txt)
  
}