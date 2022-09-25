

# libraries ---------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)

# count bigrams -----------------------------------------------------------

#' @param dataset texto em formato tidy
count_bigrams <- function(dataset) {
  
  input_var <- dataset %>% 
    pull(input) %>% 
    unique()
  
  
  dataset %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopwords::data_stopwords_nltk$pt,
           !word2 %in% stopwords::data_stopwords_nltk$pt,
           !is.na(word1) & !is.na(word2)) %>%
    count(word1, word2, sort = TRUE) %>% 
    mutate(input_var = input_var)
}
#' Visualizr grafos orientados de bi-grams
#' 
#' @param bigrams estatística de contagem de bi-grams
visualize_bigrams <- function(bigrams, quartil) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  input_var <- bigrams %>% 
    pull(input_var) %>% 
    unique()
    
  
  bigrams %>%
    filter(n >= quantile(n, probs =   {{quartil}}  )) %>% 
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()+
    ggtitle(input_var)
}
