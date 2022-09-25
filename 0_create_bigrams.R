

# libraries ---------------------------------------------------------------

library(tidytext)
library(tidyverse)


# create bi-grams ---------------------------------------------------------



#' Create bi-grams
#' 
#' @param dataset dataframe com as colunas de texto
#' 
#' @param input_var text var
#' 
#' id column: identificador da unidade de análise, por exemplo: capítulo, cliente, livro etc.
create_bigram <- function(dataset, input_var){
  
  dataset %>% 
    select(id,  {{input_var}}  ) %>% 
    tidytext::unnest_tokens(
      output = bigram,
      input =   {{input_var}}  ,
      token = "ngrams",
      n = 2
    )
  
}





