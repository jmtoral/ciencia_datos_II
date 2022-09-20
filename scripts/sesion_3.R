
# 1. Bibliotecas ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud)
library(reshape2)


# Datos -------------------------------------------------------------------
url_vieja <-  "https://es.wikisource.org/wiki/Constituci%C3%B3n_Pol%C3%ADtica_de_la_Rep%C3%BAblica_de_Chile"
url_nueva <-  "https://es.wikisource.org/wiki/Propuesta_de_Constituci%C3%B3n_Pol%C3%ADtica_de_la_Rep%C3%BAblica_de_Chile_de_2022#Art%C3%ADculo_1"


# Scrapear ----------------------------------------------------------------

## La Actual Constitución

cons_actual <- read_html(url_vieja) |> 
  html_elements(xpath = "//div[@class='mw-parser-output']/p") |>  #html_nodes()
  html_text()

# cons_actual <- read_html(url_vieja) |> 
#   html_elements("p") |>  #html_nodes()
#   html_text()

cons_actual_tbl <- tibble(
  texto = cons_actual,
  tipo = "actual"
) 

## La nueva Constitución

cons_nueva <- read_html(url_nueva) |> 
  html_elements(xpath = "//dl/dd") |>  #html_nodes()
  html_text()

# cons_nueva <- read_html(url_nueva) |> 
#   html_elements("dd , dt") |>  #html_nodes()
#   html_text()

cons_nueva_tbl <- tibble(
  texto = cons_nueva,
  tipo = "nueva"
) 


# Unir dos datasets -------------------------------------------------------

cons_chile <- cons_actual_tbl |> 
  bind_rows(cons_nueva_tbl)


# Tokenizar ---------------------------------------------------------------

cons_chile_tokens <- cons_chile |> 
  unnest_tokens(output = "palabra",
                input = "texto",
                token = "words") |> 
  count(tipo, palabra, sort = T) |> 
  filter(!palabra %in% tm::stopwords("es")) |>  #Yes
  filter(!palabra %in% c("artículo", "podrá", "ser", "nº", "sólo",
                         "deberá", "podrán", "éstas", "ésta")) |> 
  filter(nchar(palabra)>2) |> 
  filter(!str_detect(palabra, "[0-9]")) # Quita cualquier palabra con algún tipo de dígito
  


# TF-IDF ------------------------------------------------------------------

cons_chile_tfidf <- cons_chile_tokens |> 
  bind_tf_idf(
    term = "palabra",
    document = "tipo",
    n = "n"
  ) |> 
  arrange(-tf_idf)


# Nube de palabras

cons_chile_tfidf |> 
  reshape2::acast(palabra ~ tipo, value.var = "tf_idf", fill = 0) |> 
  comparison.cloud()









