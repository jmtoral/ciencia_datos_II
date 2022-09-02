library(tidyverse) # Un viejo conocido
library(readxl) # Porque una vida sin excel no es vida...
library(janitor) # Limpieza
library(tidytext) # Una navaja suiza para el procesamiento de texto
library(quanteda) # Herramientas poderosas de análisis de texto
library(tm) # El estándar para el trabajo de minería de texto
library(igraph) # Análisis de redes
library(ggraph) # Visualizar redes



con21nl <-  read_csv(
  "https://raw.githubusercontent.com/jmtoral/ciencia_datos_II/main/data/con21nl.csv"
)


bi_nl21 <- con21nl |> 
  unnest_tokens(
    output = "bigram",
    input = "prop_c",
    token = "ngrams",
    n = 2
  ) |> 
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% tm::stopwords("es")) |> 
  filter(!word2 %in% tm::stopwords("es")) |> 
  count(word1, word2, sort=T)









