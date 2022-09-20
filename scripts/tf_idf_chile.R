
# 1. Bibliotecas ----------------------------------------------------------


library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud)
library(reshape2)


# Datos -------------------------------------------------------------------


url_vieja <-  "https://es.wikisource.org/wiki/Constituci%C3%B3n_Pol%C3%ADtica_de_la_Rep%C3%BAblica_de_Chile"
url_nueva <-  "https://es.wikisource.org/wiki/Propuesta_de_Constituci%C3%B3n_Pol%C3%ADtica_de_la_Rep%C3%BAblica_de_Chile_de_2022#Art%C3%ADculo_1"


cv <- read_html(url_vieja) |> 
  html_elements("p") |> 
  html_text() 

cv.t <- tibble(texto = cv,
               tipo = "Actual")



cn <- read_html(url_nueva) |> 
  html_elements("dd , dt") |> 
  html_text()

cn.t <- tibble(texto = cn,
               tipo = "Propuesta")


cn.comp <- cn.t |> 
  bind_rows(cv.t)




# Tokens ----------------------------------------------------------------


otras.stop = c("artículo", "ley", "podrá", "constitución",
               "ser", "deberá", "sólo", "éste", "p", "ésta",
               "I", "ñ", "l", "hubiere", "ex", "n", "m", "promoverá")



conteo <- cn.comp |> 
  unnest_tokens("palabra", "texto", token = "words") |> 
  filter(!palabra %in% tm::stopwords("es")) |>
  filter(!palabra %in% otras.stop) |>
  filter(!str_detect(palabra, "[0-9]")) |> 
  filter(nchar(palabra)>1) |> 
  count(palabra, tipo, sort=T)


x <- conteo |> 
  bind_tf_idf(palabra, tipo, n) |> 
  arrange(-tf_idf) |> 
  group_by(tipo) |> 
  top_n(500) |> 
  ungroup()




png("img/prueba.png", width = 800, height = 800, units="px", res=100)
x %>%
  acast(palabra ~ tipo, value.var = "tf_idf", fill = 0) %>%
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 200)
dev.off()



# Bigramas! ---------------------------------------------------------------


### Una función para limpiarlas a todas

limpiar <- function(data, vars) {
  data |> 
    filter(!{{ vars }} %in% tm::stopwords("es")) |>
    filter(!{{ vars }} %in% otras.stop) |>
    filter(!str_detect({{ vars }}, "[0-9]")) |> 
    filter(nchar({{ vars }})>1) 
}



conteo <- cn.comp |> 
  unnest_tokens("bigrama", "texto", token = "ngrams", n=2) |> 
  separate(bigrama, c("w1","w2"), sep = " ") |> 
  limpiar(w1) |> 
  limpiar(w2) |> 
  unite(bigrama, c("w1", "w2"), sep = " ") |> 
  count(bigrama, tipo, sort=T)


x <- conteo |> 
  bind_tf_idf(bigrama, tipo, n) |> 
  arrange(-tf_idf) |> 
  group_by(tipo) |> 
  top_n(500) |> 
  ungroup()




png("img/prueba2.png", width = 800, height = 800, units="px", res=100)
x %>%
  acast(bigrama ~ tipo, value.var = "tf_idf", fill = 0) %>%
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 200)
dev.off()

