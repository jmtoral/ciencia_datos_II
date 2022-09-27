## Sesión 4
## Análisis de correlaciones y redes con n-grams

# 1. Bibliotecas ----------------------------------------------------------

library(tidyverse)

library(tidytext)
library(quanteda)

library(ggraph)
library(igraph)

library(widyr)


# 2. Datos ----------------------------------------------------------------

# Utilizaremos los datos de la SCJN que scrapearon la sesión pasada.

sesion_ppo <- read_csv("data/sesion_ppo.csv")

votos <- tibble(
  interlocutores = c("ARTURO ZALDÍVAR LELO DE LARREA",
  "LUIS MARÍA AGUILAR MORALES",
  "YASMÍN ESQUIVEL MOSSA",
  "LORETTA ORTIZ AHLF",
  "ALBERTO PÉREZ DAYÁN",
  "JUAN LUIS GONZÁLEZ ALCÁNTARA CARRANCÁ",
  "JORGE MARIO PARDO REBOLLEDO",
  "ANA MARGARITA RÍOS FARJAT",
  "JAVIER LAYNEZ POTISEK",
  "ALFREDO GUTIÉRREZ ORTIZ MENA",
  "NORMA LUCÍA PIÑA HERNÁNDEZ") ,
  contra = c("no", "no", "sí", "sí", "sí",
             "no", "no", "no", "no", "no", "no")
)


# 3. Funciones ------------------------------------------------------------

### Una función para limpiarlas a todas


limpiar <- function(data, vars) {
  data |> 
    filter(!{{ vars }} %in% tm::stopwords("es")) |>
    filter(!{{ vars }} %in% c("sea")) |>
    filter(!str_detect({{ vars }}, "[0-9]")) |> 
    filter(nchar({{ vars }})>1) 
}


# 3. Limpieza -------------------------------------------------------------

intervenciones <- sesion_ppo %>% 
  mutate(n_palabras = str_count(texto, "\\w+")) %>% 
  filter(n_palabras > 1000)


# 4. Extraer bigramas --------------------------------------------------------

int.bigramas <- intervenciones |> 
  unnest_tokens("bigrama", "texto", token = "ngrams", n=2) |> 
  separate(bigrama, c("w1","w2"), sep = " ") |> 
  limpiar(w1) |> 
  limpiar(w2) |> 
  mutate(interlocutores = str_to_title(interlocutores)) |>
  mutate(interlocutores = str_replace(interlocutores, "\\bDe\\b", 
                                      "de"))

# 5. Contruir una sociomatriz

conteo.bigramas <- int.bigramas %>%
  count(w1, w2) |>
  filter(n > 8) 

grafo.bigramas <- conteo.bigramas |>
  graph_from_data_frame()


# # 6. Imprimir el grafo --------------------------------------------------

ggraph(grafo.bigramas, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# 7. Un grafo más bonito --------------------------------------------------


set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(grafo.bigramas, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.04, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=2) +
  theme_void()


# 8. Correlaciones entre términos -----------------------------------------

tokens.scjn <- sesion_ppo %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% tm::stopwords("es")) %>% 
  filter(!str_detect(word, "[0-9]"))

pairwise.corr <- tokens.scjn  %>%
  group_by(word) %>% 
  filter(n() > 5) %>% 
  count(word, interlocutores) %>% 
  pairwise_cor(word, interlocutores, n, sort = TRUE)


  