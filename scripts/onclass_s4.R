## Algunas herramientas de bigramas (análisis de redes y correlaciones en pares)
## 20 de septiembre de 2022


# 0. Bibliotecas ----------------------------------------------------------

library(tidyverse)
library(tidytext)

library(igraph) # install.packages("igraph") 
library(ggraph) # Graficas grafos con gg

library(widyr) # Análisis de correlaciones en pares
library(quanteda)

# 1. Datos ----------------------------------------------------------------

sesion_ppo <- read_csv("https://raw.githubusercontent.com/jmtoral/ciencia_datos_II/main/data/sesion_ppo.csv")

glimpse(sesion_ppo)


# 2. Extraer bigramas -----------------------------------------------------

# La niña está triste.

# Bigrama

# La niña
# niña está
# está triste

# Trigrama

# La niña está
# niña está triste

intervenciones <- sesion_ppo |>
  mutate(n_palabras = str_count(texto, "\\w+")) |>
  filter(n_palabras > 1000)
  
int.bigramas <- intervenciones |> 
  unnest_tokens(bigramas, texto, token = "ngrams", n = 2) |> 
  separate(bigramas, c("w1", "w2"), sep = " ") |> # 1. Separar los bigramas
  filter(!w1 %in% tm::stopwords("es")) |> # 2. Limpiar la primera columna
  filter(!w2 %in% tm::stopwords("es")) |>  # 3. Limpiar la segunda columna
  filter(!w1 %in% c("así", "ahora")) |> # 2. Limpiar la primera columna
  filter(!w2 %in% c("así", "ahora")) |>   # 3. Limpiar la segunda columna
  mutate(interlocutores = str_to_title(interlocutores)) |> 
  mutate(interlocutores = str_replace(interlocutores,
                                      "\\bDe\\b", "de"))


# 3. Construir una sociomatriz (sociomatrix) ------------------------------

conteo.bigramas <- int.bigramas |> 
  count(w1, w2, sort = T) |> 
  filter(n > 3)

grafo.bigramas <- conteo.bigramas |> 
  graph_from_data_frame()


# 4. Imprimir el grafo -------------------------------------------------------
# ggplot

ggraph(grafo.bigramas, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust =1)
  
# 5. Pulir el gráfico --------------------------------------------------

# Definir flecha

set.seed(2002)

a <- grid::arrow(type = "closed", length = unit(.05, "inches"))

ggraph(grafo.bigramas, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a,
                 show.legend = FALSE,
                 end_cap = circle(.04, "inches")) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust =1, size = 2) +
  theme_void()



# 6. Correlaciones en pares (pairwise correlations) -----------------------

tokens.int <- intervenciones |> # Sólo las intervenciones
  unnest_tokens(word, texto) |> 
  filter(!word %in% tm::stopwords("es")) 

pairwise.cor <- tokens.int |> 
  group_by(word) |> 
  filter(n() > 5) |> 
  count(word, interlocutores) |> 
  pairwise_cor(word, interlocutores, n, sort = T )

pairwise.cor |> 
  filter(item1 == "delito")

pairwise.cor |> 
  filter(item1 == "social")



# 7. Visualizar correlaciones ---------------------------------------------

## Rho de Pearson

pairwise.cor |> 
  filter(correlation > 0.9) |> 
  graph_from_data_frame() |> 
  ggraph(layout = "fr") +
  geom_edge_link(aes(
    alpha = correlation
  )) +
  geom_node_point(size = 2, color = "lightblue") +
  geom_node_text(aes(label = name), size = 3) +
  theme_void()

ggsave("red_cor.png", width = 10, height = 10)







