
# 0. Información general -----------------------------------------------------

###### Sesión 2 LTP_TC2002B_570
###### Autor: Manuel Toral
###### Fecha: 2022/08/23
###### Título: Introducción al análisis de datos


# 1. Bibliotecas  -----------------------------------------------------

library(tidyverse) # Un viejo conocido
library(readxl) # Porque una vida sin excel no es vida...
library(janitor) # Limpieza
library(tidytext) # Una navaja suiza para el procesamiento de texto
library(quanteda) # Herramientas poderosas de análisis de texto
library(tm) # El estándar para el trabajo de minería de texto
library(wordcloud) # Gráficas de nubes de palabras



# 2. Datos ----------------------------------------------------------------

con21 <- read_excel("data/baseDatosCandidatos.xls")




# 3. Limpieza -------------------------------------------------------------

con21nl <- con21 |> 
  clean_names() |> # El nombre de las variables en minúsculas
  filter(entidad == "NUEVO LEÓN") |> 
  filter(tipo_candidato == "PROPIETARIO") |>
  select(partido_coalicion, candidatura, distrito:nombre_candidato,
         edad, genero, propuesta_1:propuesta_genero)

# 4. Quemar a quienes no contestaron la encuesta

pereza <- con21nl |> 
  mutate(
    vacio = case_when(
      is.na(propuesta_1) & is.na(propuesta_2) & is.na(propuesta_genero) ~ "Sí",
      TRUE ~ "No"
    )
  ) |> 
  filter(vacio == "Sí")

# 5. Pegar las 3 propuestas en una sola
  
con21nl_clean <- con21nl |> 
  filter(!num_lista_o_formula %in% pereza$num_lista_o_formula) |> 
  mutate(propuesta_1 = str_replace_na(propuesta_1, replacement = "")) |> 
  mutate(propuesta_2 = str_replace_na(propuesta_2, replacement = "")) |> 
  mutate(propuesta_genero = str_replace_na(propuesta_genero, replacement = "")) |> 
  mutate(prop_c = str_c(
    propuesta_1, propuesta_2, propuesta_genero, sep = " "
  ))


# 6. Tokenizar y conteos

# ¿Cuáles fueron las palabras más usadas por cada pp?

pal_nl21 <- con21nl_clean |> 
  unnest_tokens(
    output = "palabras",
    input = "prop_c",
    token = "words"
  ) |> 
  group_by(partido_coalicion, palabras) |> 
  summarise(cuenta = n()) |> 
  arrange(-cuenta) |> 
  filter(!palabras %in% tm::stopwords("es")) |> 
  filter(!palabras %in% c("así", "ser", "méxico", "país", "tener")) |> 
  ungroup() |> 
  group_by(partido_coalicion) |> 
  mutate(prop_unicas = n()/sum(cuenta)) |> 
  top_n(5, cuenta)

# Graficar ggplot2

pal_nl21 |> 
  ungroup() |> 
  ggplot(
    aes(x = cuenta, y = palabras)
  ) +
  geom_col() +
  facet_wrap(~partido_coalicion, scales = "free")

# Me interesa el tema del agua

con21nl_clean |> 
  unnest_tokens(
    output = "palabras",
    input = "prop_c",
    token = "words"
  ) |> 
  group_by(partido_coalicion, palabras) |> 
  summarise(cuenta = n()) |> 
  arrange(-cuenta) |> 
  filter(!palabras %in% tm::stopwords("es")) |> 
  filter(!palabras %in% c("así", "ser", "méxico", "país", "tener")) |>
  filter(palabras %in% c("agua", "hidráulico", "hidráulica"))
  ungroup()


