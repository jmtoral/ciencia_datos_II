
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

con21 <- read_excel("TEC/LTP_TC2002B_570/data/baseDatosCandidatos.xls")


# 3. Filtros y Select ------------------------------------------------------

con21nl <- con21 |> 
  clean_names() |> 
  filter(entidad == "NUEVO LEÓN") |> 
  filter(tipo_candidato == "PROPIETARIO") |>  
  select(partido_coalicion, candidatura, distrito:nombre_candidato,
         edad, genero, propuesta_1:propuesta_genero)



# Quienes no pusieron nada ------------------------------------------------


pereza <- con21nl |> 
  mutate(
    pereza = case_when(
      is.na(propuesta_1) & is.na(propuesta_2) & is.na(propuesta_genero) ~ "sí",
      TRUE ~ "no"
    )
  ) |> 
  filter(pereza == "sí") 


pereza  |> 
  select(partido_coalicion, distrito, nombre_candidato)



# Pegar propuestas --------------------------------------------------------------


con21nl_clean <- con21nl |> 
  filter(!num_lista_o_formula %in% pereza$num_lista_o_formula) |> 
  mutate(propuesta_1 = str_replace_na(propuesta_1, replacement = "")) |> 
  mutate(propuesta_2 = str_replace_na(propuesta_2, replacement = "")) |> 
  mutate(propuesta_genero = str_replace_na(propuesta_genero, replacement = "")) |> 
  mutate(prop_c = str_c(
    propuesta_1, propuesta_2, propuesta_genero, sep = " "
  ))



# Tokenizar y conteos ---------------------------------------------------------------


pal_count_nl <-  con21nl |> 
  unnest_tokens( 
    output = "palabras", 
    input = "prop_pasted", 
    token = "words" 
  ) |> 
  count(partido_coalicion, palabras, sort = T) |> 
  group_by(partido_coalicion) |> 
  mutate(total = sum(n)) |> 
  mutate(unicas = n()) |> 
  mutate(prop_unicas = unicas/total) |> 
  filter(!palabras %in% tm::stopwords("es")) 


# Partidos ----------------------------------------------------------------


pal_count_nl <-  pal_count_nl |> 
  mutate(siglas = case_when(
    partido_coalicion == "FUERZA POR MÉXICO" ~ "FxM",
    partido_coalicion == "JUNTOS HACEMOS HISTORIA" ~ "JHH",
    partido_coalicion == "MOVIMIENTO CIUDADANO" ~ "MC",
    partido_coalicion == "PARTIDO ACCIÓN NACIONAL" ~ "PAN",
    partido_coalicion == "PARTIDO DE LA REVOLUCIÓN DEMOCRÁTICA" ~ "PRD",
    partido_coalicion == "PARTIDO DEL TRABAJO" ~ "PT",
    partido_coalicion == "PARTIDO ENCUENTRO SOLIDARIO" ~ "PES",
    partido_coalicion == "PARTIDO REVOLUCIONARIO INSTITUCIONAL" ~ "PRI",
    partido_coalicion == "PARTIDO VERDE ECOLOGISTA DE MÉXICO" ~ "PVEM",
    partido_coalicion == "REDES SOCIALES PROGRESISTAS" ~ "RSP",
    TRUE ~ partido_coalicion
  ))

# Wordcloud ---------------------------------------------------------------


wordcloud(pal_count_nl$palabras, pal_count_nl$n, max.words = 100)

# Conteo ------------------------------------------------------------------


pal_count_nl |> 
  ggplot(aes(
    x= n,
    y= reorder_within(palabras, n, siglas),
    fill = siglas
  )) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~siglas, scales = "free") +
  guides(fill="none") +
  labs(title= "Palabras más frecuentes por partido",
       x="Frecuencia", y= "")


# Proporción --------------------------------------------------------------


pal_count_nl |> 
  distinct(partido_coalicion, prop_unicas) |> 
  mutate(prop_unicas = round(prop_unicas, 2)) |> 
  arrange(-prop_unicas) |> 
  ggplot(aes(
    x = prop_unicas,
    y = reorder(partido_coalicion, prop_unicas)
  )) +
  geom_col() +
  labs(x= "Proporción únicas sobre el total",
       y= "Partidos",
       title="Proporción de palabras únicas por partido")
