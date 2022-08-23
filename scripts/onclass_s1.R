
# 0. Información general -----------------------------------------------------

###### Sesión 1 LTP_TC2002B_570
###### Autor: Manuel Toral
###### Fecha: 2022/08/06
###### Título: Introducción a la tokenización


# 1. Bibliotecas  -----------------------------------------------------

library(tidyverse) # Un viejo conocido
library(readxl) # Porque una vida sin excel no es vida...
library(tidytext) # Una navaja suiza para el procesamiento de texto
library(quanteda) # Herramientas poderosas de análisis de texto
library(tm) # El estándar para el trabajo de minería de texto
library(wordcloud) # Gráficas de nubes de palabras




# Datos -------------------------------------------------------------------

url <- "https://candidaturas2021.ine.mx/documentos/descargas/baseDatosCandidatos.xls"

download.file(url, destfile = "baseDatosCandidatos.xls", mode = "wb") # Windows

#download.file(url, destfile = "baseDatosCandidatos.xls") #Mac


con21 <- read_excel("baseDatosCandidatos.xls")


# Limpieza y selección ----------------------------------------------------

con21nl <- con21 |> 
  filter(ENTIDAD == "NUEVO LEÓN") |> 
  select(PARTIDO_COALICION, CANDIDATURA,
        ENTIDAD, DISTRITO, NOMBRE_CANDIDATO,
        GENERO, PROPUESTA_1:PROPUESTA_GENERO) |> 
  mutate(GANZO = 0) |> 
  mutate(GANZO2 = 1) |> 
  mutate(GANZO3 = 2) |> 
  mutate(propuesta_completa = 
           str_c(PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO, sep = " ")
           ) |> 
  select(-contains("GANZO")) |> 
  select(-contains("PROPUESTA_", ignore.case = F)) |> 
  filter(!is.na(propuesta_completa))



# Desanidar tokens (tokenizar / tokenize) -----------------------------------------------

pal_con21nl <- con21nl |> 
  unnest_tokens(
    output = "palabra",
    input = "propuesta_completa",
    token = "words"
  ) |> 
  count(palabra, sort = T)  |> 
  filter(!palabra %in% tm::stopwords("es"))



