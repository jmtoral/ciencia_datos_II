
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



# 2. Datos -------------------------------------------------------------------

# El enlace directo al .xls
url <- "https://candidaturas2021.ine.mx/documentos/descargas/baseDatosCandidatos.xls" 

#Descargar en la carpeta "datos"
download.file(url, destfile = "data/baseDatosCandidatos.xls", mode = "wb")

# Leer datos
con21 <- read_excel("data/baseDatosCandidatos.xls")

# Datos de Nuevo León

con21nl <- con21 |> 
  filter(ENTIDAD == "NUEVO LEÓN" ) |>
  select(PARTIDO_COALICION, CANDIDATURA, DISTRITO,
         EDAD, GENERO, PROPUESTA_1:PROPUESTA_GENERO)

# 3. Limpieza

con21nl <- con21nl |> 
  mutate(prop_pasted = str_c(  
    PROPUESTA_1, PROPUESTA_2, PROPUESTA_GENERO, sep = " "))  |> 
  select(-contains("PROPUESTA_")) |> 
  filter(!prop_pasted == "NA")


# 4. Tokenización

con21nl |> 
  unnest_tokens( 
    output = "palabras", 
    input = "prop_pasted", 
    token = "words" 
  ) |> 
  count(palabras, sort = T) |> 
  filter(!palabras %in% tm::stopwords("es"))

