
pacman::p_load(tidyverse,
               tidytext,
               cluster, 
               tm)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ine <- readxl::read_excel("data/baseDatosCandidatos.xls")

abb <- tibble(
  PARTIDO_COALICION = unique(ine$PARTIDO_COALICION),
  siglas = c("PT", "PVEM", "MC", "MORENA", "PES", "RSP",
           "FxM", "VxM", "PAN", "PRI", "PRD", "JHH", "Ind")
)
  
# Explorar ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ine <- ine %>% 
  left_join(abb) %>% 
  select(siglas, contains("PROPUESTA")) %>% 
  mutate(propuesta_c = str_c(PROPUESTA_1,
                             PROPUESTA_2,
                             #                           PROPUESTA_GENERO,
                             sep = " ")) %>% 
  select(siglas, propuesta_c)


ine.tm <- ine %>% 
  unnest_tokens( palabras, propuesta_c) %>% # Matriz de términos
  filter(!palabras %in% tm::stopwords("es"))

### Matriz de document - término


ine.dtm <- ine.tm %>%
  count(siglas, palabras) %>% 
  cast_dtm(siglas, palabras, n)

## Recorte de sparsity 

ine.sub <- tm::removeSparseTerms(ine.dtm, 0.01)

tm::inspect(ine.sub)

# Distancia

d <- dist(ine.sub, method = "euclidian")

clus <- hclust(d, method = "complete")

plot(clus)


e <- dist(ine.dtm, method = "euclidian")

clus <- hclust(e, method = "complete")

plot(clus)


d <- dist(ine.sub, method = "manhattan")

clus <- hclust(d, method = "complete")

plot(clus)


e <- dist(ine.dtm, method = "manhattan")

clus <- hclust(d, method = "complete")

plot(clus)

# Términos

ine.term <- tm::removeSparseTerms(ine.dtm, 0.05)

d <- dist(ine.term, method = "euclidian")

clus <- hclust(d, method = "complete")

plot(clus)