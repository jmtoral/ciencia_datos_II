library(tidyverse)

library(tidytext)
library(tidymodels)
library(textrecipes)
library(discrim)
library(naivebayes)

library(readxl)


noticias <- read_excel("data/train.xlsx") %>% 
  bind_rows(read_excel("data/development.xlsx")) 

noticias_cln <- noticias %>% 
  mutate(Text = str_remove(Text, "\\*NUMBER\\*")) %>% 
  mutate(Text = str_to_lower(Text)) %>% 
  mutate(Category = as.factor(Category))



noticias_split <- initial_split(noticias_cln, strata = Category)

noticias_train <- training(noticias_split)
noticias_test <- testing(noticias_split)

noticias_receta <- recipe(Category ~ Text, data = noticias_train)


noticias_receta  <- noticias_receta  %>%
  step_tokenize(Text) %>% 
  step_stopwords(Text, language = 'es', keep = FALSE) %>% 
  step_tokenfilter(Text, max_tokens = 1e4) %>%
  step_tfidf(Text)


noticias_wf <- workflow() %>%
  add_recipe(noticias_receta)



nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")


nb_fit <- noticias_wf %>%
  add_model(nb_spec) %>%
  fit(data = noticias_train)

set.seed(234)
noticias_folds <- vfold_cv(noticias_train)

nb_wf <- workflow() %>%
  add_recipe(noticias_receta) %>%
  add_model(nb_spec)


nb_rs <- fit_resamples(
  nb_wf,
  noticias_folds,
  control = control_resamples(save_pred = TRUE)
)

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)

nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Category, .pred_Fake) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for fake news",
    subtitle = "Each resample fold is shown in a different color"
  )


conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")



obs <-  "El perro triste de Trump muere en atentado terrorista con los iluminati"






null_classification <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

