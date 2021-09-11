pacman::p_load(tidyverse,  tidymodels, textrecipes, discrim, naivebayes)

#noticias_falsas <- read_csv("input/onlyfakes1000.csv") 

noticias_falsas <- read_excel("input/train.xlsx") 

complaints_split <- initial_split(noticias_falsas, strata = Category)

complaints_train <- training(complaints_split)
complaints_test <- testing(complaints_split)

dim(complaints_train)
dim(complaints_test)

complaints_rec <-
  recipe(Category ~  Text , data = complaints_train)


complaints_rec <- complaints_rec %>%
  step_tokenize(Text) %>%
  step_tokenfilter(Text, max_tokens = 1e3) %>%
  step_tfidf(Text)


complaint_wf <- workflow() %>%
  add_recipe(complaints_rec)

nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

nb_fit <- complaint_wf %>%
  add_model(nb_spec) %>%
  fit(data = complaints_train)

predict(nb_fit, complaints_test)

### Eval

set.seed(234)
complaints_folds <- vfold_cv(complaints_train)

complaints_folds

nb_wf <- workflow() %>%
  add_recipe(complaints_rec) %>%
  add_model(nb_spec)

nb_wf


nb_rs <- fit_resamples(
  nb_wf,
  complaints_folds,
  control = control_resamples(save_pred = TRUE)
)

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)


nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = Category, .pred_True) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "Each resample fold is shown in a different color"
  )


conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")
