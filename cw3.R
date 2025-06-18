library(ranger)
library(modeldata)
library(tidymodels)
library(skimr) 
library(GGally) 
library(openair)
library(ggpubr)
tidymodels_prefer()
# Cw2
# 1. Wczytanie i przygotowanie danych
air <- mydata |>
  selectByDate(year = 2002) |>
  na.omit() |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")),
    lubridate::as_date(date)
  )
air |> count(ozone)
air
air |>
  count(ozone) |>
  mutate(prop = n/sum(n))

# 2. Podział na zbiór treningowy/testowy ze stratyfikacją względem klasy
data_split <- initial_split(air, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)


# 3. Recipe: transformacje, normalizacja, ekstrakcja z daty
# recipe bez date,wd,pm10,pm25,so2,co
ozone_recipe <- recipe(ozone ~  ws + no2 + nox + o3, data = train_data) |>
  update_role(nox,o3,new_role = "ID") |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#Uwzględnienie pozostałych zmiennych
ozone_recipe2 <- recipe(ozone ~ wd + ws + no2 + nox+date+pm10+pm25+so2+co+o3, data = train_data) |>
  step_date(date, features = c("dow", "month")) |>  # wyodrębnienie cech czasowych
  update_role(nox,o3,new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#bez podziału daty
ozone_recipe3 <- recipe(ozone ~ wd + ws + no2 + nox+date+pm10+pm25+so2+co+o3, data = train_data) |>
  update_role(nox,o3,new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#bez daty
ozone_recipe4 <- recipe(ozone ~ wd + ws + no2 + nox+pm10+pm25+so2+co+o3, data = train_data) |>
  update_role(nox,o3,new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

ozone_recipe |> summary()
ozone_recipe2 |> summary()

ozone_recipe |> prep() |> bake(train_data) |> _[1:10,] |> DT::datatable()

# 4. Model regresji logistycznej
lr_mod <- 
  logistic_reg() |> 
  set_engine("glm")

# 5. Workflow
ozone_wf <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe)

ozone_wf2 <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe2)

ozone_wf3 <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe3)

ozone_wf4 <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe4)

ozone_wf
ozone_wf2

# 6. Trenowanie modelu
ozone_fit <-  
  ozone_wf |> 
  fit(data = train_data)

ozone_fit2 <-  
  ozone_wf2 |> 
  fit(data = train_data)

ozone_fit3 <-  
  ozone_wf3 |> 
  fit(data = train_data)

ozone_fit4 <-  
  ozone_wf4 |> 
  fit(data = train_data)

ozone_recipe |> summary()

ozone_fit |> 
  extract_fit_parsnip() |> 
  tidy()

ozone_recipe2 |> summary()

ozone_fit2 |> 
  extract_fit_parsnip() |> 
  tidy()

ozone_fit3 |> 
  extract_fit_parsnip() |> 
  tidy()

ozone_fit4 |> 
  extract_fit_parsnip() |> 
  tidy()

# 7. Predykcje i ocena modelu
ozone_pred <- predict(ozone_fit, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, test_data)) |> 
  bind_cols(test_data |> select(ozone))

ozone_pred_train <- predict(ozone_fit, train_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, train_data)) |> 
  bind_cols(train_data |> select(ozone))

ozone_pred2 <- predict(ozone_fit2, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit2, test_data)) |> 
  bind_cols(test_data |> select(ozone))

ozone_pred3 <- predict(ozone_fit3, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit3, test_data)) |> 
  bind_cols(test_data |> select(ozone))

ozone_pred4 <- predict(ozone_fit4, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit4, test_data)) |> 
  bind_cols(test_data |> select(ozone))

ozone_pred
ozone_pred2
ozone_pred3
ozone_pred4

# 8. Wykres ROC

ozone_pred |>
  roc_curve(truth = ozone, .pred_Niskie) |>
  autoplot()
ozone_pred_train |>
  roc_curve(truth = ozone, .pred_Niskie) |>
  autoplot()

ozone_pred2 |>
  roc_curve(truth = ozone, .pred_Niskie) |>
  autoplot()

ozone_pred3 |>
  roc_curve(truth = ozone, .pred_Niskie) |>
  autoplot()

ozone_pred4 |>
  roc_curve(truth = ozone, .pred_Niskie) |>
  autoplot()

ozone_pred |> 
  roc_auc(truth=ozone, .pred_Niskie)
ozone_pred_train |> 
  roc_auc(truth=ozone, .pred_Niskie)

ozone_pred2 |> 
  roc_auc(truth=ozone, .pred_Niskie)

ozone_pred3 |> 
  roc_auc(truth=ozone, .pred_Niskie)

ozone_pred4 |> 
  roc_auc(truth=ozone, .pred_Niskie)

# Cw3
#CV
set.seed(345)
folds <- vfold_cv(data = train_data, v = 10)
folds$splits[[1]] %>% analysis() %>% dim()
folds


ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)


resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)
  
bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# V-krotna
folds <- vfold_cv(data = train_data, v = 10,repeats = 10)
folds
ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)
ozone_re

resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)

bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# Bootstrap
folds <- bootstraps(train_data, times = 10)
folds
ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)
ozone_re

resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)

bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# las losowy
#CV
lr_mod <- 
  rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

set.seed(234)
ozone_wf <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe)

set.seed(345)
folds <- vfold_cv(data = train_data, v = 10)
folds$splits[[1]] %>% analysis() %>% dim()
folds


ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)

ozone_fit <-  
  ozone_wf |> 
  fit(data = train_data)

ozone_pred <- predict(ozone_fit, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, test_data)) |> 
  bind_cols(test_data |> select(ozone))

resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)

bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# V-krotna
folds <- vfold_cv(data = train_data, v = 10,repeats = 10)
folds
ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)

ozone_fit <-  
  ozone_wf |> 
  fit(data = train_data)

ozone_pred <- predict(ozone_fit, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, test_data)) |> 
  bind_cols(test_data |> select(ozone))

resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)

bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# Bootstrap
folds <- bootstraps(train_data, times = 10)
folds
ozone_re <- 
  ozone_wf |> 
  fit_resamples(folds)

ozone_fit <-  
  ozone_wf |> 
  fit(data = train_data)

ozone_pred <- predict(ozone_fit, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, test_data)) |> 
  bind_cols(test_data |> select(ozone))

resultsample <- ozone_re |> 
  collect_metrics()

resultnormal <- 
  ozone_pred |>
  roc_auc(truth = ozone, .pred_Niskie)

bind_rows(resultsample, resultnormal) |>
  knitr::kable(digits = 3)

# Komentarz

# Wszystkie wyniki przy użyciu próbkowania czy to przy regresji
# logistycznej czy lesie losowym są niewiele niższe od zwykłego działania na zbiorze treningowym i testowym
# jednak jest większe prawdopodobieństwo że działanie ich przewidywania jest dokładniejsze dzięki próbkowaniu.
# Przez to że zbiory są losowane wyniki w różnych podejściach się różnią i dodatkowo walidacje krzyżowe dają podobne wyniki
# a bootstrap daje mniejszą dokładność co może być spowodowane randomowością powtórzonych danych. V-krotna walidacja powinna
# dać najbardziej prawdopodobne wyniki przez różne konfiguracje przetwarzanych danych.