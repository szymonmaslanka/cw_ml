#Cw 4
# Można dostroić min_n – minimalna liczba obserwacji w węźle wymaganych do jego podziału.
# min_n kontroluje minimalną liczbę przykładów, które muszą być obecne w węźle, aby mógł on zostać podzielony.
# Mniejsze wartości powodują, że drzewo rośnie bardziej szczegółowo (większe ryzyko przeuczenia).
# Większe wartości prowadzą do bardziej ogólnych podziałów (mniejsze ryzyko przeuczenia, ale większe ryzyko niedouczenia).


library(ranger)
library(modeldata)
library(tidymodels)
library(skimr) 
library(GGally) 
library(openair)
library(ggpubr)
library(tidymodels)
library(rpart.plot)
library(vip)  
tidymodels_prefer()
# Części Cw2 i Cw3
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

lr_mod <- 
  rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

set.seed(234)
ozone_wf <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(ozone_recipe)

ozone_fit <-  
  ozone_wf |> 
  fit(data = train_data)

ozone_pred <- predict(ozone_fit, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit, test_data)) |> 
  bind_cols(test_data |> select(ozone))

# Cw 5
# Są 3 hiper-parametry do optymalizacji mtry,trees,min_n

tune_spec <- 
  rand_forest(
    trees = tune(),
    min_n = tune()) |> 
  set_engine("ranger",importance = "impurity") |> 
  set_mode("classification")

tune_spec

siatka <- grid_regular(min_n(),
                       trees(),
                       levels = 6)
# podgląd parametrów 
siatka

siatka |> 
  count(trees)
siatka |> 
  count(min_n)

set.seed(234)
folds <- vfold_cv(train_data,strata=ozone)

set.seed(345)

# workflow

ozone_wf <- 
  workflow() |> 
  add_model(tune_spec) |> 
  add_recipe(ozone_recipe)

# statystyki oceny dokładnosci modelu 

miary_oceny <-
  yardstick::metric_set(# tym parametrem możesz definiować
    accuracy,
    mcc,
    npv,
    roc_auc)

# Optymalizacja 

fit_tree <-
  ozone_wf |>
  tune_grid(
    resamples = folds,
    grid = siatka,
    metrics = miary_oceny
  )

fit_tree

fit_tree %>%
  collect_metrics() %>%
  mutate(trees = factor(trees)) %>%
  ggplot(aes(min_n, mean, color = trees)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

show_best(fit_tree, metric = "accuracy")

select_best(fit_tree, metric = "accuracy")

final_mod <-  
  ozone_wf |> 
  finalize_workflow(best_mod)

final_fit <- 
  final_mod |> 
  last_fit(split = data_split)

final_fit %>%
  collect_metrics()

final_fit |> 
  collect_predictions() |> 
  roc_curve(truth = ozone, .pred_Niskie) |> 
  autoplot()

final_fit |> extract_workflow()

final_fit |> 
  extract_workflow() |> 
  extract_fit_parsnip() |>
  vip() 

# eksport danych do tabeli

final_fit |>
  extract_workflow() |>
  extract_fit_parsnip() |>
  vip() |> 
  _$data |> 
  knitr::kable(digits = 1)
