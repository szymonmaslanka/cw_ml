library(tidymodels)

# Dodatkowe pakiety
library(rpart.plot)  # wizualizacja drzew decyzyjnych 
library(vip)         # wykres wagi zmiennych

data("cells", package = "modeldata")
cells

set.seed(123)
split <- initial_split(data = cells |> select(-case), 
                       prop = 3/4, 
                       strata = class)

train <- training(split)
test <- testing(split)

tune_spec <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune()) |> 
  set_engine("rpart") |> 
  set_mode("classification")

tune_spec

siatka <- grid_regular(cost_complexity(), 
                       tree_depth(), 
                       levels = 5)
siatka

# podgląd parametrów 

siatka |> 
  count(tree_depth)

siatka |> 
  count(cost_complexity)

set.seed(234)
folds <- vfold_cv(train)

set.seed(345)

# workflow

work <- 
  workflow() |> 
  add_model(tune_spec) |> 
  add_formula(class ~ .)

# statystyki oceny dokładnosci modelu 

miary_oceny <-
  yardstick::metric_set(# tym parametrem możesz definiować
    accuracy,
    mcc,
    npv,
    roc_auc)

# Optymalizacja 

fit_tree <-
  work |>
  tune_grid(
    resamples = folds,
    grid = siatka,
    metrics = miary_oceny
  )

fit_tree

fit_tree |> collect_metrics()

fit_tree %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

show_best(fit_tree, metric = "accuracy")

select_best(fit_tree, metric = "accuracy")

best_mod <- select_best(fit_tree, metric = "accuracy")

final_mod <-  
  work |> 
  finalize_workflow(best_mod)

final_fit <- 
  final_mod |> 
  last_fit(split = split)

final_fit %>%
  collect_metrics()

final_fit |> 
  collect_predictions() |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

final_fit |> extract_workflow()

final_fit |> 
  extract_workflow() |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = F)

# wykres 

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

args(decision_tree)
 
?decision_tree()
