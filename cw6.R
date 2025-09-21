library(tidymodels)
library(glmnet)
library(ranger)
library(rpart)
library(vip)
library(GGally)
library(ggplot2)
library(purrr)
library(openair)
library(skimr)
library(rpart.plot)

pkg = c(
  "tidymodels",
  "glmnet",
  "ranger",
  "rpart",
  "readr",
  "tidymodels",
  "vip",
  "ggthemes",
  "openair",
  "gt"
)

pkg |> 
  purrr::map(.f = ~ require(.x, character.only = T)) ; rm(pkg)

tidymodels_prefer()

importMeta(source = "aurn") |> knitr::kable()

dane <- importAURN(site = "kc1", year = 2021)



skimr::skim(dane)

dane <- dane |> 
  select(o3, nox, no2, no, ws, wd, air_temp) |> 
  na.omit()

dane
wd_factor <- function(dane, wd, name = "short", ...){

  
  rose_breaks <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
  
  if (name == "long") {
    rose_labs <- c(
      "North", "North-Northeast", "Northeast", "East-Northeast",
      "East",  "East-Southeast",  "Southeast", "South-Southeast",
      "South", "South-Southwest", "Southwest", "West-Southwest",
      "West",  "West-Northwest",  "Northwest","North-Northwest",
      "North"
    )
    
    
  } else if (name == "short") {
    
    rose_labs <- c( 
      "N", "NNE", "NE", "ENE",
      "E", "ESE", "SE", "SSE",
      "S", "SSW", "SW", "WSW",
      "W", "WNW", "NW", "NNW",
      "N"
    )
    
  }
  
  
  dane <- dane %>%
    mutate(wd_cardinal =
             cut({{wd}},
                 breaks = rose_breaks,
                 labels = rose_labs,
                 right = F,
                 include.lowest = T),
           ...	 
    )
  
  return(dane) 
}

dane2 <- dane |> wd_factor(wd = wd)

dane2

dane2 |> select(-wd, -wd_cardinal) |> 
  GGally::ggpairs()

dane2 |> select(-wd, -wd_cardinal, -no2, -nox) |> 
  GGally::ggpairs()

set.seed(321)
spl <- initial_split(dane, prop = 0.8)
train <- training(spl)
test  <- testing(spl)

rec <- recipe(o3 ~ ., data = train) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

glm_spec <- linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet")

tree_spec <- decision_tree(cost_complexity = tune(), min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
  set_engine("ranger") |>
  set_mode("regression")

wf_glm  <- workflow() |> add_model(glm_spec)  |> add_recipe(rec)
wf_tree <- workflow() |> add_model(tree_spec) |> add_recipe(rec)
wf_rf   <- workflow() |> add_model(rf_spec)   |> add_recipe(rec)

set.seed(111)
folds <- vfold_cv(train, v = 5)
reg_metrics <- metric_set(rmse, rsq)

glm_grid <- grid_regular(
  penalty(range = c(-5, 0)),
  mixture(range = c(0, 1)),
  levels = c(penalty = 10, mixture = 5)
)

tree_grid <- grid_regular(
  cost_complexity(range = c(-6, -1)),
  min_n(range = c(2, 50)),
  levels = c(cost_complexity = 6, min_n = 6)
)

rf_grid <- grid_regular(
  mtry(range = c(1, 2)),
  min_n(range = c(2, 4)),
  levels=3
)

set.seed(333)
glm_res <- tune_grid(wf_glm, resamples = folds, grid = glm_grid, metrics = reg_metrics)
set.seed(334)
tree_res <- tune_grid(wf_tree, resamples = folds, grid = tree_grid, metrics = reg_metrics)
set.seed(335)
rf_res <- tune_grid(wf_rf, resamples = folds, grid = rf_grid, metrics = reg_metrics)

tree_best <- select_best(tree_res, metric = "rmse")
glm_best  <- select_best(glm_res, metric = "rmse")
rf_best   <- select_best(rf_res, metric = "rmse")

final_glm  <- finalize_workflow(wf_glm, glm_best) |> fit(data = train)
final_tree <- finalize_workflow(wf_tree, tree_best) |> fit(data = train)
final_rf   <- finalize_workflow(wf_rf, rf_best) |> fit(data = train)

pred_glm  <- predict(final_glm, test)  |> bind_cols(test |> select(o3))
pred_tree <- predict(final_tree, test) |> bind_cols(test |> select(o3))
pred_rf   <- predict(final_rf, test)   |> bind_cols(test |> select(o3))

res_glm  <- pred_glm  |> metrics(truth = o3, estimate = .pred)
res_tree <- pred_tree |> metrics(truth = o3, estimate = .pred)
res_rf   <- pred_rf   |> metrics(truth = o3, estimate = .pred)

results_table <- bind_rows(
  res_glm  |> mutate(model = "GLMNET"),
  res_tree |> mutate(model = "RPART"),
  res_rf   |> mutate(model = "RANGER")
) |> select(model, .metric, .estimate)

print(results_table)

preds_all <- bind_rows(
  pred_glm  |> mutate(model = "GLMNET"),
  pred_tree |> mutate(model = "RPART"),
  pred_rf   |> mutate(model = "RANGER")
)

ggplot(preds_all, aes(x = o3, y = .pred, color = model)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ model) +
  labs(
    title = "Predykcja O3 – porównanie modeli",
    x = "Rzeczywiste O3",
    y = "Przewidywane O3"
  ) +
  theme_minimal()

#Ranger najlepszy