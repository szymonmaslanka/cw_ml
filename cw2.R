# Wstęp
library(tidymodels) 
library(skimr) 
library(GGally) 
library(openair)
library(ggpubr)

tidymodels_prefer()

mydata

air <- mydata |> selectByDate(year = 2002) 
air |> skim()

air <- air |> na.omit()

set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggpairs()

set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggplot(aes(nox, no2)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~ x) + 
  stat_cor(label.x = 10, label.y = 80) + 
  stat_regline_equation(label.x = 10, label.y = 82) +
  theme_bw()

air |>    
  ggplot(aes(date, o3)) +     
  geom_line() +     
  theme_bw()

air |> 
  pull(o3) |> 
  range()  

air <-
  air |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")
  ))

air |> count(ozone)

air |>
  count(ozone) |>
  mutate(prop = n/sum(n))

air |> glimpse()

# Cw2
# 1. Wczytanie i przygotowanie danych
air <- mydata |>
  selectByDate(year = 2002) |>
  na.omit() |>
  mutate(o3 = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")),
    o3 = factor(o3),
    lubridate::as_date(date)
  )
air |> count(o3)
air
air |>
  count(o3) |>
  mutate(prop = n/sum(n))

# 2. Podział na zbiór treningowy/testowy ze stratyfikacją względem klasy
data_split <- initial_split(air, strata = o3)
train_data <- training(data_split)
test_data <- testing(data_split)


# 3. Recipe: transformacje, normalizacja, ekstrakcja z daty
# recipe bez date,wd,pm10,pm25,so2,co
ozone_recipe <- recipe(o3 ~  ws + no2 + nox, data = train_data) |>
  update_role(nox,new_role = "ID") |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#Uwzględnienie pozostałych zmiennych
ozone_recipe2 <- recipe(o3 ~ wd + ws + no2 + nox+date+pm10+pm25+so2+co, data = train_data) |>
  step_date(date, features = c("dow", "month")) |>  # wyodrębnienie cech czasowych
  update_role(nox,new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#bez podziału daty
ozone_recipe3 <- recipe(o3 ~ wd + ws + no2 + nox+date+pm10+pm25+so2+co, data = train_data) |>
  update_role(nox,new_role = "ID") |>
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

#bez daty
ozone_recipe4 <- recipe(o3 ~ wd + ws + no2 + nox+pm10+pm25+so2+co, data = train_data) |>
  update_role(nox,new_role = "ID") |>
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
  bind_cols(test_data |> select(o3))

ozone_pred2 <- predict(ozone_fit2, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit2, test_data)) |> 
  bind_cols(test_data |> select(o3))

ozone_pred3 <- predict(ozone_fit3, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit3, test_data)) |> 
  bind_cols(test_data |> select(o3))

ozone_pred4 <- predict(ozone_fit4, test_data, type = "prob") |> 
  bind_cols(predict(ozone_fit4, test_data)) |> 
  bind_cols(test_data |> select(o3))

ozone_pred
ozone_pred2
ozone_pred3
ozone_pred4

# 8. Wykres ROC

ozone_pred |>
  roc_curve(truth = o3, .pred_Niskie) |>
  autoplot()

ozone_pred2 |>
  roc_curve(truth = o3, .pred_Niskie) |>
  autoplot()

ozone_pred3 |>
  roc_curve(truth = o3, .pred_Niskie) |>
  autoplot()

ozone_pred4 |>
  roc_curve(truth = o3, .pred_Niskie) |>
  autoplot()

ozone_pred |> 
  roc_auc(truth=o3, .pred_Niskie)

ozone_pred2 |> 
  roc_auc(truth=o3, .pred_Niskie)

ozone_pred3 |> 
  roc_auc(truth=o3, .pred_Niskie)

ozone_pred4 |> 
  roc_auc(truth=o3, .pred_Niskie)

# Ocena

# Model regresji ma dobrą jakość w każdym przypadku ponad 80% jest dobrze rozpoznane
# zmienne wd, pm10, pm25, so2, co nie wnoszą zawiele do modelu jedynie poprawiają jego jakość o kilka procent
# zmienna date nie wpływa na model jednak po wyizolowaniu ważnych predyktorów czyli dni tygodnia i miesięcy model znacznie się poprawia
# no2 i nox są istotnymi danymi ale są mocno skorelowane więc jednej z nich (nox) przypisuje role "ID"
# Normalizacja zmiennych numerycznych jest potrzebna ponieważ mają one różne jednostki jednak powoduje niewielkie różnice w wynikach
# nie trzeba użyć YeoJohnson, nie ma potrzeby zmiany rozkładu