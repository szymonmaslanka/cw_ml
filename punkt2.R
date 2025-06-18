library(skimr)
library(nycflights13)
library(tidymodels)
tidymodels_prefer()

?flights
?weather

set.seed(123)
flights_data <-
  flights |>
  mutate(
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = lubridate::as_date(time_hour)
  ) |>
  inner_join(weather, by = c("origin", "time_hour")) |>
  select(dep_time,
         flight,
         origin,
         dest,
         air_time,
         distance,
         carrier,
         date,
         arr_delay,
         time_hour) |>
  na.omit() |>
  mutate_if(is.character, as.factor)

flights_data |>
  count(arr_delay)

flights_data |>
  count(arr_delay) |>
  mutate(prop = n/sum(n))

flights_data |> glimpse()

flights_data |>
  skimr::skim()

set.seed(222)
data_split <- initial_split(data = flights_data, prop = 3/4)
train_data <- training(data_split)
test_data <-  testing(data_split)

flights_rec <-
  recipe(arr_delay ~., data = train_data)

flights_rec <-
  recipe(arr_delay ~ ., data = train_data) |>
  update_role(flight, time_hour, new_role = "ID")

flights_rec |> summary()

flights_data |>
  distinct(date) |>  # zwrava wartości unikalne daty (bez powtrzeń)
  mutate(date = as.numeric(date))

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) |> 
  update_role(flight, time_hour, new_role = "ID") |> 
  step_date(date, features = c("dow", "month")) |> 
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = F)

flights_rec |> summary()

flights_rec |> prep() |> bake(train_data) |> _[1:10,] |> DT::datatable()

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) |> 
  update_role(flight, time_hour, new_role = "ID") |> 
  step_date(date, features = c("dow", "month")) |> 
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = F) |> 
  step_dummy(all_nominal_predictors())

flights_rec |> summary()

test_data |> 
  distinct(dest) |> 
  anti_join(train_data)

flights_rec <-
  recipe(arr_delay ~ ., data = train_data) |>
  update_role(flight, time_hour, new_role = "ID") |>
  step_date(date, features = c("dow", "month")) |>
  step_holiday(date,
               holidays = timeDate::listHolidays("US"),
               keep_original_cols = F) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

flights_rec |> prep()

lr_mod <- 
  logistic_reg() |> 
  set_engine("glm")

logi_work <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(flights_rec)

logi_work

logi_fit <-  
  logi_work |> 
  fit(data = train_data)


flights_rec |> summary()

logi_fit |> 
  extract_fit_parsnip() |> 
  tidy()

predict(logi_fit, test_data)

predict(logi_fit, test_data, type = "prob")

pred_test <- 
  augment(logi_fit, test_data) |>
  select(-dest,
         -flight,
         -origin,
         -dep_time,
         -air_time,
         -distance,
         -carrier,
         -date)
pred_test

pred_test  |> 
  roc_curve(truth = arr_delay, .pred_late) |> 
  autoplot()

pred_test |> 
  roc_auc(truth = arr_delay, .pred_late)

pred_test |> summary()
