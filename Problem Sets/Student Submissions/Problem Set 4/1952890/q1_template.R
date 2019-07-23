# Problem Set 4
# Question 1


library(tidyverse)
library(tidymodels)

read_csv2('basketball_complete.csv') -> basketball
basketball

# Visualisierung (fill-Argument ist Erfolgsrate)
# ggplot(basketball, aes(loc_x, loc_y, fill=...)) +
#   stat_bin_hex()

train_test_split <- initial_split(basketball, prop = 0.8)
train_set <- training(train_test_split)
test_set <- testing(train_test_split)

train_set %>%
  recipe(shot_made_flag ~ combined_shot_type + loc_x + loc_y + minutes_remaining + period + playoffs + season + seconds_remaining + shot_distance + shot_type) %>%
  step_bin2factor(shot_made_flag) %>% # Convert independent variable/target as factor for classification
  step_knnimpute(all_predictors()) %>%
  step_dummy(all_predictors(), -all_numeric()) %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  check_missing(all_predictors()) %>%
  prep() -> prep_rec

train_set_baked <- prep_rec %>% juice() # juice() equals: bake(new_data = train_set)
test_set_baked <- prep_rec %>% bake(new_data = test_set)

logistic_reg(mode="classification") %>%
  set_engine('glm') %>%
  fit(shot_made_flag ~ ., data=train_set_baked) -> model_reg

model_reg %>%
  predict(new_data = test_set_baked) %>%
  bind_cols(truth = test_set_baked$shot_made_flag) -> preds

preds %>% metrics(truth, .pred_class)
preds %>% f_meas(truth, .pred_class)
preds %>% mcc(truth, .pred_class)

folds <- vfold_cv(data = train_set_baked, v = 5, strata = "shot_made_flag")
folds$splits$`1`
analysis(folds$splits$`1`)
assessment(folds$splits$`1`)

logistic_reg(mode="classification") %>%
  set_engine('glm') %>%
  fit(shot_made_flag ~ ., data=analysis(folds$splits$`1`)) %>%
  predict(new_data = assessment(folds$splits$`1`)) %>%
  bind_cols(truth = assessment(folds$splits$`1`)$shot_made_flag) %>%
  mcc(truth, .pred_class)

fit_and_predict <- function(model, split_data){
  model %>%
    fit(shot_made_flag ~ ., data=analysis(split_data)) %>%
    predict(new_data = assessment(split_data)) %>%
    bind_cols(truth = assessment(split_data)$shot_made_flag) %>%
    mcc(truth, .pred_class)
}

logistic_reg(mode="classification") %>%
  set_engine('glm') %>%
  list() %>%
  map2_df(folds$splits, fit_and_predict) -> res

res
mean(res$.estimate)
sd(res$.estimate)

logistic_reg(mode="classification") %>%
  set_engine('glm') %>%
  list() %>%
  map2_df(folds$splits, fit_and_predict) %>%
  pull() %>%
  mean()

svm_rbf(mode="classification") %>%
  set_engine('kernlab') %>%
  list() %>%
  map2_df(folds$splits, fit_and_predict) %>%
  pull() %>%
  mean()

rand_forest(mode="classification") %>%
  set_engine('ranger') %>%
  list() %>%
  map2_df(folds$splits, fit_and_predict) %>%
  pull() %>%
  mean()

boost_tree(mode="classification") %>%
  set_engine('xgboost') %>%
  list() %>%
  map2_df(folds$splits, fit_and_predict) %>%
  pull() %>%
  mean()
