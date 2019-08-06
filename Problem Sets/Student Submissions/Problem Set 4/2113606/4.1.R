# Problem Set 4
# Question 1


library(tidyverse)
library(ggplot2)
library(tidymodels)
library(rsample)

#a
#Load data and visualize using a stat_hex_bin plot (x-loc, y-loc, success)
data_import <- read.csv2("~/ADS19/Problem Sets/04/basketball_complete.csv", sep = ";", dec = ",")

data_import %>%
  ggplot(aes(loc_x, loc_y)) +
  stat_bin_hex()-> data_vis
head(data_import)
#b
#recipe to preprocess (imputation, transformation, cleaning, dummy, interactions, normalization, multivariate transformations)
#split 80/20

  data_split <- initial_split(data_import, prop = 0.8, strata = "default")
  data_train <- training(data_split)
  data_test <- testing(data_split)

model_recipe <- recipe(default ~., data = data_train)
summary(model_recipe)

model_recipe_steps <- model_recipe %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_predictors()) %>%
  step_scale(all_numeric())

#c
#predict outcome of shots with 3 classification models
#evaluate models

prepped_recipe <- prep(model_recipe_steps, training = data_train)

data_train_preprocessed = bake(prepped_recipe, data_train)
data_test_preprocessed = bake(prepped_recipe, data_test)

#Build models:

logistic_glm <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(default ~., data = data_train_preprocessed)

rf_mod <-
  rand_forest(mode = "classification", trees = 250) %>%
  set_engine("ranger") %>%
  fit(default ~., data = data_train_preprocessed)

boost_md <-
  boost_tree(mode = "classification",
             trees = 1500,
             mtry = 3,
             learn_rate = 0.03,
             sample_size = 1,
             tree_depth = 5) %>%
  set_engine("xgboost") %>%
  fit(default ~., data = data_train_preprocessed)

#Predictions:

predictions_glm <- logistic_glm %>%
  predict(new_data = data_test_preprocessed) %>%
  bind_cols(data_test_preprocessed %>% dplyr::select(default))

predictions_rf <- rf_mod %>%
  predict(new_data = data_test_preprocessed) %>%
  bind_cols(data_test_preprocessed %>% dplyr::select(default))

predictions_boost <- boost_mod %>%
  predict(new_data = data_test_preprocessed) %>%
  bind_cols(data_test_preprocessed %>% dplyr::select(default))

#Evaluation:
predictions_glm %>%
  conf_mat(default, .pred_class)

predictions_glm %>%
  conf_mat(default, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_rf %>%
  conf_mat(default, .pred_class)

predictions_rf %>%
  conf_mat(default, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_boost %>%
  conf_mat(default, .pred_class)

predictions_boost %>%
  conf_mat(default, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))




