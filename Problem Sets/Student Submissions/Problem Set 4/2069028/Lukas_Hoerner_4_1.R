# Problem Set 4
# Question 1

library(tidyverse)
library(readr)
library(ranger)
library(rsample)
library(tidymodels)
library(lubridate)
library(parsnip)
bb <- read_csv2("Problem Sets/04/basketball_complete.csv")
#a) ggplot mit drei Variablen
bb %>%
  ggplot(aes(x = loc_x, y = loc_y, z = shot_made_flag))+
  #stat_summary_2d() = 2d "Kästchen" Plot
  stat_summary_hex() #Plot mit Hexagons


# b)

bb %>%
  mutate(seconds = seconds_remaining + 60*minutes_remaining) %>% #abhängige Variablen Minuten + Sekunden
  mutate(lastsecond = ifelse(seconds < 3, 1, 0)) %>% #für "Notversuche"
  mutate(zone = ifelse(loc_x*loc_x < 6500 & loc_y < 190, 1, 0)) -> bbmore #%>% Würfe mit guter Position (Zone)
  #mutate(season = as.numeric(substr(season,1,nchar(season)-3))) #probiert, aber hat Modell verschlechtert
bb_split <- initial_split(bbmore, prop = 0.80, strata = "shot_made_flag")
bb_train <- training(bb_split)
bb_test <- testing(bb_split)

#variablen für reicpe festlegen
bb_recipe <- recipe(shot_made_flag ~ period + playoffs + shot_distance + zone + combined_shot_type + shot_type + lastsecond, data = bb_train)
summary(bb_recipe)
# prepare recipe: all steps -> https://www.rdocumentation.org/packages/recipes/versions/0.1.5
bb_recipe %>%
  step_other(period, threshold = 0.05, other = 5) %>% #combine shots in overtime
  step_num2factor(shot_made_flag, playoffs, lastsecond) %>%
  step_dummy(playoffs, period, lastsecond, zone, role = "predictor") %>%
  step_dummy(all_predictors(), -all_numeric()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors(), all_numeric())  -> bb_recipe_steps
summary(bb_recipe_steps)

prepped_recipe_bb <- prep(bb_recipe_steps, training = bb_train)
prepped_recipe_bb

bb_train_preprocessed <- bake(prepped_recipe_bb, bb_train)
bb_test_preprocessed <- bake(prepped_recipe_bb, bb_test)
bb_train_preprocessed
#------------------------------------------------------------------
#logistic glm
logistic_glm <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(shot_made_flag ~ ., data = bb_train_preprocessed)

glmn <-
  logistic_reg(penalty = 0.005, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  fit(shot_made_flag ~ ., data = bb_train_preprocessed)
glmn

rf_mod <-
  rand_forest(
    mode = "classification",
    trees = 150) %>%
  set_engine("ranger") %>%
  fit(shot_made_flag ~ ., data = bb_train_preprocessed)

boost_mod <-
  boost_tree(mode = "classification",
             trees = 200,
             mtry = 5,
             learn_rate = 0.05,
             sample_size = 1,
             tree_depth = 2) %>%
  set_engine("xgboost") %>%
  fit(shot_made_flag ~ ., data = bb_train_preprocessed)

predictions_glm <- logistic_glm %>%
  predict(new_data = bb_test_preprocessed) %>%
  bind_cols(bb_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_glmnet <- glmn %>%
  predict(new_data = bb_test_preprocessed) %>%
  bind_cols(bb_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_rf <- rf_mod %>%
  predict(new_data = bb_test_preprocessed) %>%
  bind_cols(bb_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_boost <- boost_mod %>%
  predict(new_data = bb_test_preprocessed) %>%
  bind_cols(bb_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_glm %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_glmnet %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_rf %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_boost %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas", "auc"))

predictions_boost_prob <- boost_mod %>%
  predict(new_data = bb_test_preprocessed, type = "prob") %>%
  bind_cols(bb_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_boost_prob %>%
  roc_curve(shot_made_flag, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

predictions_boost_prob %>%
  roc_auc(shot_made_flag, .pred_0)

#__________________________________________________________________
