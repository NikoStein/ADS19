# Problem Set 4
# Question 1


library(tidyverse)
library(hexbin)
library(rsample)
library(tidymodels)
library(ranger)
library(xgboost)

# a)
df <- read_csv2("basketball_complete.csv")

df %>%
  group_by(loc_x, loc_y) %>%
  summarise(sum_weight = sum(shot_made_flag), count = n(), value = sum_weight/count) %>%
  arrange(desc(sum_weight)) %>%
  ggplot(mapping = aes(x = loc_x, y = loc_y, z = value)) +
  stat_summary_hex() +
  theme_bw()

# b)

df %>%
  mutate(combined_shot_type = as.factor(combined_shot_type),
         period = as.factor(period),
         playoffs = as.factor(playoffs),
         shot_type = as.factor(shot_type),
         season = as.factor(season),
         shot_made_flag = as.factor(shot_made_flag)) %>%
  select(-loc_x) %>%
  select(-loc_y) -> df

vapply(df, function(x) mean(!is.na(x)), numeric(1))
#--> no missing values

df_split <- initial_split(df, prop = 0.8, strata = "shot_made_flag")
df_train <- training(df_split)
df_test <- testing(df_split)

model_recipe <- recipe(shot_made_flag ~., data = df_train)
summary(model_recipe)


model_recipe %>%
  step_dummy(combined_shot_type, period, playoffs, season, shot_type) %>%
  step_scale(all_numeric()) %>%
  step_nzv(all_predictors()) -> model_recipe_steps

prep(model_recipe_steps, training = df_train) -> prepped_recipe

df_train_preprocessed <- bake(prepped_recipe, df_train)

df_test_preprocessed <- bake(prepped_recipe, df_test)


# LOgistic Regression

logistic_glm <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(shot_made_flag ~., data = df_train_preprocessed)

predictions_glm <- logistic_glm %>%
  predict(new_data = df_test_preprocessed) %>%
  bind_cols(df_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_glm %>%
  conf_mat(shot_made_flag, .pred_class)

predictions_glm %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))


# Random Forrest

rf_mod <-
  rand_forest(
    mode = "classification",
    trees = 250) %>%
  set_engine("ranger") %>%
  fit(shot_made_flag ~ ., data = df_train_preprocessed)

predictions_rf <- rf_mod %>%
  predict(new_data = df_test_preprocessed) %>%
  bind_cols(df_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_rf %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

# Boosted Tree

boost_mod <-
  boost_tree(mode = "classification",
             trees = 1500,
             mtry = 3,
             learn_rate = 0.03,
             sample_size = 1,
             tree_depth = 5) %>%
  set_engine("xgboost") %>%
  fit(shot_made_flag ~ ., data = df_train_preprocessed)

predictions_boost <- boost_mod %>%
  predict(new_data = df_test_preprocessed) %>%
  bind_cols(df_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_boost %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))


predictions_boost %>%
  metrics(truth = shot_made_flag, estimate = .pred)

