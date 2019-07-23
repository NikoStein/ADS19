# Problem Set 4
# Question 1


library(tidyverse)
library(recipes)
library(tidymodels)
library(rsample)
library(caret)
library(parsnip)

# Aufgabe 1 ----
## Set WD to project src

readAllShots <- function() {
  path <- './Problem Sets/04/basketball_complete.csv'
  read.csv2(path)
}
# call
allShots <- readAllShots()

allShots %>%
  ggplot(aes(x = loc_x, y = loc_y, z = shot_made_flag)) +
  stat_bin_hex(aes(fill = ..ndensity..), bins = 30) +
  stat_summary_hex(fun = "mean" , colour = 'grey') +
  guides(alpha = FALSE, size = FALSE) +
  theme_bw()

# Aufgabe 2 ----

# 1. Check-Out DataSet
# ways to deal with missing data 1. delete row 2. mean value impute (gsub)
nrows <- nrow(allShots)
nrows
ncompletes <- sum(complete.cases(allShots))
ncompletes
perCompletes <<- ncompletes / nrows
attributes(allShots$shot_type)
# 1.1. Split data into train and test
# 2. Normalize/Create Dummy Variables: shot_type
# 3. Transform data: seconds and minutes can be aggregated
# 4. Drop columns unnecessary: season, combinded_shot_type, minutes and seconds_remaining
# 5. Filter noise in Data (not recommended) so ignored
## use summary to get highest or lowest values

allShotsMutated <- allShots %>%
  mutate(
    time_left_in_sec = seconds_remaining + (minutes_remaining * 60),
    seconds_remaining = NULL,
    minutes_remaining = NULL,
    season = NULL,
    combined_shot_type = NULL,
    shot_made_flag = as.factor(shot_made_flag),
  )

allShots_split = initial_split(allShotsMutated, prop = 4 / 5)
allShots_train = training(allShots_split)
allShots_test = testing(allShots_split)

model_recipe <-
  recipe(shot_made_flag ~ .,
         data = allShots_train)

model_recipe_steps <- model_recipe %>%
  step_log(shot_made_flag) %>%
  step_dummy(shot_made_flag) %>%
  step_num2factor(shot_type) %>%
  step_center(all_predictors()) %>%
  step_scale(all_numeric()) %>%
  step_nzv(all_predictors())

model_recipe_steps

prepped_recipe <-
  prep(model_recipe, training = allShots_train, retain = TRUE)
prepped_recipe

allShots_train_preprocessed <- bake(prepped_recipe, allShots_train)
allShots_test_preprocessed <- bake(prepped_recipe, allShots_test)

allShots_train_preprocessed

# Aufgabe 3

logistic_glm <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(shot_made_flag ~ ., data = allShots_train_preprocessed)

predictions_glm <- logistic_glm %>%
  predict(new_data = allShots_test_preprocessed) %>%
  bind_cols(allShots_test_preprocessed %>% dplyr::select(shot_made_flag))

predictions_glm %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))


#Die Interpretation wäre eben,
#dass du 99.5% der Shots richtig vorhersagst (accuracy).
#Der MCC misst die Korrelation zwischen deinen Predictions
#und der Realität und ist bei 1 auch maximal (also hier auch sehr gut).
#Der ist etwas schwerer zu interpretieren aber
#eben robust gegen ungleiche Klassen.

