# Problem Set 4
# Question 1


#1) The file basketball_complete.csv provides labelled data of over 25,000 basketball shots taken
#by a single player. The data is labelled and provides you with information if the shot was
#made or not.
#The variables are defined as follows:
  #• combined_shot_type: what kind of shot was taken (jump shot, lay-up, dunking)
  #• loc_x, loc_y: x and y coordinates of the shot relatie to the basket (this allows you to identify shot distancs as well as the relative position of the shot)
  #• minutes_remaining, seconds_remaining: how much time was left in the current period
  #• period: which period of the game
  #• playoffs: was the game a playoff game
  #• season: which season was the shot taken
  #• shot_type: was this a 2 point or a 3 point shot

library(tidyverse)
require(hexbin)

shots = read_csv2("Problem Sets/04/basketball_complete.csv")


#Your task is to train machine learning models to predict the outcome of the shots.

  #a.) Load the data and start your analysis by visualizing the shots. Illustrate the success
  #rate over the court as depicted in the plot below (use a stat_bin_hex plot in ggplot).

  shots %>%
    group_by(loc_x, loc_y) %>%
    mutate(rate = sum(shot_made_flag)/n()) %>%
    ggplot(aes(x = loc_x, y = loc_y, color = rate)) +
    stat_bin_hex() #erkennt rate nicht mit color oder fill


  # b.) Create a recipe to preprocess your data. Use data cleaning, imputation,
  #transformations, dummy variables, interactions, normalizations and multivariate
  #transformations as required and split the data set into 80% training and 20% test data.

    library(tidymodels)

    head(shots)
    str(shots)

    unique(shots$seconds_remaining)


    shots %>%
      select(-season, -loc_x, -loc_y) -> shots_prepared

    #Split data set
    shots_split <- initial_split(shots_prepared, prop = 0.8)
    shots_train <- training(shots_split)
    shots_test <- testing(shots_split)


    #nach NAs schauen
    colnames(shots)[colSums(is.na(shots)) > 0]
    #colnames(shots)[apply(shots, 2, anyNA)] #Variante 2
    #-> keine NAs vorhanden


    #Receipe:
    rec <- recipe(shot_made_flag ~ ., data = shots_train)

    summary(rec)

    rec %>%
      step_num2factor(shot_made_flag, period) %>% # make target variable a factor
      step_dummy(combined_shot_type, shot_type, period) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors()) -> rec # rescale all predictors


    prepped_recipe <- prep(rec, training = shots_train)
    prepped_recipe

    #andere Variante
    #rec %>%
    #  check_missing(all_predictors()) %>%
    # prep() -> prepped_rec


    shots_train_preprocessed <- bake(prepped_recipe, shots_train)
    shots_test_preprocessed <- bake(prepped_recipe,shots_test)



  # c.) Train at least 3 different classification models to predict the outcome of the shots in
  #the test data. Evaluate your models.


  #1. Logistic Regression
      logistic_reg(mode="classification") %>%
        set_engine('glm') %>%
        fit(shot_made_flag ~ ., data=shots_train_preprocessed) -> model_reg

       model_reg

       model_reg %>%
         predict(new_data = shots_test_preprocessed) %>%
         bind_cols(shots_test_preprocessed %>% dplyr::select(shot_made_flag)) -> predictions_log


       predictions_log %>%
         metrics(shot_made_flag, .pred_class)


  #3. Random Forest

    rand_forest(mode="classification",
                trees = 100) %>%
      set_engine('ranger') %>%
      fit(shot_made_flag ~ ., data=shots_train_preprocessed) -> model_forest


    model_forest %>%
      predict(new_data = shots_test_preprocessed) %>%
      bind_cols(shots_test_preprocessed %>% dplyr::select(shot_made_flag)) -> predictions_forest


    predictions_forest %>%
      metrics(shot_made_flag, .pred_class)


  #4. XGBboost

      boost_tree(mode="classification",
                 trees = 500,
                 learn_rate = 0.075,
                 tree_depth = 3) %>%
        set_engine('xgboost') %>%
        fit(shot_made_flag ~ ., data=shots_train_preprocessed) -> model_boost


      model_boost %>%
        predict(new_data = shots_test_preprocessed) %>%
        bind_cols(shots_test_preprocessed %>% dplyr::select(shot_made_flag)) -> predictions_boost

      predictions_boost

      predictions_boost %>%
        metrics(shot_made_flag, .pred_class)

      #Feature Importance
      importance <- xgboost::xgb.importance(model_boost$fit$feature_names, model = model_boost$fit)
      xgboost::xgb.plot.importance(importance)



  # Evaluate the models
  predictions = list(predictions_log, predictions_forest, predictions_boost)

  predictions %>% map(function(x){metrics(x, truth = shot_made_flag, estimate = .pred_class)})



