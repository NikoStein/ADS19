# Matriculation number: ...

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, recipes, parsnip, xgboost)

titanic = titanic::titanic_train

titanic %>%
  select(-PassengerId, -Name, -Ticket, -Fare, -Cabin, -Embarked) -> titanic


# a)
titanic_split = initial_split(titanic, prop = 0.75, strata = "Survived")
titanic_train = training(titanic_split)
titanic_test = testing(titanic_split)

# b)
model_recipe = recipe(Survived ~ .,
                      data = titanic_train)
summary(model_recipe) 

model_recipe %>%
  step_meanimpute(all_numeric()) %>%
  step_num2factor(Survived, Pclass) %>%
  step_string2factor(Sex) %>%
  step_dummy(Pclass, Sex) -> model_recipe_steps

# c)
prepped_recipe = prep(model_recipe_steps, training = titanic_train)
titanic_train_preprocessed = bake(prepped_recipe, titanic_train)
titanic_test_preprocessed = bake(prepped_recipe, titanic_test)

# d)
boost_tree(mode = "classification",
           trees = 1000,
           tree_depth = 5) %>%
  set_engine("xgboost") %>%
  fit(Survived ~., data=titanic_train_preprocessed) -> boost_mod

boost_mod %>%
  predict(new_data = titanic_test_preprocessed) %>%
  bind_cols(titanic_test_preprocessed %>% dplyr::select(Survived)) -> predictions_boost

predictions_boost %>%
  conf_mat(Survived, .pred_class)
