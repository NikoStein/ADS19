# Matriculation number: YOUR_MATRICULATION_NUMBER

# Question 3:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, recipes, parsnip, xgboost)

titanic = titanic::titanic_train

titanic %>%
  select(-PassengerId, -Name, -Ticket, -Fare, -Cabin, -Embarked) -> titanic


# a)
# Write your code here


# b)
# Write your code here

# c)
# Write your code here

# d)
# Write your code here
