# Matriculation number: YOUR_MATRICULATION_NUMBER

# Question 1:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(randomNames, tidyverse)

firstName = randomNames(100, which.names = "first", sample.with.replacement = FALSE)
lastName = randomNames(100, which.names = "last", sample.with.replacement = FALSE)

names = data.frame(firstName, lastName)

# a)
createEmail = function(firstName, lastName){
  # Write your code here
}


# b)
createPassword = function(){
  # Write your code here
}


# c)
createUserName = function(firstName, lastName){
  # Write your code here
}


# d)
completeDataset = function(firstName, lastName){
  # Write your code here
}

# Write your code here
