# Matriculation number: YOUR_MATRICULATION_NUMBER

# Question 1:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(randomNames, tidyverse)

firstName = randomNames(100, which.names = "first", sample.with.replacement = FALSE)
lastName = randomNames(100, which.names = "last", sample.with.replacement = FALSE)

names = data.frame(firstName, lastName)

# a)
createEmail = function(firstName, lastName){
  email = sprintf("%s.%s@uni-wuerzburg.de", firstName, lastName)
  return(email)
}

createEmail('Max', 'Mustermann')

# b)
createPassword = function(){
  password = paste0(rdunif(n=6, a=0, b=9), collapse = "")
  return(password)
}

createPassword()


# c)
createUserName = function(firstName, lastName){
  userName = ''
  userName = paste0(userName, tolower(str_sub(firstName, 1,2)))
  userName = paste0(userName, tolower(str_sub(lastName, 1,1)))
  userName = paste0(userName, paste0(rdunif(n=3, a=0, b=9), collapse = ""))
  return(userName)
}


# d)
completeDataset = function(firstName, lastName){
  email = createEmail(firstName, lastName)
  password = createPassword()
  userName = createUserName(firstName, lastName)
  df = data.frame(firstName, lastName, email, password, userName)
  return(df)
}

completeDataset("Max", "Mustermann")


personData = map2_df(names$firstName, names$lastName, completeDataset)
personData
