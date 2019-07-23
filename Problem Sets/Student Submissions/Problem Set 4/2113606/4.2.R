# Problem Set 4
# Question 2


require(tidyverse)
library(foreign)
library(tidytext)

files = list.files(recursive = TRUE, pattern = "*.txt")

getDocument = function(x){
  paste(unlist(scan(x, what="character",quiet = T)),collapse = " ")
}

text = map_chr(files, getDocument)
textData = data.frame(document=1:2225, text=text, category = category)

#a
#load data and modify code to get information about categories

#b
#load data into tidytext
#termfrequencies
#inverse termfrequencies on category level
#compare results of categories in meaningful way

tidy_files <- textData %>%
  unnest_tokens(word, text)
bind_tf_idf(textData, word, category, n) -> idf

#c
#apply tutorial
#to what extend topics can be learned
#reasons for misclassified topics
#reasons for unclear results
