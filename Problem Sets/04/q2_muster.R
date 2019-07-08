# Problem Set 4
# Question 2

# Name: Your Name
# Matrikelnummer: Your matriculation number

require(tidyverse)
library(foreign)

#a)

files = list.files(recursive = TRUE, pattern = "*.txt")

getDocument = function(x){
  paste(unlist(scan(x, what="character",quiet = T)),collapse = " ")
}

text = map_chr(files, getDocument)
textData = data.frame(document=1:2225, text=text)

files %>%
  str_extract_all("/.+/", simplify = TRUE) %>%
  str_replace_all("/", "") -> categories

textData$category = categories
textData$text = as.character(textData$text)

#b)
library(tidytext)

textData %>%
  group_by(category) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  
  
  top_n(5) %>%
  arrange(desc(n), .by_group = TRUE) -> termFrequencies

termFrequencies %>%
  ggplot(aes(x=word, y=n, fill=category)) +
  geom_col(position="dodge") + coord_flip() + theme_bw()


textData %>%
  group_by(category) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, document, n)
