# Problem Set 4
# Question 2


require(tidyverse)
library(foreign)
library(tidytext)
library(topicmodels)
library(tidymodels)

setwd("C:/Users/Christoph/Documents/bbc")

files = list.files(recursive = TRUE, pattern = "*.txt")

getDocument = function(x){
  paste(unlist(scan(x, what="character",quiet = T)),collapse = " ")
}

getCategory = function(x){
  paste(unlist(strsplit(x, split = "/", fixed = T))[1])
}

text = map_chr(files, getDocument)
category = map_chr(files, getCategory)
textData = data.frame(document=1:2225, text=text, category)

textData$text <- as.character(textData$text)
textData$category <- as.character(textData$category)

words <- strsplit(textData$text, split = " ")

data.frame(document = rep(textData$document, times = sapply(words, length)),
           category = rep(textData$category, times = sapply(words, length)),
           word = unlist(words)) -> textData_final

textData_final$word <- gsub("[[:punct:]]", textData_final$word, replacement = " ")

textData_final$word <- tolower(textData_final$word)


# b)

data("stop_words")
textData_final %>%
  anti_join(stop_words) -> textData_final

textData_final %>%
  group_by(category) %>%
  count(word, sort = T) %>%
  filter(word != " ", word != "") -> textData_frequency

textData_frequency %>%
  bind_tf_idf(word, category, n) -> textData_inverseFrequency

textData_inverseFrequency %>%
  group_by(category) %>%
  arrange(desc(tf_idf)) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x=word, y=tf_idf))+
  geom_col() +
  coord_flip() +
  facet_wrap(~category, scales="free") -> g

g

# c)


# cast a one-token-per-row table into a DocumentTermMatrix
category_dtm <- textData_frequency %>%
  cast_dtm(category, word, n)

category_dtm


# create a five topic LDA model
category_lda <- LDA(category_dtm, k = 5, control = list(seed = 1234))
category_lda

# returning to a tidy analysis -> turned the model into a one-topic-per-term-per-row format.
# For each combination the model has ??, the probability of that term being generated from that topic
category_lda_td <- tidy(category_lda)
category_lda_td


# find the top 5 terms within each topic
top_terms <- category_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


# visualization
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ topic, scales = "free_x")


