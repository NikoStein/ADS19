# Problem Set 4
# Question 2


require(tidyverse)
library(foreign)
library(tidytext)
library(gutenbergr)
files = list.files(recursive = TRUE, pattern = "*.txt")

#a)

getDocument = function(x){
  paste(unlist(scan(x, what="character",quiet = T)),
        collapse = " ")
}

categories <- c("business","entertainment","politics","tech","sport")

# additional function to extract the categorie for every article
getCategories <- function(x) {

  categories[(categories %in% str_extract(x, "business|politics|sport|entertainment|tech"))]
}
# add the additional column
text = map_chr(files, getDocument)
categ = map_chr(files, getCategories)
textData = data.frame(document=1:2225, text=text, categories = categ, stringsAsFactors = FALSE)

# b)

data("stop_words")
# unnest + removal of stop-words and numbers, then remove NAs
textData %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  na.omit() %>%
  count(categories, word, sort = TRUE) %>%
  ungroup() -> tidytextdata

#total words per category + merge with main dataframe
tidytextdata %>%
  group_by(categories) %>%
  summarize(total = sum(n)) -> totalwords

words <- left_join(tidytextdata, totalwords)

words %>%
  mutate(frequency = n/total) -> frequency

frequency %>%
  bind_tf_idf(word, categories, n) -> frequency

frequency %>%
  group_by(categories) %>%
  arrange(desc(tf_idf)) %>%
  top_n(25, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x=word, y=tf_idf))+
  geom_col() +
  coord_flip() +
  facet_wrap(~categories, scales="free")

# c)
# only use the documents number and get the unsupervises learning algorithm to find the topics
textData %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  na.omit() %>%
  count(document, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(document, word, n) -> tidytextdata2
# we already know about 5 possible topics
articles_lda <- topicmodels::LDA(tidytextdata2, k = 5, control = list(seed = 1234))

articles_topics <- tidy(articles_lda, matrix = "beta")

articles_topics %>%
  group_by(topic) %>%
  top_n(1, beta)


# visualize the 5 topics with allocated words

articles_topics %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) -> articles_topics_terms

articles_topics_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


articles_lda_gamma <- tidy(articles_lda, matrix = "gamma")
articles_lda_gamma

as.numeric(articles_lda_gamma$document) -> articles_lda_gamma$document
as.numeric(textData$document) -> textData$document

articles_lda_gamma %>%
  left_join(textData) -> classify

classify %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) -> compare
