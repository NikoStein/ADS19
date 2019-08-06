# Problem Set 4
# Question 2

library(tidyverse)
library(foreign)
library(tidytext)
library(gutenbergr)
library(topicmodels)
files = list.files(recursive = TRUE, pattern = "*.txt")
files
#a)
getDocumentNumber = function(x){
  paste(unlist(scan(x, what="character",quiet = T)), #scan= data to vector; what-> type of data to be read; quiet > keine Lesemeldungen
        collapse = " ") #optional character string to separate the results
}

categories <- c("business","entertainment","politics","tech","sport")

# additional function to extract the categorie for every article
getCategories <- function(x) {

  categories[(categories %in% str_extract(x, "business|politics|sport|entertainment|tech"))]
}
getDocumentNumber(files)
# add the additional column
text = map_chr(files, getDocumentNumber)
categ = map_chr(files, getCategories)
textData = data.frame(document=1:2225,text=text, categories = categ, stringsAsFactors = FALSE) #document=1:X, um eigene Spalte für Dokumentennummer einzufügen

# b)

data("stop_words") #load data set "stop words"

# unnest + removal of stop-words and numbers, then remove NAs
textData %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>% #filter for only words
  na.omit() %>%
  count(categories, word, sort = TRUE) %>% #variables to group by, sort> sort in descending order of n
  ungroup() -> tidytextdata

#count words per category, merge with main dataframe
tidytextdata %>%
  group_by(categories) %>%
  summarize(total = sum(n)) -> totalwords

words <- left_join(tidytextdata, totalwords)

#add frequency = term frequency
words %>%
  mutate(frequency = n/total) %>% #for testing tf of bind_tf_idf
  bind_tf_idf(word, categories, n) -> frequency #term=word>terms for tf; categories=document>containing ID'S

frequency %>%
  group_by(categories) %>%
  arrange(desc(tf_idf)) %>% #relevanteste Variable: tf hoch, idf klein -> wenig relevant
  top_n(25, tf_idf) %>% #25 relevanteste Variablen nach tf:idf
  ungroup() %>% #ohne ungroup + mutate keine richtige Reihenfolge
  mutate(word = reorder(word, tf_idf)) %>%#neu sortieren; Kategorie(group_by)> 1Var, reorder by values of a second variable [usually numeric]
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
  cast_dtm(document, word, n) -> tidytextdata2 #tidy to Term-document Matrix
# we already know about 5 possible topics
articles_lda <- topicmodels::LDA(tidytextdata2, k = 5, control = list(seed = 1234))
#?LDA
articles_topics <- tidy(articles_lda, matrix = "beta", ) #turns objects to tibbles

#topwords je Topic
articles_topics %>%
  group_by(topic) %>%
  top_n(1, beta)

articles_topics %>%
  arrange(desc(beta)) %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>% #ohne ungroup + mutate keine richtige Reihenfolge
  mutate(term = reorder(term, beta)) %>%#neu sortieren; Kategorie(group_by)> 1Var, reorder by values of a second variable [usually numeric]
  ggplot(aes(x=term, beta, fill = factor(topic)))+
  geom_col() +
  coord_flip() +
  facet_wrap(~topic, scales="free")

articles_lda_gamma <- tidy(articles_lda, matrix = "gamma")

#set doc as numeric
as.numeric(articles_lda_gamma$document) -> articles_lda_gamma$document
as.numeric(textData$document) -> textData$document
articles_lda_gamma %>%
  left_join(textData) -> classify

classify %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) -> compare
