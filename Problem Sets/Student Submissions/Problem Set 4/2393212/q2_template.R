# Problem Set 4
# Question 2


require(tidyverse)
library(textdata)
library(tidytext)
library(gutenbergr)
library(foreign)
library(viridis)
library(ggstance)
library(ggthemes)
library(extrafont)
library(topicmodels)
library(stringr)

# Aufgabe 1 ----

files = list.files(
  path = "Problem Sets/04/bbc/",
  recursive = TRUE,
  pattern = "*.txt",
  full.names = T
)

# files <- dirname(files)
# files
getDocument = function(x) {
  data.frame(
    articleText = paste(unlist(scan(
      x, what = "character", quiet = T
    )), collapse = " "),
    category = dirname(x),
    news_number = basename(x)
  )
}
text = map_df(files, getDocument)

textData = data.frame(
  document = 1:2225,
  article = text$articleText,
  # get everything after last slash [^/]+$
  category = str_extract(text$category, "[^/]+$"),
  articleNumber = gsub(".txt", "", text$news_number),
  stringsAsFactors = FALSE
)

# Aufgabe 2 ----
category_article <- textData %>%
  unnest_tokens(output = word, input = article)  %>%
  count(category, word, sort = TRUE)  %>%
  ungroup()

# calculate total in each category
total_words <-
  category_article %>%
  group_by(category) %>%
  summarize(total = sum(n))

category_article <- left_join(category_article, total_words)

head(category_article)

# remove stop words 'the', 'to', 'of', 'and', 'a', 'in'
stopwords <-
  c(
    'and', 'the', 'to', 'of', 'they', 'as', 'you', 'on', 'are',
    'and', 'by', 'are', 'on', 'said', 'was', 'his', 'at', 'mr',
    'a', '25', 'from', 'its', 'had', 'who', 'which', 'or', 'he',
    'in', 'has', 'been', 'which', 'more', 'or', 'first', 'an', 'out',
    'i', 'than', 'up', 'â', 'one', 'also', 'would', 'should', 'were',
    'is', 'us', 'all', 'there', 'about', 'when', 'all', 'if',
    'we', 'could', 'against', 'what', 'new', 'our', 'it',
    'it',
    'best',
    'that',
    'with',
    'for',
    'be',
    'have',
    'not',
    'will',
    'but',
    'this',
    'after',
    'their',
    'can',
    'hd'
  )

# Term frequency
ggplot(category_article, aes(n / total, fill = category)) +
  geom_histogram(alpha = 0.8,
                 show.legend = FALSE,
                 bins = 30) +
  xlim(NA, 0.0009) +
  labs(title = "Term Frequency Distribution in BBC Article Categories",
       y = "Count") +
  facet_wrap(~ category, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  scale_fill_viridis(end = 0.85, discrete = TRUE) +
  theme(strip.text = element_text(hjust = 0)) +
  theme(strip.text = element_text(face = "italic"))

# term’s inverse document frequency (idf)
#The inverse document frequency (and thus tf-idf)
#is very low (near zero) for words that occur in
#many of the documents in a collection;
#this is how this approach decreases the weight for common words.
#The inverse document frequency will be a
#higher number for words that occur in fewer of the documents
#in the collection.
category_article <- category_article %>%
  bind_tf_idf(word, category, n)
category_article

category_article <- category_article %>%
  select(-total) %>%
  arrange(desc(tf_idf))
category_article

plot_bbc <- category_article %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

ggplot(plot_bbc[1:20,], aes(tf_idf, word, fill = category, alpha = tf_idf)) +
  loadfonts(device = "win") +
  geom_barh(stat = "identity") +
  labs(title = "Highest tf-idf words in BBC Article Categories",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial",
              base_size = 13,
              ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1), guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis(end = 0.85, discrete = TRUE) +
  theme(legend.title = element_blank()) +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0))


plot_bbc_individual <-
  plot_bbc %>% group_by(category) %>% top_n(15) %>% ungroup
ggplot(plot_bbc_individual,
       aes(tf_idf, word, fill = category, alpha = tf_idf)) +
  loadfonts(device = "win") +
  geom_barh(stat = "identity", show.legend = FALSE) +
  labs(title = "Highest tf-idf words in BBC Article Categories",
       y = NULL, x = "tf-idf") +
  facet_wrap(~ category, ncol = 2, scales = "free") +
  theme_tufte(base_family = "Arial",
              base_size = 13,
              ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis(end = 0.85, discrete = TRUE) +
  theme(strip.text = element_text(hjust = 0)) +
  theme(strip.text = element_text(face = "italic"))

# Aufgabe 3 ----
# topic modeling = method for unsupervised classification of documents
# Latent Dirichlet allocation
by_chapter_word <- textData %>%
  unite(category_chapter, category, articleNumber) %>%
  unnest_tokens(word, article)

word_counts <- by_chapter_word %>%
  filter(!word %in% stopwords) %>%
  count(category_chapter, word, sort = T)

word_counts

articles_dtm <- word_counts %>%
  cast_dtm(category_chapter, word, n)
articles_dtm


# create four topic LDA model
articles_lda <-
  LDA(articles_dtm, k = 5, control = list(seed = 1234))
articles_lda

articles_lda_td <- tidy(articles_lda)
articles_lda_td

top_terms <- articles_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

top_terms

# plot

theme_set(theme_bw())

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")

# # Per document classifcation
articles_lda_gamma <- tidy(articles_lda, matrix = "gamma")
articles_lda_gamma

articles_lda_gamma <- articles_lda_gamma %>%
  separate(document, c("category", "article_number"), sep = "_", convert = TRUE)
articles_lda_gamma

ggplot(articles_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~category, nrow = 2)

article_classifications <- articles_lda_gamma %>%
  group_by(category, article_number) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

category_topics <- article_classifications %>%
  count(category, topic) %>%
  group_by(topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = category, topic)

category_topics

# A lot of the articles are misclassified
article_classifications %>%
  inner_join(category_topics, by = "topic") %>%
  count(category, consensus)

assignments <- augment(articles_lda, data = articles_dtm)


assignments <- assignments %>%
  separate(document, c("category", "article_number"), sep = "_", convert = TRUE) %>%
  inner_join(category_topics, by = c(".topic" = "topic"))

assignments

# matrix of which words are listed in every category
assignments %>%
  count(category, consensus, wt = count) %>%
  spread(consensus, n, fill = 0)

# which words were misclassified
wrong_words <- assignments %>%
  filter(category != consensus)
wrong_words

wrong_words %>%
  count(category, consensus, term, wt = count) %>%
  arrange(desc(n))

word_counts %>%
  filter(word == 'increase')
