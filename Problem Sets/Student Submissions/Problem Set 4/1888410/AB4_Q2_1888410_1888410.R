# Problem Set 4
# Question 2

require(tidyverse)
library(foreign)
require(tidytext)



# 2) In the folder BBC you will find a dataset with different BBC news articles from five different categories.

  # a.) Load this data into R using a modified version of the code provided in the template.
  # Note that you have to modify the code to add information on the categories to your final data frame.

  files = list.files(recursive = TRUE, pattern = "*.txt")


  getDocument = function(x){
    paste(unlist(scan(x, what="character",quiet = T)),collapse = " ")
  }

  text = map_chr(files, getDocument)
  categorie = map_chr(files, function(x){basename(dirname(x))})

  textData = data.frame(document=1:2225, categorie = as.factor(categorie), text=text, stringsAsFactors = FALSE)

  #check NAs
  colnames(textData)[colSums(is.na(textData)) > 0]
  #-> keine NA Werte vorhanden






  # b.) Proceed by loading the data into tidytext and extract term frequencies as well as
  # inverse term frequencies on the category level. Interpret the results by comparing
  # categories in a meaningful way.

  textData %>%
    unnest_tokens(word, text) -> tidy_bbc

  #ohne stopwords
  data("stop_words")

  #remove stopwords
  textData %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) -> tidy_bbc

  #erzeugt spalte Anzahl (n) wie oft ein Wort in einer Kategorie vorkommt
  tidy_bbc %>%
    group_by(categorie, word) %>%
    summarise(n = n()) %>%
    arrange(-n) -> words

          #2.Variante
          #tidy_bbc %>%
          #  count(categorie, word, sort = TRUE)

  #erzeugt df mit Gesamtanzahl von Woertern pro Kategorie
  words %>%
    group_by(categorie) %>%
    summarize(total = sum(n)) -> total_words

  #joint wie oft einzelnes Wort vorkommt mit Gesamtanzahl von Woertern pro Kategorie
  tidy_bbc <- left_join(words, total_words)
  tidy_bbc


  ###Term Frequency Analysis###

  #Term frequency (tf) =  n/total
  #Term frequency is the number of times a word appears in a categorie divided by the total number of terms (words) in that categorie.
  #Ein Begriff ist für ein Dokument besonders wichtig indem es sehr häufig vorkommt (term-frequency). Normalisiert von 0 bis 1. Das häufigste ist 1 niedrigste 0.

  #Inverted document frequency (idf): log(Anzahl aller Dokumente / Anzahl der Dokumente in denen der bestimmte Begriff auftaucht)
  #IDF ist kein Maß auf dem Dokument sondern ein Maß für jeden Begriff. Messe wie speziell ist ein gewisser Begriff. Ist der Begriff in jedem Dokument dann ist er wahrscheinlich irrelevant
  #Ist er hingegen in wenigen Dokumenten dann scheint er sehr wichtig für diesen Themengebiet zu sein.

  #(tf-idf): ist tf x idf. TF ist auf Begriff Ebene und IDF auf Dokumenten Ebene. -> je hoeher der Wert desto aussagekraeftiger fuer eine Categorie
  #Terms that are common in a document get a higher weight (high TF) and Terms that are rare in the whole corpus get a higher weight (high IDF)

  # Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common
  #Notice that idf and thus tf-idf are zero for these extremely common words. These are all words that appear in all five categories,
  #so the idf term (which will then be the natural log of 1) is zero.
  #The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection;
  #this is how this approach decreases the weight for common words.
  tidy_bbc %>%
    bind_tf_idf(categorie, word, n) -> tidy_bbc


  #Let’s look at terms with high tf-idf
  #Here we see all proper nouns, names that are in fact important. None of them occur in all of the categories, and they are important, characteristic words for each text.
  tidy_bbc %>%
    select(-total) %>%
    arrange(desc(tf_idf))

  #Let’s look specifically at the most important words in each categorie:
      #sport
      tidy_bbc %>%
        filter(categorie == "sport") %>%
        select(-total) %>%
        arrange(desc(tf_idf))

      #politics
      tidy_bbc %>%
        filter(categorie == "politics") %>%
        select(-total) %>%
        arrange(desc(tf_idf))

      #tech
      tidy_bbc %>%
        filter(categorie == "tech") %>%
        select(-total) %>%
        arrange(desc(tf_idf))

      #business
      tidy_bbc %>%
        filter(categorie == "business") %>%
        select(-total) %>%
        arrange(desc(tf_idf))

      #entertainment
      tidy_bbc %>%
        filter(categorie == "entertainment") %>%
        select(-total) %>%
        arrange(desc(tf_idf)) -> test

    #Term Frequency Distribution in the categories
    ggplot(tidy_bbc, aes(n/total, fill = categorie)) +
      geom_histogram(show.legend = FALSE, bins = 30) +
      xlim(NA, 0.0009) +
      facet_wrap(~categorie, ncol = 2, scales = "free_y")


    #visualization for these high tf-idf words, top 15 nouns with the highest inverse term frequencies and highes n in each categorie
    #Woerter die fuer eine Kategorie besonders aussagekraeftig ist
    tidy_bbc %>%
      filter(n >= 40) %>% #nur Woerter die haeufiger als 40 mal in einem Text vorkommen, da viele Woerter mit n=1 eine hohen tf_idf Wert haben
      ungroup %>%
      arrange(desc(tf_idf,n)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      group_by(categorie) %>%
      top_n(15, tf_idf) %>%
      top_n(15,n) %>%
      arrange(desc(n)) %>%
      ungroup()   %>%
      ggplot(aes(word, tf_idf, fill = categorie)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~categorie, ncol = 2, scales = "free") +
      coord_flip()

    #-> in allen Kategorien haben die Top 15 Woerter den gleichen tf_idf Wert






  # c.) The tidytext website has a nice tutorial on topic modeling with LDA (https://cran.rproject.org/web/packages/tidytext/vignettes/topic_modeling.html).
  # Transfer the approach to the data at hand and assess to what extend we can learn topics in an
  # unsupervised manner. Have a look at some of the articles which were misclassified –
  # can you see reasons why? Also consider articles where the LDA classification yields
  # an unclear result – again try to figure out why this is the case.

  require(tm)
  require(topicmodels)

  tidy_bbc

  ###Latent Dirichlet Allocation with the topicmodels package####
  ###
  tidy_bbc %>%
    cast_dtm(categorie, word, n) -> categories_dtm

  categories_dtm

  #Now we are ready to use the topicmodels package to create a four topic LDA model. -> k = 5 because 5 categories
  categories_lda <- LDA(categories_dtm, k = 5, control = list(seed = 1234))
  categories_lda

  #Now tidytext gives us the option of returning to a tidy analysis, using the tidy and augment verbs borrowed from the broom package.
  #In particular, we start with the tidy verb.
  categories_lda_td <- tidy(categories_lda)
  categories_lda_td

  # => Notice that this has turned the model into a one-topic-per-term-per-row format. For each combination the model has β, the probability of that term being generated from that topic.

  #We could use dplyr’s top_n to find the top 5 terms within each topic:
  categories_lda_td %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) -> top_terms

  top_terms

  #visualization of the top words in each categorie:
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_bar(stat = "identity") +
    scale_x_reordered() +
    facet_wrap(~ topic, scales = "free_x") +
    theme_bw()




  ###Per-document classification####
  ###
  categories_lda_gamma <- tidy(categories_lda, matrix = "gamma")
  categories_lda_gamma


  #es wurden keine missclassifiziert?
  ggplot(categories_lda_gamma, aes(gamma, fill = factor(topic))) +
    geom_histogram() +
    facet_wrap(~ document, nrow = 2)



