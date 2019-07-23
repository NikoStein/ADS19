# Problem Set 2
# Question 1



#1a
library(tidyverse)
library(rvest)
library(xml2)
library(purrr)
library(repurrrsive)
library(stringr)

getTagsCleaned <- function(x) {
  html_nodes(x, "a") %>%
    html_text()
}

## Aufgabe 1
getTenQuotes <- function() {
  url <- read_html('http://quotes.toscrape.com/')

  quotes <-
    html_nodes(url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text", " " ))]') %>%
    html_text()
  authors <-
    html_nodes(url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "author", " " ))]') %>%
    html_text()
  tags <-
    html_nodes(url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "tags", " " ))]')

  cleanTags <- map(tags, getTagsCleaned)

  together <- data.frame(authors, cbind(cleanTags), quotes)
  x <- c("authors", "tags", "quotes")
  colnames(together) <- x
  together
}

## Aufgabe 2
getOneHundredQuotes <- function() {
  x <- c("authors", "quotes", "tags")
  map_df(1:10, function(i) {
    cat(".")

    pg <-
      read_html(sprintf('http://quotes.toscrape.com/page/%d/', i))

    quotes <-
      html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text", " " ))]') %>%
      html_text()
    authors <-
      html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "author", " " ))]') %>%
      html_text()
    tags <-
      html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "tags", " " ))]')

    cleanTags <- map(tags, getTagsCleaned)

    rbind(
      data.frame(
        'authors' = authors,
        'quotes' = quotes,
        'tags' = cbind(cleanTags),
        stringsAsFactors = F
      )
    )

  }) -> allQuotes
  colnames(allQuotes) <- x
  allQuotes
}

# Aufgabe 3
getAllAuthorsUrl <- function() {
  authorsWithUrl <- map_df(1:10, function(i) {
    cat(".")

    pg <-
      read_html(sprintf('http://quotes.toscrape.com/page/%d/', i))

    authors <-
      html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "author", " " ))]') %>%
      html_text()

    authorsUrl <-
      html_nodes(pg, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "quote", " " ))]') %>%
      html_nodes('span a') %>%
      html_attr('href')

    rbind(data.frame(
      'authors' = authors,
      'authorsUrl' = authorsUrl,
      stringsAsFactors = F
    ))
  })
  authorsWithUrl
}

# Aufgabe 4
authorsWithUrl <- getAllAuthorsUrl()

getInfoOnAuthor <- function(url) {
  base <- read_html(sprintf('http://quotes.toscrape.com%s', url))
  authorDetail <-
    data.frame(
      authors = html_node(base, 'h3') %>% html_text(),
      bornDate = html_node(base, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "author-born-date", " " ))]') %>% html_text(),
      description = html_node(base, 'div.author-description') %>% html_text(),
      stringsAsFactors = F
    )
  authorDetail$authors = gsub(pattern = "[\n] *", "", authorDetail$authors)
  authorDetail
}

# Aufgabe 5
authorsNotUnique = map_df(authorsWithUrl$authorsUrl, getInfoOnAuthor)
authorWithDetails = unique(authorsNotUnique)

authorWithDetails$day = str_sub(authorWithDetails$bornDate,-8,-7)
authorWithDetails$month = regmatches(authorWithDetails$bornDate,regexpr("[a-zA-Z]*",authorWithDetails$bornDate))
authorWithDetails$year = str_sub(authorWithDetails$bornDate,-4,-1)

between <- table(authorWithDetails$year)
between <- sum(between[names(between) >= 1800 & names(between) < 1900])

# Aufgabe 6
allQuotes <- getOneHundredQuotes()
allQuotes %>%
  group_by(authors) %>%
  count(authors, sort = T, name = "brian") -> maxQuote

maxQuote[which.max(maxQuote$brian),]

# Aufgabe 7

allQuotes %>%
  group_by(authors) %>%
  summarise(number = as.numeric(n())) %>%
  summarise("Mean of Quotes" = mean(number))

# Aufgabe 8
allQuotes[grepl('life', allQuotes$tags),]

joined_data <- left_join(allQuotes, authorWithDetails)
joined_data

#max(countedQuotes$n)

## ausführen
#getTenQuotes()
#brian=getOneHundredQuotes()
