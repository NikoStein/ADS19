# Problem Set 2
# Question 1

require(tidyverse)
require(rvest)
library(robotstxt)

# paths_allowed(c(url))
# robots.txt nicht bei jeder HTML-Seite hinterlegt, dann kommt HTTP-Status: 404

#######################################################################################################################################
# Mögliche Methoden, um unerwünschte \n und Leerzeichen zu entfernen:

# tags %>%
#   str_remove('\n')

# quotes_df %>%
#   mutate(tags = str_remove_all(tags, '\n'))

# str_remove_all('\n') %>%
#   str_replace_all('  +',' ') -> tags # '  +' entfernt alle Spaces, die >= 2 mal vorkommen und ersetzt diese mit einem Leerzeichen

# authorDetails$month <- gsub("([A-Za-z]+).*", "\\1", authorDetails$author_borndate)
# str_split_fixed(authorDetails$author_borndate, " ")

# am einfachsten: str_trim() oder str_squish()
# str_trim() removes whitespace from start and end of string
# str_squish() also reduces repeated whitespace inside a string.
#######################################################################################################################################


# a) Use rvest to scrape the first 10 quotes. Return a data frame with 3 columns (author, tags,quotes).
# b) Use your function to collect all 100 quotes from the website and store them in a data frame (quotes).

url = 'http://quotes.toscrape.com/'

getDataFromURL <- function(url) {

  read_html(url) -> rawData

  # quotes
  rawData %>%
    html_nodes('.text') %>%
    html_text() -> quotes

  # author
  rawData %>%
    html_nodes('.author') %>%
    html_text() -> author

  # tags
  rawData %>%
    html_nodes('.tags') %>%
    html_text() %>%
    str_squish() -> tags

  quotes_df <- data.frame(quotes, author, tags)

  return(quotes_df)

}

# page/ und die Nummer der Seite an die oben gegebene URL anhängen
# paste() fügt ein zusätzliches Leerzeichen ein, paste0() nicht
urls = paste0(url, 'page/', 1:10)

# Anzeigen der ersten 10 Zitate (url ohne paste0)
getDataFromURL(url) -> quotesTest

# map_df wendet für jede Werte von urls die Funktion getDataFromURL an
Hundret_quotes <- map_df(urls, getDataFromURL)


# c) Additionally, we want to collect more information on the authors.
# Therefore, your next task is to scrape the URLs of each author’s about-page.

url2 = 'http://quotes.toscrape.com' # ohne / am Ende

getDataFromURLWithLinks <- function(url) {

  read_html(url) -> rawData

  # Links werden über html_attr('href') identifiziert
  rawData %>%
    html_nodes('.quote span a') %>%
    html_attr("href") -> links

  # vollständiger Link
  author_url = paste0(url2, links)

  df_Links <- data.frame(author_url)

  return(df_Links)

}

getDataFromURLWithLinks(url)

# Alle 10 Seiten durchgehen
urls = paste0(url, 'page/', 1:10)

AlleLinks <- map_df(urls, getDataFromURLWithLinks)

# d) Write a function to scrape the content of an about-page returning a data frame (authorDetails)
# with 3 columns (author, description, bornDate).
# Apply the function to scrape all about-pages.

getAuthorsWithDescription <- function (author_url) {

  read_html(author_url) -> AuthorData

  AuthorData %>%
    html_nodes('.author-title') %>%
    html_text() -> author_title

  AuthorData %>%
    html_nodes('.author-description') %>%
    html_text() -> author_description

  AuthorData %>%
    html_nodes('.author-born-date') %>%
    html_text() -> author_borndate

  df_AuthorDescription <- data.frame(author_title, author_description, author_borndate)

  return(df_AuthorDescription)

}

for (i in AlleLinks) {
  authorDetails <- map_df(i, getAuthorsWithDescription)
}

# Duplikate aus dataframe entfernen
# Neues dataframe authorDetailsUnique
authorDetails [!duplicated(authorDetails$author_title), ] -> authorDetailsUnique

# e1) The authorDetails data frame stores the information on the birth data in one column (bornDate).
# Transform the data frame to store the information on the day, month and year in distinct columns.

authorDetailsUnique %>%
  separate(author_borndate, c("Month", "Day","Year")) -> authorDetailsUniqueWithDate

# How many authors where born in the 19th century (1800-1899)?
author_count <- sum(authorDetailsUniqueWithDate$Year >= 1800 & authorDetailsUniqueWithDate$Year <= 1899, na.rm=TRUE) #na.rm=TRUE
sprintf("%s Autoren sind im 19. Jahrhuntert geboren", author_count)

# e2) Transform and summarize the quotes data set to answer the following questions:
# 1. Which author has the most quotes on the website?
quoteSummary <- as.data.frame(plyr::count(Hundret_quotes, "author"))
quoteSummary

# 2. How many quotes does an author have on average?
quoteSummary %>%
  summarise(avg = mean(freq)) -> avg

sprintf("An author has %s quotes on average", avg)

# 3. Find all quotes that use the tag “life”
Hundret_quotes[grep("life",Hundret_quotes$tags),]

# e3) Join both data frames (you may need to transform keys first)
# Hundret_quotes and authorDetailsUniqueWithDate

authorDetails %>%
  separate(author_borndate, c("Month", "Day","Year")) -> authorDetailsWithDate

authorDetailsWithDate$ID <- seq.int(nrow(authorDetailsWithDate))
Hundret_quotes$ID <- seq.int(nrow(Hundret_quotes))

merge(Hundret_quotes, authorDetailsWithDate, by='ID') -> MergeQuotes
