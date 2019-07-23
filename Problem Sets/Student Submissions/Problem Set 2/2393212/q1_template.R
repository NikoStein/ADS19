# Problem Set 2
# Question 1


library(tidyverse)
library(rvest)
library(robotstxt)

quotesToScrapeURL <- "http://quotes.toscrape.com"
paths_allowed(c(quotesToScrapeURL))

# a)
getFirstTenQuotesDataFrame <- function(URL) {
  quotes <- read_html(URL)

  quotes %>%
    html_nodes(".quote .author") %>%
    html_text() %>%
    head(10) -> author

  quotes %>%
    html_nodes(".quote .tags .keywords") %>%
    html_attr("content") %>%
    head(10) -> tags

  quotes %>%
    html_nodes(".quote .text") %>%
    html_text() %>%
    head(10) %>%
    noquote()  -> quotes

  df <- data.frame(author, tags, quotes)
  df
}

# run a)
getFirstTenQuotesDataFrame(quotesToScrapeURL)


# b)
getAllHundredQuotes <- function() {
  df <- data.frame(author=character(),
                   tags=character(),
                   quotes=character())
  for(k in c(1:10)) {
    df <- rbind(df,getFirstTenQuotesDataFrame(paste0(quotesToScrapeURL, "/page/", k)))
  }
  df
}

# run b)
getAllHundredQuotes()


# c)
# distinct values
getAllAuthorURLs <- function() {
  allPagesURLs <- vector()
  allAuthorsURLs <- vector()
  for(k in c(1:10)) {
    read_html(paste0(quotesToScrapeURL, "/page/", k)) %>%
      html_nodes(".quote span a") %>%
      html_attr("href") -> URLsOfOnePage
    allPagesURLs <- c(allPagesURLs, URLsOfOnePage)
  }
  for(i in c(1:length(allPagesURLs))) {
    if (!(allPagesURLs[i] %in% allAuthorsURLs)) {
      allAuthorsURLs <- c(allAuthorsURLs, allPagesURLs[i])
    }
  }
  allAuthorsURLs
}

# run c)
getAllAuthorURLs()

# d)
getAuthorDetails <- function(authorURL) {
  read_html(paste0(quotesToScrapeURL, authorURL)) %>%
    html_nodes(".author-title") %>%
    html_text() -> author

  author %>%
    str_remove_all("\\n") %>%
    str_remove_all("[ \t]+$") %>% #trailing whitespace
    str_remove_all("^[ \t]") -> author #leading whitespace

  read_html(paste0(quotesToScrapeURL, authorURL)) %>%
    html_nodes(".author-description") %>%
    html_text() -> description

  description %>%
    str_remove_all("\\n") %>%
    str_remove_all("[ \t]+$") %>% #trailing whitespace
    str_remove_all("^[ \t]") -> description #leading whitespace

  read_html(paste0(quotesToScrapeURL, authorURL)) %>%
    html_nodes(".author-born-date") %>%
    html_text() -> bornDate

  bornDate %>%
    str_remove_all("\\n") %>%
    str_remove_all("[ \t]+$") %>% #trailing whitespace
    str_remove_all("^[ \t]") -> bornDate #leading whitespace

  authorDetails <- data.frame(author=character(), description=character(), bornDate=character())

  authorDetails <- rbind(authorDetails, data.frame(author, description, bornDate))

  authorDetails
}

getAllAuthorDetails <- function(allAuthorURLs) {
  allAuthorDetails <- data.frame(author = character(),
                              description = character(),
                              bornDate = character())
  for(i in c(1:length(allAuthorURLs))) {
    allAuthorDetails <- rbind(allAuthorDetails, getAuthorDetails(allAuthorURLs[i]))
  }
  allAuthorDetails
}

# run d)
getAllAuthorDetails(getAllAuthorURLs())


# e) i)
transformDateOfAllAuthorDetails <- function(allAuthorDetails) {
  allAuthorDetails %>%
    mutate(day = bornDate %>% str_extract_all("^.*(?=,)") %>% str_remove_all("[^[0-9]]"),
           month = bornDate %>% str_remove_all("[^[A-Z][a-z]]"),
           year = bornDate %>% str_remove_all(".*(?= )|^[ \t]")) %>%
    select(-bornDate) -> allAuthorDetails
  allAuthorDetails
}

# run e) i)
allAuthorDetails <- transformDateOfAllAuthorDetails(getAllAuthorDetails())
allAuthorDetails

allAuthorDetails %>%
  filter(as.numeric(year) >= 1800 & as.numeric(year) <= 1899) -> allAuthorOfNineteenthCentury
sprintf("%s authors were born in the 19th century.", nrow(allAuthorOfNineteenthCentury))


# e) ii) 1.
getAuthorWithMostQuotes <- function() {
  getAllHundredQuotes() %>%
    select(author) %>%
    mutate(numberOfQuotes = 1) %>%
    group_by(author) %>%
    summarise(numOfQuotes = sum(numberOfQuotes)) %>%
    arrange(-numOfQuotes) -> df
  df[1,1]
}
# run e) ii) 1.
getAuthorWithMostQuotes()

# e) ii) 2.
getNumberOfQuotesInAverage <- function() {
  getAllHundredQuotes() %>%
    select(author) %>%
    mutate(numberOfQuotes = 1) %>%
    group_by(author) %>%
    summarise(numOfQuotes = sum(numberOfQuotes)) %>%
    arrange(-numOfQuotes) -> df
  mean(df$numOfQuotes)
}
# run e) ii) 2.
getNumberOfQuotesInAverage()

# e) ii) 3.
getQuotesThatUseTheTagLife <- function() {
  quoteList <- vector()
  allHundredQuotes <- getAllHundredQuotes()
  for(i in c(1:nrow(allHundredQuotes))) {
    if (grepl(allHundredQuotes[i,2], "life")) {
      quoteList <- c(quoteList, allHundredQuotes[i,3])
    }
  }
  quoteList
}

# run e) ii) 3.
getQuotesThatUseTheTagLife()


# e) iii)
getJoinedDataFrames <- function() {
  allHundredQuotes <- getAllHundredQuotes()
  allAuthorDetails <- transformDateOfAllAuthorDetails(getAllAuthorDetails(getAllAuthorURLs()))
  joinedDataFrame <- left_join(allHundredQuotes, allAuthorDetails, by = "author")
  joinedDataFrame
}
# run e) iii)
getJoinedDataFrames()
