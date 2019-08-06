# Problem Set 1
# Question 2

library(tidyverse)

library(RCurl)
library(RJSONIO)
URL <- "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL, ssl.verifyhost = 0L, ssl.verifypeer = 0L))

# a)
# BESCHREIBUNG DER DATEN
# response_parse ist zunächst einmal eine Liste der Länge 3 mit den Einträgen "kind",
# "totalItems" und "items". Sowohl beim Eintrag "kind" als auch beim Eintrag "totalItems"
# handelt es sich einfach nur um einen einzelnen Wert.
# Der Eintrag "items" hingegen ist wiederum eine Liste mit 40 Einträgen. Diese Einträge
# besitzen nur die numerischen Bezeichnungen 1 - 40.
class(response_parsed)
length(response_parsed)
(names(response_parsed))
length(response_parsed$items[[1]])

# Bei jedem Eintrag response_parsed$items[[i]] handelt es sich wiederum eine Liste der
# Länge 8 mit den Einträgen "kind", "id", "etag", "selfLink", "volumeInfo", "saleInfo"
# "accessInfo" und "searchInfo". Bei den Einträgen volumeInfo, saleInfo und accessInfo
# handelt es sich wiederum um Listen.
length(response_parsed$items[[1]])
names(response_parsed$items[[1]])
class(response_parsed$items[[1]]$volumeInfo)

length(response_parsed$items[[1]]$volumeInfo) # 22
length(response_parsed$items[[1]]$saleInfo) # 7
length(response_parsed$items[[1]]$accessInfo) # 10

#########################

# b) # Author, Title, Publishing Date, Rating of each book

names(response_parsed$items[[1]]$volumeInfo) # Hier sind in den Einträgen "authors",
# "title", "publishedDate" und "averageRating" alle benötigten Informationen vorhanden.
bookData <- response_parsed$items %>%
  lapply(FUN = function(x) {
    author <- x$volumeInfo$authors
    title <- x$volumeInfo$title
    publishedDate <- x$volumeInfo$publishedDate
    rating <- x$volumeInfo$averageRatin

    data <- list(author, title, publishedDate, rating)
    names(data) <- c("author", "title", "publishedDate", "rating")
    return(data)
  })

###########################

# c)
getBookList = function(numberOfItems) {

  bookData <- response_parsed$items %>%
    lapply(FUN = function(x) {
      author <- paste(x$volumeInfo$authors, collapse = " ")
      title <- x$volumeInfo$title

      if(is.null(x$volumeInfo$publishedDate)) {
      publishedDate <- NA
      } else {
        publishedDate <- x$volumeInfo$publishedDate
      }

      if(is.null(x$volumeInfo$averageRating)) {
        rating <- NA
      } else {
        rating <- x$volumeInfo$averageRating
      }

      data <- cbind(author, title, publishedDate, rating)
      data <- as.data.frame(data)
      colnames(data) <- c("author", "title", "publishedDate", "rating")
      return(data)
    })

  bookDataFrame <- data.frame()
  for(i in 1:length(bookData)) {
    bookDataFrame <- rbind(bookDataFrame, bookData[[i]])
  }

  # order(x = vector) gibt einen Vektor von Zahlen 1 bis length(vector) aus, wobei die erste
  # Zahl für den geordneten ersten Eintrag steht.
  bookDataFrameOrdered <- bookDataFrame[order(bookDataFrame["publishedDate"]), ]
  bookDataFrameOrdered <- bookDataFrameOrdered[order(bookDataFrameOrdered["title"]), ]

  for(i in 1:N) {
    cat(paste(bookDataFrame[i, "author"],
          bookDataFrame[i, "title"],
          bookDataFrame[i, "publishedDate"],
          bookDataFrame[i, "rating"], collapse = " "), "\n")
  }

}

##################

# d)

getBookSalesInfo = function(response) {
  number <- response_parsed$items %>%
    sapply(FUN = function(x) {
      x$id == response
    }) %>% which()

  cat(paste0("Country: ", response_parsed$items[[number]]$saleInfo$country, collapse = " "),
    paste0("Price: ", paste0(response_parsed$items[[number]]$saleInfo$retailPrice,
                             collapse = " ")),
      paste0("Link: ", response_parsed$items[[number]]$saleInfo$buyLink, collapse = ""),
      sep = "\n")
}

# BEISPIEL
response <- response_parsed$items[[14]]$id
getBookSalesInfo(response)
