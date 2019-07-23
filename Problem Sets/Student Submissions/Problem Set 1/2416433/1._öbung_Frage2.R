# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
library(tidyverse)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))


response_parsed$items[[1]]$volumeInfo$authors
response_parsed$items[[1]]$saleInfo$buyLink
lapply(response_parsed$items, function(x){x$volumeInfo$authors})



#a)
view(response_parsed)
summary(response_parsed)
#b)
# AUTHOR
lapply(response_parsed$items, function(x){x$volumeInfo$authors})

# 2. Moeglichkeit

author.function <- function(x){
  return(x$volumeInfo$authors)
}
map(response_parsed$items, author.function)

# TITLE
lapply(response_parsed$items, function(x){x$volumeInfo$title})

# 2. Moeglichkeit

title.function <- function(x){
  return(x$volumeInfo$title)
}
map(response_parsed$items, title.function)

# PUBLISHED DATE
lapply(response_parsed$items, function(x){x$volumeInfo$publishedDate})

# 2. Moeglichkeit

publishedDate.function <- function(x){
  return(x$volumeInfo$publishedDate)
}
map(response_parsed$items, publishedDate.function)

# RATING
lapply(response_parsed$items, function(x){x$volumeInfo$averageRating})

# 2. Moeglichkeit

rating.function <- function(x){
  return(x$volumeInfo$averageRating)
}
map(response_parsed$items, rating.function)

#c)
#authors.function <- function(){

#}
#getBookList = function(numberOfItems) {
#  list.authors <- lapply(response_parsed$items, function(x){x$volumeInfo$authors})
#  list.titles <- lapply(response_parsed$items, function(x){x$volumeInfo$title})
#  list.Dates <- lapply(response_parsed$items, function(x){x$volumeInfo$publishedDate})
#  list.Ratings <- lapply(response_parsed$items, function(x){x$volumeInfo$averageRating})
#  test <- seq(1, 5, 1)
#  authors.list <- map(response_parsed$items[test], author.function)
#  test.vector <- c(10,20)
#  for (i in seq(1, 5, 1)) {
#    index.author <- match(c(authors.list[2]),authors.list)
#    test.vector <- append(test.vector, match(c(authors.list[i]),authors.list))

# }



#}

#d)
zufall <- sample(1:40, 1)
book.ids <- lapply(response_parsed$items, function(x){x$id})
id <- book.ids[zufall]
getBookSalesInfo = function(book.id) {
  index <- match(c(book.id), book.ids)
  country <- sprintf("%s", response_parsed$items[[index]]$saleInfo$country)
  price <- sprintf("%s %s", response_parsed$items[[index]]$saleInfo$retailPrice$amount,
                   response_parsed$items[[index]]$saleInfo$retailPrice$currencyCode)
  buyLink <- sprintf("%s", response_parsed$items[[index]]$saleInfo$buyLink)
  book.information <- c(country, price, buyLink)
  return(book.information)
}
getBookSalesInfo(id)
