# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
library(tidyverse)

URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))


#c) for()?, order(),  bsp: mtcars[order("hp"),]


#a)
glimpse(response_parsed)

#Es ist eine Liste voller Listen mit den jeweiligen Daten zu den Büchern, wie ID, saleInfo, ....

#b)

response_parsed$items[[1]]$volumeInfo$authors

  authors <- lapply(response_parsed$items, function(x) {x$volumeInfo$authors})
  title <- lapply(response_parsed$items, function(x) {x$volumeInfo$title})
  publishing_date <- lapply(response_parsed$items, function(x) {x$volumeInfo$publishedDate})
  rating <- lapply(response_parsed$items, function(x) {x$volumeInfo$averageRating})


response_parsed$items

#c)
getBookList = function(numberOfItems) {
  authors <- lapply(response_parsed$items, function(x) {x$volumeInfo$authors})
  title <- lapply(response_parsed$items, function(x) {x$volumeInfo$title})
  publishing_date <- lapply(response_parsed$items, function(x) {x$volumeInfo$publishedDate})
  rating <- lapply(response_parsed$items, function(x) {x$volumeInfo$averageRating})
  df_books <- data.frame(authors, title, publishing_date,rating)

  for(i in 1:numberOfItems){
    #fehlt
  }
}

getBookList(3)

#d)
getBookSalesInfo = function(response) {
  df_books_new <- data.table(authors, title, publishing_date, rating)
  df_books_new$ID <- lapply(response_parsed$items, function(x) {x$id})
  df_books_new$location <- lapply(response_parsed$items, function(x) {x$accessInfo$country})
  df_books_new$link <- lapply(response_parsed$items, function(x) {x$saleInfo$buyLink})
  df_books_new$price <- lapply(response_parsed$items, function(x) {x$saleInfo$listPrice$amount})
  return(df_books_new)
}
df_booksnew <- getBookSalesInfo(response)
df_booksnew
