# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
library(tidyverse)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))
#speichert Inhalt der URL in Variablen
#a)
summary(response_parsed)
#List with 40 entries

#b)
# Your code
replicate(1, lapply(response_parsed$items, function(x){x$volumeInfo$authors}), simplify = "array")
lapply(response_parsed$items, function(x){x$volumeInfo$authors})
lapply(response_parsed$items, function(x){x$volumeInfo$title})
lapply(response_parsed$items, function(x){x$volumeInfo$publishedDate})
lapply(response_parsed$items, function(x){x$volumeInfo$averageRating})

#optional:
map(response_parsed$items, function(x){x$volumeInfo$authors})
#auch möglich: sapply

#einzelne Einträge
response_parsed$items[[1]]$volumeInfo$authors
response_parsed$items[[1]]$volumeInfo$title
response_parsed$items[[1]]$volumeInfo$publishedDate
response_parsed$items[[1]]$volumeInfo$averageRating

#c)
getBookList = function() {
  author<-lapply(response_parsed$items, function(x){x$volumeInfo$authors})
  title<-lapply(response_parsed$items, function(x){x$volumeInfo$title})
  items<-lapply(response_parsed$items, function(x){x$volumeInfo$publishedDate})
  averageRating<-lapply(response_parsed$items, function(x){x$volumeInfo$averageRating})
  order(author, title, items, averageRating, na.last = TRUE, decreasing = FALSE)
}
#order(... = [author, title, items, averageRating] )}
getBookList()
#order Befehl mtcars[order(mtcars$hp)]
#d)Create a function which provided with a string argument specifying a book id (from
#the 40 books in your list) returns where this book is available as well as price and a
#buy link. [You may want to change the API call to simplify and generalize this task.]
#getBookSalesInfo(response)
getBookSalesInfo = function(response) {
  response_parsed$items[[1]]$saleInfo$buyLink
  response_parsed$items[[1]]$saleInfo$country
  cat(response_parsed$items[[1]]$saleInfo$listPrice$amount,response_parsed$items[[1]]$saleInfo$listPrice$currencyCode)
}
#lapply(response_parsed$items, function(x){x$volumeInfo$authors)
