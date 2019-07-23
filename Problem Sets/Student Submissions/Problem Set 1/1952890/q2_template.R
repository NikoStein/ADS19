# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))

#a) Describe the structure of the response object. Explain dimensions and nesting of the elements.

summary(response_parsed)

#b) Using *apply calls extract the author, the title, publishing date and the rating of each book
#   in the response. [You will need minimal functions which do the addressing – these can be defined within the *apply call!]

# Für jedes Teil aus der Liste response_parsed$items soll die Operation volumeInfo$author ausgeführt werden:
# lapply macht das für 1, 2, 3 etc. (für jeden Eintrag in der Liste)

# sapply
# lapply
# map

# jeden Teil aus der Liste response_parsed$items nehmen und darauf die Funktion anwenden. Bsp. für x=1: response_parsed$items[[1]]$volumeInfo$authors)
authors <- sapply(response_parsed$items, function(x) {x$volumeInfo$author}, simplify = TRUE)
title <- sapply(response_parsed$items, function(x) {x$volumeInfo$title}, simplify = TRUE)
publishedDate <- sapply(response_parsed$items, function(x) {x$volumeInfo$publishedDate}, simplify = TRUE)
averageRating <- sapply(response_parsed$items, function(x) {x$volumeInfo$averageRating}, simplify = TRUE)

#c) Combine your individual calls in one function which specifies how many items to be shown.
#   Sort the list by date and title and return it.

#   Bsp.: mtcars[order(mtcars$hp),] -> gleiches Vorgehen mit Autoren und Sortieren nach verschiedenen Argumenten

getBookList = function(numberOfItems) {

}

#d) Create a function which provided with a string argument specifying a book id
#   (from the 40 books in your list) returns where this book is available as well as price
#   and a buy link. [You may want to change the API call to simplify and generalize this task.]

getBookSalesInfo = function(response) {

}
