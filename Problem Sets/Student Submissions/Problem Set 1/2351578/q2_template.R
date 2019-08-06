# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))


#a)
View(response_parsed)
summary(response_parsed)

#Das response object ist eine Liste der Länge 3, mit Information zu der Gesamtanzahl der Items in allen Unterlisten
# sowie wiederum einer Liste mit 40 Büchern des Autors George R.R. Martin.
#Jedes der 40 Bücher ist wiederum eine Liste mit Informationen zur ID und volume-, sale, access- und searchinfo. Diese enthalten teilweise wieder Listen.

#b)

# extracting the author, the title, publishing date and the rating of each book

lapply(response_parsed$items, function(x) {c(x$volumeInfo$authors, x$volumeInfo$title,
                                             x$volumeInfo$publishedDate, x$volumeInfo$maturityRating)})


#c)

getBookList = function(numberOfItems) {

  # make sample
        slist <- sample(response_parsed$items, 3)

  # Extract desired information of sample

        slistmatr <- sapply(slist, function(x) {c(x$volumeInfo$authors, x$volumeInfo$title,
                                                     x$volumeInfo$publishedDate, x$volumeInfo$maturityRating)})

        slistmatr
}

#Test

getBookList(3)


#d)
getBookSalesInfo = function(response) {
  # Your code
}
