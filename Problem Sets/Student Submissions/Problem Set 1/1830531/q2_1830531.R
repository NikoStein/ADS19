# Problem Set 1
# Question 2


library(bitops)
library(RCurl)
library(RJSONIO)
library(tidyverse)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))



#a)
glimpse(response_parsed)
# A list that consist out of 3 lists.The third list contains one list for every item. The 40 lists contain 8 lists themselves.

#b)
lapply(response_parsed$items, function(result)result$volumeInfo$author)
lapply(response_parsed$items, function(result)result$volumeInfo$title)
lapply(response_parsed$items, function(result)result$volumeInfo$averageRating)
lapply(response_parsed$items, function(result)result$volumeInfo$publishedDate)

#c)
getBookList = function(numberOfItems) {
  result <- response_parsed$items[c(1:numberOfItems)]
  list(author = (lapply(result, function(res)res$volumeInfo$author)),
       title = (sort(unlist(lapply(result, function(res)res$volumeInfo$title)))),
       avgrating = (lapply(result, function(res)res$volumeInfo$averageRating)),
       PublishedDate = (sort(unlist(lapply(result, function(res)res$volumeInfo$publishedDate)))))
}

#d)
getBookSalesInfo = function(response) {
  id <- response
  search <- map(response_parsed$items, function(x) if (x$id == id){ list(Country = x$saleInfo$country, Price = c(x$saleInfo$retailPrice$amount,
                                                                         x$saleInfo$retailPrice$currencyCode), Link = x$saleInfo$buyLink) } )

  Filter(Negate(is.null), search)

}

