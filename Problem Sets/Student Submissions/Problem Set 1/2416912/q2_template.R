# Problem Set 1
# Question 2


#This library is used to manipulate data
library(dplyr)
library(RCurl)
library(RJSONIO)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))


#a)
attributes(response_parsed$items[[1]])
typeof(response_parsed$items[[1]])
attributes(response_parsed$items[[1]])
#str(response_parsed)
#

# b)
data <- lapply(response_parsed$items,  '[[', c(5))
dataEnd <- lapply(data, '[', c('title', 'publishedDate', 'authors'[[1]]))
dataEndnow = lapply(dataEnd, as.data.frame)
dataEndnow = bind_rows(dataEndnow)
trimList = getBookList(5)
trimList

getBookSalesInfo(10)

#c)
getBookList = function(numberOfItems) {
  newdata<-dataEndnow[order(dataEndnow$publishedDate),]
  datatrim <-head(newdata, n= numberOfItems)
  return(datatrim)
}

#d)
getBookSalesInfo = function(response) {
  vekNames = c('shop', 'price', 'link')
  resp <- dataEndnow[response,]
  data[[response]][["infoLink"]]
}

