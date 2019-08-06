# Problem Set 1
# Question 2


library(RCurl)
library(RJSONIO)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))



#a)
View(response_parsed)
summary(response_parsed)
#Verschachtelte Liste/Tabelle mit mehreren Dimensionen und Unterstrukturen (Liste von Listen)

#b)
#Zugriff auf die Unterstrukturen mit [[]] bis gesuchtes Element auftaucht
response_parsed$items[[1]]$volumeInfo$authors
response_parsed$items[[2]]$volumeInfo$authors
response_parsed$items[[3]]$volumeInfo$authors
#...
response_parsed$items[[1]]$volumeInfo$title
#...

#Vereinfacht: Schritte werden auf jeden Eintrag der Liste angewandt
#Alternative: map, sapply
#gleiche funktion auf mehrere Daten einer Tabelle anwenden (zeilen- oder spaltenweise)
lapply(response_parsed$items, function(x) {x$volumeInfo$authors})
results <- data.frame(lapply(response_parsed$items, function(x) {x$volumeInfo$authors}))


#c)
getBookList = function(numberOfItems) {
  #Sortiert nach date und title (decreasing)
  results[order(FALSE, date, title)]

}

#d)
getBookSalesInfo = function(response) {
  #String-Argument: id, price, availability, link für 40 Dokumente
  i <- 40
  while(i != 0){

  id <- response_parsed$items[[i]]$id
  avail <- response_parsed$items[[i]]$accessInfo$isAvailable
  link <- response_parsed$items[[i]]$volumeInfo$buyLink
  price <- response_parsed$items[[i]]$saleInfo$listPrice$amount
  print(id, avail, price, link)
  i <- i-1
  }
}
