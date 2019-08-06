# Problem Set 1
# Question 2

library(RCurl)
library(RJSONIO)
URL = "https://www.googleapis.com/books/v1/volumes?q=george+r+r+martin&maxResults=40"
response_parsed <- fromJSON(getURL(URL,ssl.verifyhost = 0L, ssl.verifypeer = 0L))


#a)
# Your code
typeof(response_parsed)
#or: eapply(.GlobalEnv,typeof)$response_parsed

view(response_parsed)
#Solution: It's a list with 3 elements: kind (character), totalItems (double) and items (list).
# The list items has a length of 40.

#Zwei Moeglichkeiten um zum Titel von Buch 40 zu kommen
response_parsed$items[[40]]$volumeInfo$title
#oder: response_parsed[[3]][40][[1]][[5]][1][[1]]

#Some other list operations:
#rename: names(list) <- c("Book","Author","Title",...) #or by creation list <- list(Book = books, Author = author,...)
#extracting: [] returns list , [[]] & $ returns object in a list
    #Example Code to see the difference:
    #response_parsed[2] -> it's still a list you have the component and a double of the totalItems
    #response_parsed[[2]] -> here you only get the double number of totalitems
    #response_parsed$totalItems -> same, you only get the double number of totalitems



#b)
# Your code

#$authors => Author
#$title => Titel
#$publishedDate => publishing Date
#$maturityRating =>  Rating

extractFunc <- function(x){

  author = x$volumeInfo$authors
  title = x$volumeInfo$title
  date = x$volumeInfo$publishedDate
  rating = x$volumeInfo$maturityRating

  cat(sprintf("Author: %s \n Title: %s \n Publishing Date: %s \n Rating: %s \n\n", author, title, date, rating))

  #Direkt-Variante
  #cat(sprintf("Author: %s \n Title: %s \n Publishing Date: %s \n Rating: %s \n\n", x$volumeInfo$authors, x$volumeInfo$title, x$volumeInfo$publishedDate, x$volumeInfo$maturityRating))
  #Paste-Variante
  #paste("Author: " ,x$volumeInfo$authors, "\n", "Title: ",  x$volumeInfo$title, "Publishing Date: ", x$volumeInfo$publishedDate, "\n", "Rating: ", x$volumeInfo$maturityRating ,"\n\n")

}

lapply(response_parsed$items, extractFunc)
#Die Elemente [[1]] NULL kommen von der cat Funktion.
#ALternative waere mit paste darunter, da funktionieren die Zeilenumbrueche aber nicht



#c)
getBookList = function(numberOfItems) {
  # Your code

  l = response_parsed$items[1:numberOfItems]

  for(i in l){

    #get author, title,.. from list element i
    author = i$volumeInfo$authors
    title = i$volumeInfo$title
    date = i$volumeInfo$publishedDate
    rating = i$volumeInfo$maturityRating

    cat(sprintf("Author: %s \n Title: %s \n Publishing Date: %s \n Rating: %s \n\n", author, title, date, rating))
  }

  #sort list

}

#call function with 4 items
getBookList(4)




#d)
getBookSalesInfo = function(response) {

  #available = Was ist damit gemeint?
  price = response_parsed$items[[response]]$saleInfo$retailPrice$amount
  currencie = response_parsed$items[[response]]$saleInfo$retailPrice$currencyCode
  link = response_parsed$items[[response]]$saleInfo$buyLink
  cat(sprintf("Price: %s %s \nBuy link: %s \n", price, currencie, link))
  #paste(price, currencie, link)
}

#call function with book id (for example book id = 10)
getBookSalesInfo(10)



