# Problem Set 2
# Question 1
# Rselenium
require(rvest)
library(tidyverse)



url = "http://quotes.toscrape.com/"
urlX = "http://quotes.toscrape.com"
bsp = "http://quotes.toscrape.com/author/Elie-Wiesel/"

# a)
# Use rvest to scrape the first 10 quotes.
# Return a data frame with 3 columns (author,tags,quotes).

getDataForOneUrl = function(url){
  read_html(url) -> qts
  #Die Zitate
  qts%>%
    html_nodes('.text') %>%
    html_text() -> q
  #Die Autoren
  qts%>%
    html_nodes('.author') %>%
    html_text() -> author
  #Die tags
  qts%>%
    html_nodes('.tags') %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_replace_all("  +", " ")->tag

  qts_data <- data.frame(q,author,tag)
  return(qts_data)
}
#Ausgabe 10 Ziatate
getDataForOneUrl(url)

# b)
# Use your function to collect all 100 quotes
# from the website and store them in a data frame (quotes).


# Fuege url zusammen und speichere alle noetigen URLS in urls
urls2 = "page/"
urls = paste0(url,url2,1:10)
getDataforAll = function(url){
# nimm aus urls, wende function auf alles in urls an
# und packe es mit map_df in einen Dataframe
quotes = map_df(urls, getDataForOneUrl)
return(quotes)
}
# Ausgabe 100 Zitate
getDataforAll(url)->allQts

# c)
# Additionally, we want to collect more information on the authors.
# Therefore, your next task is to scrape the URLs of each author’s about-page.
# Vorgehen wie in a)
getAuthorLinks = function(url){
  read_html(url) -> qts
  #Die Links zu den Autorenseiten
  qts%>%
    html_nodes('.quote span a')%>%
    html_attr("href")->links

  author_url = paste0(urlX,links)

  qts_data_links <- data.frame(author_url)

  return(qts_data_links)

}
# Ausgabe AutorLink
getAuthorLinks(url)

urlsX=paste0(url,'page/',1:10)
AlleL <- map_df(urlsX,getAuthorLinks)
print(AlleL)
# d)
# Write a function to scrape the content of an about-page returning a
# data frame (authorDetails) with 3 columns (author, description, bornDate).
# Apply the function to scrape all about-pages.
getAuthorInfo = function(AuthorUrl){
  read_html(AuthorUrl) -> qts
      #Autor
      qts%>%
        html_nodes(".author-title")%>%
        html_text()%>%
        str_remove_all("\n")->name
      #Beschreibung
      qts%>%
        html_nodes(".author-description")%>%
        html_text()%>%
        str_remove_all("\n")->description
      #Geburtsdatum
      qts%>%
        html_nodes(".author-born-date")%>%
        html_text()->bornDate
      #Schreibe in df
      aut_data <- data.frame(name, description,bornDate)

  return(aut_data)
}
#getAuthorInfo(bsp) -> B

#getAllAuthorInfo = function(url){
# all_author_data = map_df(AlleL,getAuthorInfo)
#return(all_author_data)
#}
#getAllAuthorInfo(url)
#author_data <- map_df(aut_data, getAuthorInfo)

# Fuer Alle Autoren aus Autorenlinks Infos ziehen und speichern
for(i in AlleL){
  all_author_data = map_df(i,getAuthorInfo)
}
# herausnehmen der Dubletten
all_u_author_data <- unique(all_author_data)
# e)
# Next, your task is to analyze the collected data.
# Leverage your data wrangling skills to perform the following tasks:

# i)
# Transform the data frame to store the information on
# the day, month and year in distinct columns.

all_u_author_data%>%
  mutate(day=str_split(string = bornDate, pattern = ' ', simplify = TRUE)[, 2],
           month=str_split(string = bornDate, pattern = ' +', simplify = TRUE)[, 1],
                   year=str_split(string = bornDate, pattern = ', ', simplify = TRUE)[, 2])%>%
  mutate(day=str_remove_all(day,","))%>%
  select(-bornDate) ->new_data
# How many authors where born in the 19th century (1800-1899)
new_data%>%
  filter(year>=1800 & year <1900)->filter_new_data
# ii)
#Transform and summarize the quotes data set to answer the following
#questions:
 # 1. Which author has the most quotes on the website?
all_author_data%>%
  count(name)->c
  #2. How many quotes does an author have on average?

  #3. Find all quotes that use the tag “life”
allQts%>%
  filter(tag=="life")->life

# iii) Join both data frames (you may need to transform keys first)
