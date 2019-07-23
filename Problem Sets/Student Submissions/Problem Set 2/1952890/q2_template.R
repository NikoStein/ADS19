# Problem Set 2
# Question 2


require(tidyverse)
require(rvest)
library(robotstxt)

# a) First, use rvest to scrape at least the 1000 latest deals.
# In order to minimize server traffic, you should scrape all the relevant information from the starting page and do not need
# to follow the deep links to the particular deals.
# Your function should return a data frame (deals) with 5 columns (title, temperature, author, deep link, number of comments).

url = "https://www.mydealz.de/"

# Problem: title und temperature werden nicht alle von SelectorGadget ausgewählt
# Anzahl von title und temperature ungleich und daher Error
# Lösung: über html_nodes das komplette Grid abfragen und in diesem Grid die gesuchten Infos auswählen

getDataFromURL <- function(url) {

  read_html(url) -> rawData

  rawData %>%
    html_nodes('.threadGrid') %>%
    html_node('.thread-title--list') %>%
    html_text() %>%
    str_trim() -> title

  rawData %>%
    html_nodes('.threadGrid') %>%
    html_node('.vote-temp--burn , .space--h-2.text--b') %>%
    html_text() %>%
    str_trim() -> temp

  rawData %>%
    html_nodes('.threadGrid') %>%
    html_node('.thread-username') %>%
    html_text() %>%
    str_trim() -> author

  rawData %>%
    html_nodes('.threadGrid') %>%
    html_node('.thread-title--list') %>%
    html_attr('href') -> deepLink

  rawData %>%
    html_nodes('.threadGrid') %>%
    html_node('.space--h-3.btn--mode-boxSec') %>%
    html_text() %>%
    str_trim() -> numberOfComments

  deals <- data.frame(title, temp, author, deepLink, numberOfComments)

    return(deals)

  }

getDataFromURL(url)

# alle Einträge der ersten 50 Seiten
urls = paste0(url, '?page=', 1:50)

newDeals <- map_df(urls, getDataFromURL)

# b) Having collected the data set you will probably notice that there are some arcane symbols in the title and temperature columns.
# Clean both columns by changing the encoding from ‘latin1’ to ‘ASCII’ using the iconv function.
# Additionally, remove unneeded whitespaces, line breaks and tabulators from the data frame.

#iconv(newDeals,from = "latin1", to = "ASCII") -> newDealsEncoded

# c1) What share of the posted deals is voted hot (over 0 degrees)?
# What share is voted cold (below 0 degrees)?
# What is the average temperature of a deal?

#str_remove_all(newDeals$temp, '°') -> newDeals2

# alle ° entfernen und Autohot-Bewertungen ausblenden
newDeals %>%
  mutate(temp = str_remove_all(temp, '°')) %>%
  filter(temp != 'Autohot!') -> newDeals2
  #mutate(temp = na_if(temp, 'Autohot!')) -> newDeals2  # Autohot-Bewertungen als NA deklarieren
  #transform(temp, as.numeric(temp)) -> newDeals2       # klappt nicht

# Welche Datentypen haben die Spalten aktuell?
sapply(newDeals2, class)
# Temperatur-Spalte von character zu numeric umwandeln, um Berechnungen durchführen zu können
newDeals2$temp <- as.numeric(as.character(newDeals2$temp))
# Neue Datentypen
sapply(newDeals2, class)

newDeals2 %>%
  filter(temp > 0) -> degree_hot
newDeals2 %>%
  filter(temp < 0) -> degree_cold

share_hot <- nrow(degree_hot) / nrow(newDeals)
share_cold <- nrow(degree_cold) / nrow(newDeals)

newDeals2 %>%
  summarise(avg = mean(temp)) -> avg

sprintf('%s Prozent der Deals sind mit hot bewertet', share_hot*100)
sprintf('%s Prozent der Deals sind mit cold bewertet', share_cold*100)
sprintf('Die Durchschnittstemperatur eines Deals beträgt %s °', avg)

# c2) On average, do hot or cold deals have more comments?

# Da im aktuellen Datensatz keine negativen Temperaturen vorhanden sind, werden zur Veranschaulichung der Aufgabe
# Temperaturen bis 400° als 'cold' definiert (analoge Funktionsweise mit 0)

# Kommentar-Spalte umwandeln in numeric
newDeals2$numberOfComments <- as.numeric(as.character(newDeals2$numberOfComments))

newDeals2 %>%
  filter(temp <= 400) %>%
  summarise(mean(numberOfComments)) -> avg_cold
newDeals2 %>%
  filter(temp > 400) %>%
  summarise(mean(numberOfComments)) -> avg_hot

sprintf('Hot deals haben durchschnittlich %s Kommentare.', avg_hot)
sprintf('Cold deals haben durchschnittlich %s Kommentare.', avg_cold)

# c3) Which author posted the most deals?
# How many deals did an author post on average?

# Neues dataframe deals_per_author: mit count() wird die Anzahl der Deals pro Autor in der Spalte freq ausgegeben
deals_per_author <- as.data.frame(plyr::count(newDeals2, "author"))

# Maximum der Spalte freq ausgeben
deals_per_author %>%
  summarise(max(freq)) -> max_deals # dataframe mit einem Eintrag

# Eintrag mit entsprechendem Autor ausgeben
deals_per_author %>%
  filter(freq == max_deals[,1]) -> maximum

sprintf('%s postete mit einer Anzahl von %s die meisten Deals', maximum[,1], maximum[,2])

# Mittelwert
deals_per_author %>%
  summarise(mean(freq)) -> avg_deals

sprintf('Ein Autor postet im Durchschnitt %s Deals', avg_deals)

# c4) Based on the title, what share of deals is about Xiaomi products?

newDeals2[grep("Xiaomi",newDeals2$title),] -> Xiaomi_Deals

share_Xiaomi <- nrow(Xiaomi_Deals) / nrow(newDeals2)

sprintf('%s Prozent der Deals enthalten Xiaomi-Produkte.', share_Xiaomi*100)
