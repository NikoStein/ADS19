# Problem Set 2
# Question 2

# Name: Philipp Seufert
# Matrikelnummer: 1888410


require(tidyverse)
require(rvest)

# a. First, use rvest to scrape at least the 1000 latest deals. In order to minimize server traffic, you should scrape all the relevant information from the starting page and do 
# not need to follow the deep links to the particular deals. Your function should return a data frame (deals) with 5 columns (title, temperature, author, deep link, number of comments).

#Aufbau der Internetseite: https://www.mydealz.de/?page=3
#Latest deals Internetseite: https://www.mydealz.de/new?page=3
url = 'https://www.mydealz.de/new?page='

page <- c(1:150) #150 Seite*20 Eintraege = 3000 Deals

#Notiz: Merke, hier wird über einzelne Nodes iteriert nicht pages (siehe q1 template) deswegen hat die Variante vorher nicht funktioniert!
getDeals = function(node){
  
  # Nimmt auf jeder Seite die 20 Nodes und extrahiert (title, temperature, author, deep link, number of comments) in ein data frame
  #
  # Args:
  #   node: Einzelne Node einer Seite (20 Nodes pro Seite), 
  #   (Notiz: Durch diese Funktion wird jede Node einzeln bearbeitet nicht wie in der anderen Variante wo auf einmal 20 Nodes pro Seite bearbeitet werden)
  #
  # Returns:
  #   dataframe with columns (title, temperature, author, deep link, number of comments).
  
  #Title
  node %>%
    html_nodes('.thread-title--list') %>%
    html_text() %>%
    str_replace_all("[\\n\\t\\°]+", "") -> title
  
              #Title (andere Variante)
              #node %>%
              #  html_nodes('div a img') %>%
              #  html_attr("alt") %>%
              #  str_replace_all("[\\n\\t\\°]+", "") -> title
              
  #Temperature
  node %>%
    html_nodes('.border--color-borderGrey') %>%
    html_text()  %>%
    str_replace_all("[Abgelaufen\\n\\t\\°]+", "")-> temperature
  
  #Author
  node %>%
    html_nodes('.linkPlain .thread-username') %>%
    html_text() -> author
  
  #Deep Link
  node %>%
    html_nodes('.boxAlign-jc--all-c') %>%
    html_attr("href") %>%
    na.omit()-> link
  
              #Link (andere Variante)
              #node %>%
              #  html_nodes('div strong a') %>%
              #  html_attr("href") %>%
              #  na.omit()-> link
  
  #Number of Comments
  node %>%
    html_nodes('.space--h-3.btn--mode-boxSec') %>%
    html_nodes('span') %>%
    html_text() -> comments
  
  # Fehlende Werte abfangen und mit NA ersetzen
  title = ifelse(length(title) == 0, NA, title)
  temperature = ifelse(length(temperature) == 0, NA, temperature)
  author = ifelse(length(author) == 0, NA, author)
  link = ifelse(length(link) == 0, NA, link)
  comments = ifelse(length(comments) == 0, NA, comments)
  
  deals = data.frame(Title = title, Temperature = temperature, Author = author, DeepLink = link, NumberOfComments = comments)
}



getData = function(page){
  
  # Iteriert ueber die Anzahl der Seiten (pages) und erstellt fuer jede Seite alle Nodes und
  # erstellt dann mitHilfe der getDeals Funktion den Data Frame pro Seite
  #
  # Args:
  #   page: Seiten Nummer 
  #
  # Returns:
  #   dataframe mit allen deals auf allen pages
  
  dealsURL = paste0(url,page)
  
  read_html(dealsURL) -> rawData
  
  rawData %>%
    html_nodes('.thread--type-list .threadGrid') -> nodes
  
  
  testdeals = map_df(nodes, getDeals)
}

# iteriert ueber alle Seiten und pro Seite 20 neue Deals (Deals pro Node) zum Datensatz deals hinzu
deals <- map_df(page, getData)



# b. Having collected the data set you will probably notice that there are some arcane symbols in the title and temperature columns. Clean both columns by changing the
# encoding from ‘latin1’ to ‘ASCII’ using the iconv function. Additionally, remove unneeded whitespaces, line breaks and tabulators from the data frame.


    # 1. Change the encoding from ‘latin1’ to ‘ASCII’ 
    #deals$Title <- iconv(deals$Title, "latin1", "ASCII", sub = "") # kann noch mit sub = "",byte oder NA 
        
        #-> Mit iconv kann ich doch nur die Umlaute ersetzen lassen in diesem Fall mit einem Leerzeichen, 
        #   das ergibt aber doch keinen Sinn hier, macht den Titel nur schlimmer.
    
    # Das sinnvollste in meinen Augen waere hoechstens diese Variante da werden die Umlaute wenigstens durch den ersten Buchstaben ersetzt, z.B. ü -> u statt ue
    #deals$Title <- stringi::stri_trans_general(deals$Title, "latin-ascii")
    
    
    # 2. Whitespace, line breaks and tabulators were already removed in a)
    
    # 3. Temperature column:
    
        #Replace N values with 0 (Temperature N (Neu) mit 0 ersetzen)
        deals$Temperature[deals$Temperature == 'N'] <- 0
        
        #Change Temperature column to numeric
        deals %>% 
          mutate_at(vars(Temperature), as.numeric) -> deals
        
        #verschiedene Tests
              # Check if Numeric
              is.numeric(deals$Temperature)
              #Auf Eintraege mit N checken
              nrow(deals[deals$Temperature == 'N',])
              # teste Temperature cSpalte auf NA oder Infinite 
              any(is.na(deals$Temperature) | is.infinite(deals$Temperature))
              #Test auf is.null
              is.null(deals)
    
    # 4. Number of comments column:
              
        #Change Temperature column to numeric
        deals %>% 
          mutate_at(vars(NumberOfComments), as.numeric) -> deals    
        
    
    

# c.) Use your data wrangling skills to answer the following questions:
        
      #   i.) What share of the posted deals is voted hot (over 0 degrees)? What share is voted cold (below 0 degrees)? What is the average temperature of a deal?
        
            # Hot Deals
            deals %>% 
              summarise(Percentage_Hot = (nrow(deals[deals$Temperature > 0,]) / nrow(deals)) *100)
            # A: 93,9 % of the posted deals are voted hot.
            
            # Cold Deals
            deals %>% 
              summarise(Percentage_Cold = (nrow(deals[deals$Temperature < 0,]) / nrow(deals)) *100)
            # A: 5,86% of the posted deals are voted cold.
            
            # Average Temperature
            deals %>% 
              summarise(Average_Temperature = mean(deals$Temperature))
             # A: The average temperature of a deal is 426 degrees
                    
              
            
      #   ii.) On average, do hot or cold deals have more comments?
            
            deals %>% 
              #mutate(Temp_Group = ifelse(Temperature > 0, "Hot", "Cold")) %>% # Mit nur zwei Gruppen und Annahme = 0 -> cold (nur 7 Eintraege haben 0)
              mutate(Temp_Group = ifelse(Temperature > 0, "Hot", ifelse(Temperature < 0, "Cold", "Neutral"))) %>%
              group_by(Temp_Group) %>%
              #na.omit() %>%
              summarise(Mean = mean(NumberOfComments))
            # A: Hot deals have on average more comments (29.6) compared to cold comments (11.9) and neutral (2.14).
            

            
      #   iii.) Which author posted the most deals? How many deals did an author post on average?
            
            # Author posted the most deals
            deals %>%
              group_by(Author) %>%
              summarise(Count = n()) %>%
              arrange(-Count)
            # A: The author 'Funmilka' posted the most deals with 189 deals.
            
            # Deals an author post on average
            deals %>%
              group_by(Author) %>%
              summarise(Count = n()) %>%
              na.omit() %>%
              summarise(Mean = mean(Count)) 
            # A: An author post on average 2.15 deals.


            
      #   iv.) Based on the title, what share of deals is about Xiaomi products?
            
            # Amount of Xiaomi products
            deals %>%
              filter(str_detect(Title, "Xiaomi")) %>%
              summarise(Count = n())
            # A: There are 88 Xiaomi products.
            
            # Percentage of Xiaomi products
            deals %>%
              filter(str_detect(Title, "Xiaomi")) %>%
              summarise(Percentage = (n()/nrow(deals))*100)
            # A: 2,93% of the products are about Xiaomi.
            
    

            
      