# Problem Set 2
# Question 2


library(tidyverse)
library(rvest)
library(purrr)
library(RSelenium)

# a)



url_base <- "https://www.mydealz.de/?page=%d"
sprintf(url_base, 1:5) -> allURLMyDealz

getInformation <- function(url){
  read_html(url) -> mydealz
  mydealz %>%
    html_nodes(".thread--mode-default .vote-temp--burn , .thread--deal .vote-temp--burn , .space--h-2.text--b") %>%
    html_text()  %>% data.frame()-> temperatur

  mydealz %>%
    html_nodes(".thread-title--list") %>%
    html_text()  %>% data.frame()-> title

  mydealz %>%
    html_nodes(".linkPlain .thread-username") %>%
    html_text()  %>% data.frame()-> author

  mydealz %>%
    html_nodes(".footerMeta-actionSlot .btn--mode-boxSec") %>%
    html_text()  %>% data.frame()-> comments

  mydealz %>%
    html_nodes(".boxAlign-jc--all-c.btn") %>%
    html_attr("href")  %>% data.frame() %>% na.omit() -> deepLink

  allInformation <- data.frame(title, temperatur, author, comments, deepLink)
  return(allInformation)
}

df <- map_df(allURLMyDealz, getInformation)

colnames(df) <- c('Title', 'Temperature', 'Author', 'Number_of_Comments', 'Deep_Link')


# b)

# Clean both columns by changing the encoding from 'latin1' to 'ASCII' using the iconv function
iconv(df$Title, from = "latin1", to = "ASCII")
iconv(df$Temperature, from = "latin1", to = "ASCII")

df$Temperature %>%
  str_remove_all("°") -> temperature_cleaned

df$Temperature <- temperature_cleaned

df$Temperature <- as.numeric(df$Temperature)
df$Number_of_Comments <- as.numeric(df$Number_of_Comments)

# Additionally, remove unneeded whitespaces, line breaks and tabulators from the data frame
df$Title %>%
  str_remove_all("\\n") %>%
  str_remove_all("[ \t]+$") %>%
  str_remove_all("^[ \t]")

df$Temperature %>%
  str_remove_all("\\n") %>%
  str_remove_all("[ \t]+$") %>%
  str_remove_all("^[ \t]")

df$Author %>%
  str_remove_all("\\n") %>%
  str_remove_all("[ \t]+$") %>%
  str_remove_all("^[ \t]")

df$Number_of_Comments %>%
  str_remove_all("\\n") %>%
  str_remove_all("[ \t]+$") %>%
  str_remove_all("^[ \t]")

df$Deep_Link %>%
  str_remove_all("\\n") %>%
  str_remove_all("[ \t]+$") %>%
  str_remove_all("^[ \t]")

# c)

# What share of the posted deals is voted hot?
df %>%
  filter(Temperature > 0) %>%
  summarise(shareHotDeals = n()/nrow(df))

# What share is voted cold?
df %>%
  filter(Temperature < 0) %>%
  summarise(shareHotDeals = n()/nrow(df))

# What is the average temperature of a deal?
df %>%
  summarise(meanTemperatur = mean(Temperature))

# On average, do hot or cold deals have more comments?
df %>%
  filter(Temperature > 0) %>%
  summarise(meanCommentsHotDeals = mean(Number_of_Comments))

df %>%
  filter(Temperature < 0) %>%
  summarise(meanCommentsHotDeals = mean(Number_of_Comments))

# Which author posted the most deals?
df %>%
  group_by(Author) %>%
  summarise(PostsPerAuthor = n()) %>%
  arrange(-PostsPerAuthor) -> df_PostsPerAuthor

# How many deals did an author post on average?
df_PostsPerAuthor %>%
  summarise(avg = mean(PostsPerAuthor))

# Based on the title, what share of deals is about Xiaomi products?
df %>%
  filter(grepl("Xiaomi", df$Title, fixed = T)) %>%
  summarise(shareXiaomiProducts = n()/nrow(df))





