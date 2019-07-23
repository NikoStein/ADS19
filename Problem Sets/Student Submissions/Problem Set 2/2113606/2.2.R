# Problem Set 2
# Question 2

#a
#scrape at least the 1000 latest deals from starting page
#store in data frame (deals) with 5 columns (title, temperature, author, deep link, number of comments)

library (rvest)
library(tidyverse)
url = " https://www.mydealz.de/"

url %>%
  read_html() -> dealsPage

dealsPage %>%
  html_nodes("strong a") %>%
  html_text -> title

dealsPage %>%
  html_nodes("div span") %>%
  html_text -> temperature

dealsPage %>%
  html_nodes("button span") %>%
  html_text -> author

dealsPage %>%
  html_nodes("span a") %>%
  html_text -> deeplink

dealsPage %>%
  html_nodes("a span") %>%
  html_text -> ncomments

data.frame(title = title, temperature = temperature, author = author, deeplink = deeplink, ncomments = ncomments) -> deals

#b
#change cols title and temperature from latin1 to ASCII

iconv(deals$title, from="latin1", to = "ASCII")
iconv(deals$temperature, from="latin1", to = "ASCII")

#remove unneeded whitespaces, line breaks and tabulators
deals %>%
  str_remove_all("\\n") %>% #\n
  str_remove_all("[ \t]+$") %>% #trailing whitespace
  str_remove_all("^[ \t]") #leading whitespace
  str_replace_all(x, "[\r\n]" , "") #line breaks


#c
  #share of posted deals over 0 / below 0
  counthot = 0
  countcold = 0
  deals %>%
    ifelse(deals$temperature > 0, counthot + deals$temperature)
    mutate(sharehot = counthot/sum(deals$temperature))
    ifelse(delas$temperature < 0, countcold + deals$temperature)
    mutate(sharecold = countcold/sum(deals$temperature))

  #average temperature of deals
deals %>%
  summarise(avgTemp = avg(deals$temperature))

  #avg comments of hot and cold deals
deals %>%
  select(deals$temperature > 0) %>%
  sum(deals$ncomments) -> commentshot

deals %>%
  select(deals$temperature < 0) %>%
  sum(deals$ncomments) -> commentscold

#which author posted most deals?
deals %>%
  group_by(author) %>%
  sort(decreasing = T)

#how many deals posted an author on avg?
deals %>%
  group_by(author)
  summarise(avgPosts = avg(title))

#based on title, what share of deals is about "Xiaomi" products?
  deals[grep("Xiaomos", deals$title)]










