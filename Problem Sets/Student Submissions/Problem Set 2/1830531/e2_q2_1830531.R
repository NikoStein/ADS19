# Problem Set 2
# Question 2

# 2

library(tidyverse)
library(rvest)

# Extract the variables of interest out of each deal
GetDeals <- function(list) {

  title = list %>%
    html_nodes(".thread-title--list") %>%
    html_text() %>%
    str_trim()

  temperature = list %>%
    html_nodes(".bRad--a") %>%
    html_text() %>%
    str_trim() %>%
    str_extract("^[-+]?[0-9]*")

  author = list %>%
    html_nodes(".thread-username") %>%
    html_text()

  deeplink = list %>%
    html_nodes(".btn--mode-primary") %>%
    html_attr("href")

  nComments = list %>%
    html_nodes(".space--h-3.btn--mode-boxSec") %>%
    html_text()

  title = ifelse(length(title) == 0, NA, title)
  temperature = ifelse(length(temperature) == 0, NA, as.numeric(temperature))
  author = ifelse(length(author) == 0, NA, author)
  deeplink = ifelse(length(deeplink) == 0, NA, deeplink)
  nComments = ifelse(length(nComments) == 0, NA, as.numeric(nComments))

  tibble(title, temperature, author, deeplink, nComments)
}

url <- "https://www.mydealz.de/new?page=%d"

# Utilize the function over each element inside the nodes and create the nodes for each page

deals <- map_df(1:60, function(i){

  read_html(sprintf(url, i)) %>%
    html_nodes(".thread--deal .threadGrid") %>%
    map_df(GetDeals)

}
)

#c)

deals %>%
  filter(temperature > 0) %>%
  summarize(overall = nrow(deals), nHOT = n(),
            Hot_in_Perc = n()/nrow(deals)*100,
            meanTemp = mean(temperature, na.rm = TRUE),
            meanComments = mean(nComments))

deals %>%
  filter(temperature <= 0) %>%
  summarize(overall = nrow(deals), nCold = n(),
            COld_in_Perc = n()/nrow(deals)*100,
            mean = mean(temperature, na.rm = TRUE),
            meanComments = mean(nComments))

mean(deals$temperature, na.rm = TRUE)

# hot dealz have on average more comments

deals %>%
  group_by(author) %>%
  summarize(nDealz = n()) %>%
  arrange(desc(nDealz)) -> AuthorDealz

AuthorDealz

mean(AuthorDealz$nDealz)

# 1.725 on average per author

deals %>%
  filter(grepl("(?i)xiaomi", title)) %>%
  summarize(n = n(), Xi_in_Perc = n / nrow(deals)*100)



