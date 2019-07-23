# Problem Set 2
# Question 2


library(tidyverse)
library(rvest)
library(xml2)
library(purrr)
library(repurrrsive)
library(stringr)

# a) und teilweise b)

# title, temperature, author, deep link, number of comments
# a.thread-title--list ((title
# span.vote-temp--burn t((emperature
# span.thread-username ((author
# a.thread-title--list href attribute ((deep link
# span.footerMeta-actionSlot a span
getDealsFromLink <- function(url) {
  url <- read_html(url)
  data <-
    data.frame(
      title = html_nodes(url, css = 'a.thread-title--list') %>% html_text() %>% str_replace_all("[\n,\t]*", ""),
      temperature = ifelse(
        length(html_text(
          html_nodes(url, css = 'span.vote-temp')
        )) == 0,
        html_text(html_nodes(url, css = 'div.text--color-grey span')) %>% str_replace_all("[\n,\t,°]*", ""), #0
        html_text(html_nodes(url, css = 'span.vote-temp')) %>% str_replace_all("[\n,\t,°]*", "")
      ),
      author = ifelse(
        length(html_text(
          html_nodes(url, css = 'span.vote-temp')
        )) == 0,
        'Unbekannt',
        html_nodes(url, css = 'span.thread-username') %>% html_text()
      ),
      deepLink = html_nodes(url, css = 'a.thread-title--list') %>% html_attr('href'),
      commentsCount = html_nodes(url, css = 'span.footerMeta-actionSlot a span') %>% html_text(),
      stringsAsFactors = F
    )
  data
}


map_df(1:51, function(i) {
  page <- sprintf('https://www.mydealz.de/new?page=%d', i)
  pagedfs <- getDealsFromLink(page)
  pagedfs
}) %>%
  head(1000) -> listOfDeals

# b)
iconv(listOfDeals, "latin1", "ASCII")
listOfDeals$temperature = as.numeric(listOfDeals$temperature)
listOfDeals$commentsCount = as.numeric(listOfDeals$commentsCount)

#A c) i)
#greater than 0 %
greaterThan <-
  listOfDeals[listOfDeals$temperature > 0, ] %>% nrow()
lowerThan <- listOfDeals[listOfDeals$temperature <= 0, ] %>% nrow()

greaterThan = 1 / nrow(listOfDeals) * greaterThan
greaterThan

lowerThan = 1 / nrow(listOfDeals) * lowerThan
lowerThan

listOfDeals$temperature %>%
  mean(na.rm=TRUE)

# c) ii)
listOfDeals %>%
  filter(listOfDeals$temperature > 0) %>%
  summarise(mean = mean(commentsCount, na.rm=TRUE))

listOfDeals %>%
  filter(listOfDeals$temperature <= 0) %>%
  summarise(mean = mean(commentsCount, na.rm=TRUE))

# c) iii)
listOfDeals %>%
  group_by(author) %>%
  summarise(number = n()) -> maxDeals

maxDeals[which.max(maxDeals$number),]

mean(maxDeals$number)

# c iv)
listOfDeals %>%
  filter(str_detect(title, 'Xiaomi')) -> xaomiDeals

nrow(xaomiDeals)/nrow(listOfDeals)
