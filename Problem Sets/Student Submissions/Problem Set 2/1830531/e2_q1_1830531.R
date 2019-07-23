# Problem Set 2
# Question 1

#a)

library(tidyverse)
library(rvest)

url = 'http://quotes.toscrape.com/'

# read in url and extract nodes from page 1
read_html(url) -> quote
quote %>%
  html_nodes(".quote")-> node

# scrape author, quote itself and the tags for each quote.
GetQuotes <- function(nodes){
  tibble(author = nodes %>%
           html_nodes(".author") %>%
           html_text(),
         quotes = nodes %>%
           html_nodes(".text") %>%
           html_text(),
         tags = nodes %>%
           html_nodes(".tags .tag") %>%
           html_text() %>%
           paste(collapse = ", "))

}

# map function over all the quotes in page 1
map_df(node, GetQuotes)



#b)

urlbase <- 'http://quotes.toscrape.com/page/%d/'

# now get the nodes for the 10 pages and map the function over each of them
quotes <- map_df(1:10, function(i){

  qt <- read_html(sprintf(urlbase, i))
  nodes <- qt %>%
    html_nodes(".quote")
  map_df(nodes, GetQuotes)

})


# c)

urlbase <- 'http://quotes.toscrape.com/page/%d/'

# map over all the pages and paste together authorpage-urls
authorurl <- map_chr(1:10, function(i){
  read_html(sprintf(urlbase, i)) %>%
    html_nodes(".quote span a") %>%
    html_attr("href") %>%
    paste(collapse = "")

  })

# first make one vector out of the vectors the pages returned
# the split it to get one string for every author
authorurl %>%
  paste(collapse = "") %>%
  str_split("/author") %>%
  unlist() -> authorurl
authorurl[-1] -> authorurl

# function pastes author-url-ending onto the base url
# and extracts the name, description and the birthdate from authorpage
GetAuthors <- function(urls){
  author <- read_html(paste0("http://quotes.toscrape.com/author", urls))

  tibble(

    name = author %>%
      html_nodes(".author-title") %>%
      html_text() %>%
      str_trim(),

    description = author %>%
      html_nodes(".author-description") %>%
      html_text() %>%
      str_trim(),

    bornDate = author %>%
      html_nodes(".author-born-date") %>%
      html_text() %>%
      str_trim()
  )

}
# now map over all the authorpages and remove the redundant ones
map_df(authorurl, GetAuthors) %>%
  distinct()  -> authorDetails

#e)

#i)

# divide up the bornDate and reorder the new columns
authorDetails %>%
  separate(bornDate, c("month", "day", "year")) %>%
  .[,c(1,2,4,3,5)]-> i_authorDetails

i_authorDetails %>%
  filter(year >= 1800 & year <= 1899) %>%
  summarize(n())

#ii)
# Einstein has the most quotes
quotes %>%
  group_by(author) %>%
  summarize(quantity = n()) %>%
  arrange(desc(quantity)) -> quant_quotes

# an author has, on average, 2 quotes to his name
mean(quant_quotes$quantity)

life_quotes <- quotes[grep("life(\n|,|$)", quotes$tags),]

nrow(life_quotes)

# iii)

left_join(authorDetails, quotes, by = c("name" = "author")) -> authorDetailsjoin








