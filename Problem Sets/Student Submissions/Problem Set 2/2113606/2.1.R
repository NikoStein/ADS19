# Problem Set 2
# Question 1

#a
#scrape 10 quotes in df with col: author, tags, quotes
library(rvest)
getQutoes = function(url){
url = "http://quotes.toscrape.com/"
url %>%
  read_html() -> QuotesPage

QuotesPage %>%
html_nodes("span small") %>%
  html_text -> author

QuotesPage %>%
  html_nodes("div div") %>%
  html_text -> tags

QuotesPage %>%
  html_nodes("div span") %>%
  html_text -> quotes

}
data.frame(author = author, tags = tags, quotes = quotes)

head(10)

#b
#100 quotes in df
getQutoes()
data.frame(author = author, tags = tags, quotes = quotes) -> quotes

head(100)



#c
#scrape all authors about-page

  html_nodes("span a") -> authorURL
  url = paste0("http://quotes.toscrape.com/", authorURL)

  url %>%
    read_html() %>%
    html_text() -> authorPage



#d
#content of about-page in data frame (authorDetails with col = author, description, bornDate)
#apply to all about pages
getAuthorDetails = function(authorURL)
{
  authorURL = paste0("http://quotes.toscrape.com/", authorURL)

  authorURL %>%
    read_html() -> authorPage

  authorPage %>%
    html_nodes("div h3") %>%
    html_text() -> author

  authorPage %>%
    html_nodes("div div") %>%
    html_text() -> description

  authorPage %>%
    html_nodes("p span") %>%
    html_text() -> bornDate
}

  data.frame(author = author, description = description, bornDate = bornDate) -> authorDetails


mapply(head(authorURL), getAuthorDetails)

#e
#transform column bornDate to 3 distinct columns (day, month, year)
  authorDetails %>%
    mutate(birthDay = bornDate[1:2], birthMonth = bornDate[3:4], birthYear= bornDate[5:8])

  #How many authors were born between 1800 - 1899?
  ifelse(authorsDetails$birhtYear >= 1800 & authorsDetails$birthYear <= 1899, print(authorsDetails$author))
  print(authorDetails$author)

#author with most quotes:
  quotes %>%
    group_by(author) %>%
    sort(quotes$quote, decreasing = T)

#average quotes of author
  quotes %>%
    group_by(quotes$author)
    summarise(avgQuote = avg(quotes$quote))

#all tags "life"
    quotes %>%
      select(quotes$tag = "life")

#Join both dfs, transform keys
    combined <- left_join(quotes, authorsDetails, by = c(author))

