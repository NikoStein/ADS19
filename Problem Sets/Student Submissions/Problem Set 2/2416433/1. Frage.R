# Problem Set 2
# Question 1


# a)
library(tidyverse)
library(rvest)
library(purrr)

url_base = "http://quotes.toscrape.com/page/%d/"


sprintf(url_base, 1:10) -> allURL



getQuotes <- function(url){
  read_html(url) -> quoteWebsite
  quoteWebsite %>%
  html_nodes(".text") %>%
  html_text() -> text

quoteWebsite %>%
  html_nodes(".author") %>%
  html_text() -> authors

quoteWebsite %>%
  html_nodes(".tags") %>%
  html_text() -> tags

tags %>%
  str_remove_all("Tags") %>%
  str_remove_all(":")-> tagsCleaned

df <- data.frame(text, authors, tagsCleaned)
return(df)
}
df_first_page <- getQuotes(allURL[[1]])

# b)


df_final2 <- map_df(allURL, getQuotes)
colnames(df_final2) <- c('Quote', 'Author', 'Tag')

#------------------------------------------------------------------------------

#df_text <- map_df(1:10, function(i){
#  read_html(sprintf(url_base, i)) -> quoteWebsite
#  quoteWebsite %>%
#    html_nodes(".text") %>%
#    html_text() %>% data.frame()})


#df_authors <- map_df(1:10, function(i){
#  read_html(sprintf(url_base, i)) -> quoteWebsite
#  quoteWebsite %>%
#    html_nodes(".author") %>%
#    html_text() %>% data.frame()
#})

#df_tags <- map_df(1:10, function(i){
#  read_html(sprintf(url_base, i)) -> quoteWebsite
#  quoteWebsite %>%
#    html_nodes(".tags") %>%
#    html_text() %>%
#    str_remove_all("Tags") %>%
#    str_remove_all(":") %>% data.frame()
#})

#df_gesamt <- data.frame(df_text, df_authors, df_tags)
#colnames(df_gesamt) <- c('Quote', 'Author', 'Tag')

# -----------------------------------------------------------------------

# c)

# Get all authors URLs
df_authors_url <- map_df(1:10, function(i){
  read_html(sprintf(url_base, i)) -> quoteWebsite
  quoteWebsite %>%
    html_nodes(".quote span a") %>%
    html_attr("href") %>% data.frame()
})

# d) Ein Autor

url_base_author = "http://quotes.toscrape.com%s"

getAuthorInformation <- function(url, i){
  read_html(sprintf(url, i)) -> quoteWebsite
  quoteWebsite %>%
    html_nodes(".author-title") %>%
    html_text() -> auhtorName

  quoteWebsite %>%
    html_nodes(".author-born-date") %>%
    html_text() -> auhtorBornDate

  quoteWebsite %>%
    html_nodes(".author-description") %>%
    html_text() -> auhtorDescription

  data.frame(auhtorName, auhtorBornDate, auhtorDescription)

}
df_author_infotmation <- getAuthorInformation(url_base_author ,df_authors_url[[1,1]])

# Alle Autoren

df_author_names <- map_df(1:100, function(i){
  read_html(sprintf(url_base_author, df_authors_url[[i,1]])) -> quoteWebsite
  quoteWebsite %>%
    html_nodes(".author-title") %>%
    html_text() %>% data.frame()})

df_author_born <- map_df(1:100, function(i){
  read_html(sprintf(url_base_author, df_authors_url[[i,1]])) -> quoteWebsite
  quoteWebsite %>%
    html_nodes(".author-born-date") %>%
    html_text() %>% data.frame()})

df_author_description <- map_df(1:100, function(i){
  read_html(sprintf(url_base_author, df_authors_url[[i,1]])) -> quoteWebsite
  quoteWebsite %>%
    html_nodes(".author-description") %>%
    html_text() %>% data.frame()})

df_gesamt_authors <- data.frame(df_author_names, df_author_born, df_author_description)
colnames(df_gesamt_authors) <- c('Name', 'BornDate', 'Description')

# e)
# i)
df_gesamt_authors %>%
  rowwise() %>%
  mutate(Day = strsplit(BornDate, split = ' ')[[1]][2],
         Month = strsplit(BornDate, split = ' ')[[1]][1],
         Year = strsplit(BornDate, split = ' ')[[1]][3]) -> df_gesamt_authors_edit

df_gesamt_authors_edit$Day %>% str_replace_all(",", "") -> day_cleaned
df_gesamt_authors_edit$Day <- day_cleaned
df_gesamt_authors_edit$BornDate <- NULL

# How many authors where born in the 19th century (1800-1899)?
df_gesamt_authors_edit %>%
  filter(Year >= 1800 & Year <= 1899) %>%
  nrow()



# ii)
# 1
df_final2 %>%
  group_by(Author) %>%
  summarise(numberAppearences = n()) %>%
  arrange(-numberAppearences) -> mostNumberofAppearences

mostNumberofAppearences %>% head(1)

# 2
mostNumberofAppearences %>%
  summarise(avg = mean(numberAppearences))
# 3
df_final2 %>%
  filter(grepl("life", df_final2$Tag, fixed = T)) -> df_gesamt_tag_life


df_final2$ID <- seq.int(nrow(df_final2))
df_gesamt_authors_edit$ID <- seq.int(nrow(df_gesamt_authors_edit))

df_final <- merge(df_final2, df_gesamt_authors_edit, by.x = "ID", by.y = "ID")
df_final$Name <- NULL
