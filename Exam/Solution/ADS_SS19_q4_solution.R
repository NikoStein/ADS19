# Matriculation number: ...

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)


url="https://stackoverflow.com/questions/tagged/r?tab=Unanswered&pagesize=50&page=1"


# a)
urls = paste0("https://stackoverflow.com/questions/tagged/r?tab=Unanswered&pagesize=50&page=", 1:3) 

# b)
extractNodeInformation = function(node)
{
  node %>%
    html_nodes(".question-hyperlink") %>%
    html_attr("href") -> url
  
  node %>%
    html_nodes(".post-tag") %>%
    html_text() -> tags
  
  node %>%
    html_nodes(".relativetime") %>%
    html_text() -> time
  
  data.frame(tag = tags, time = time, url = url)
}

# c)
getNodesAndInformationFromUrl = function(url)
{
  url %>%
    read_html() %>%
    html_nodes(".question-summary") %>%
    map_df(extractNodeInformation)
}


# d) 
allTags = map_df(urls, getNodesAndInformationFromUrl)

allTags %>%
  group_by(tag) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

