# Problem Set 3
# Question 2

library(jsonlite)

if (!file.exists("clinton_emails.rda")) {
  clinton_emails <- fromJSON("http://graphics.wsj.com/hillary-clinton-email-documents/api/search.php?subject=&text=&to=&from=&start=&end=&sort=docDate&order=desc&docid=&limit=27159&offset=0")$rows
  save(clinton_emails, file="clinton_emails.rda")
}

load("clinton_emails.rda")

clinton_emails %>%
  mutate(from=trimws(from),
         to=trimws(to)) %>%
  filter(from != "") %>%
  filter(to != "") %>%
  filter(!grepl(";", from)) %>%
  filter(!grepl(";", to)) -> clinton_emails

library(tidygraph)
library(tidyverse)
library(ggraph)
library(RCurl)


clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  ggraph() +
  geom_edge_link() +
  centrality_degree() +
  geom_node_point() +
  geom_node_text(aes(label = name))

clinton_emails %>%
filter((to == "Hillary Clinton") | (from == "Hillary Clinton")) -> emails



emails %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree()) %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(size = log(centrality))) +
  geom_node_label(aes(filter = centrality > 500, label = name),repel = T, label.padding = 0.1)


emails %>%
  group_by(from , to) %>%
  summarize(n = n()) -> edges



left_join(emails, edges) -> combined

combined %>%
  distinct(to, from, .keep_all = TRUE) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree()) %>%
  ggraph() +
  geom_edge_link(aes(width=n), alpha = 0.4) +
  geom_node_point(aes(size = log(centrality))) +
  geom_node_label(aes(filter = centrality > 500, label = name),repel = T, label.padding = 0.1)




