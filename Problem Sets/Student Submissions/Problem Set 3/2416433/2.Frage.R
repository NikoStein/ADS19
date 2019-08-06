# Problem Set 3
# Question 2


library(tidyverse)
library(tidygraph)
library(ggraph)
library(jsonlite)
library(RCurl)


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

# a)

clinton_emails %>%
  group_by(to, from) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  arrange(desc(weight)) -> clinton_emails_sorted

clinton_emails_sorted %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name))

# b)

clinton_emails_sorted %>%
  filter(from == "Hillary Clinton" | to == "Hillary Clinton") -> clinton_emails_just_hillary

clinton_emails_just_hillary %>%
  as_tbl_graph(directed = F) %>%
  activate(nodes) %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name))

# c)

clinton_emails_just_hillary %>%
  gather(x, name, to:from) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(weight)) %>%
  ungroup() %>%
  arrange(desc(sum_weight))-> clinton_emails_just_hillary_weighted


as_tbl_graph(clinton_emails_just_hillary, directed = F) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap(),
         keyplayer = node_is_keyplayer(k = 10),
         centrality = centrality_authority()) %>%
  left_join(clinton_emails_just_hillary_weighted) %>%
  activate(edges) %>%
  filter(!edge_is_multiple()) -> table_graph1

layout1 <- create_layout(table_graph1,layout = "fr")

ggraph(layout1) +
  geom_edge_density() +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(size=log(sum_weight))) +
  geom_node_text(aes(filter=centrality > 0.051, label = name),repel = T, color = 'red')

# d)

layout2 <- create_layout(table_graph1,layout = "sphere")

ggraph(layout2) +
  geom_edge_density() +
  geom_edge_link(aes(width = weight),alpha = 0.2) +
  geom_node_point(aes(size=log(sum_weight))) +
  geom_node_text(aes(filter=centrality > 0.051, label = name),repel = T, color = 'red')+
  theme_graph() +
  theme(legend.position = "none")






