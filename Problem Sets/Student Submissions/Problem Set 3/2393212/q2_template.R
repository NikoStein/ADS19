# Problem Set 3
# Question 2

library(sp)
library(jsonlite)
library(tidygraph)
library(tidyverse)
library(ggraph)


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
  select(to,from) %>%
  mutate(weight = 1) %>%
  group_by(to, from) %>%
  summarise(weight = sum(weight)) -> clinton_emails

clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name))

# b)
# the sub-graphs exist, because there are mails, where h. clinton was not included or maybe just in CC.
clinton_emails %>%
  filter(to == "Hillary Clinton" | from == "Hillary Clinton") -> clinton_emails

clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name)) -> clinton_graph
clinton_graph

# c)
# Size the nodes according to their logarithmic centrality degree.
# Additionally, add labels only to nodes with a centrality higher than 500.
clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  mutate(neighbors = centrality_degree()) -> clinton_graph_c

layout <- create_layout(clinton_graph_c,
                        layout = "fr")

ggraph(layout) +
  geom_edge_density(aes()) +
  geom_edge_link(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(size=log(weight))) -> network





