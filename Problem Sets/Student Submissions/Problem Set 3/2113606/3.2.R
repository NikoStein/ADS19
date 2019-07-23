# Problem Set 3
# Question 2


library(tidygraph)
library(tidyverse)
library(ggraph)
library(RCurl)
library(igraph)

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


#a
#transform to table graph object
clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  ggraph() +
  geom_edge_link(aes(width = weight), alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name)) -> graph

#b
#subgraphs: Mail correspondence between two people (nodes), without connection to Clinton
#remove:
clinton_emails %>%
  filter(from != "Hillary Clinton" & to != "Hillary Clinton") -> graph
#c
#size nodes according to thei log centrality degree
#add labels to nodes with centrality > 500
V(graph)$size <- centrality_degree(graph)$res
filter(V(graph)$size >= 500)

#d
#additonal infos: edge width should show the number of interactions
#Leverage your data wrangling skills to count the number of interactions between each pair of nodes
clinton_mails %>%
  mutate(n = count(from, to))
E(graph)$weight <- n

#Join the new data frame to the email data frame

#Remove duplicate edges
simplify(graph, edge.attr.comb="sum")

#Plot the graph and remove all legends

ggraph(graph) +
  geom_edge_density(aes()) +
  geom_edge_link(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(color = factor(group),
                      size=log(sum_weight),
                      shape=keyplayer)) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() -> network
