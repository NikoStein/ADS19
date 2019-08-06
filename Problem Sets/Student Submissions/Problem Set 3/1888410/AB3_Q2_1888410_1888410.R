# Problem Set 3
# Question 2


require(tidygraph)
require(ggraph)
require(tidyverse)
require(jsonlite)

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


#2. Your second task is to analyze the infamous Hillary Clinton email data set. The code to load
#and clean the data is already in the submission script.

# a.) Transform the data set into a table graph object and visualize the email network as following:

clinton_emails %>%
  as_tbl_graph(directed = F) %>%
  ggraph() +
  geom_edge_link(alpha = 0.1) +
  geom_node_point() +
  geom_node_text(aes(label = name))



  # b.) Try to explain the small sub-graphs that are not connected to Hillary Clinton and clean the data set to remove them. Visualize the network again.


  #Antwort: Die sub-graphs enstehen durch den E-mail Austausch von anderen Personen untereinander ohne das Hillary Clinton als direkter oder indirekter (CC) Empfaenger involviert war.
  # Vielleicht wurden Sie beim zusammenstellen des Datensatzes ausversehen mit uebernommen.

  clinton_emails %>%
    filter_at(.vars = vars(to, from, originalFrom, originalTo),
              .vars_predicate = any_vars(str_detect(., "Hillary Clinton|Clinton, Hillary"))) -> clinton_only


  clinton_only %>%
    as_tbl_graph(directed = F) %>%
    ggraph() +
    geom_edge_link(alpha = 0.1) +
    geom_node_point() +
    geom_node_text(aes(label = name))



  # c.) Next, we want to improve the graph visualization by taking the centrality of the
  # nodes into account. Specifically, you should size the nodes according to their
  # logarithmic centrality degree. Additionally, add labels only to nodes with a centrality higher than 500.

  clinton_only %>%
    as_tbl_graph(directed = F) %>%
    mutate(neighbors = centrality_degree()) %>%
    ggraph() +
    geom_edge_link(alpha = 0.1) +
    geom_node_point(aes(size = log(neighbors))) +
    #geom_node_text(data = . %>% filter(neighbors > 500), aes(label = name), size = 4, repel = TRUE) +
    geom_node_label(data = . %>% filter(neighbors > 500), aes(label = name), color = "steelblue4", size = 3, repel = TRUE) +
    theme_graph()




  # d.) Finally, we want to add additional information into our figure. More specifically, we
  # want the edge-width to show how often the nodes interact with each other.

      # i.) Leverage your data wrangling skills to count the number of interactions between each pair of nodes


      clinton_only %>%
        group_by(to,from) %>%
        mutate(Count = n()) # Anzahl der Interaktionen zwischen Empfaenger und Sender



      # ii.) Join the new data frame to the email data frame


      # Join ist hier nicht mehr noetig da die neue Spalte Count gleich am Datensatz angehaengt wurde
      # (Wenn man mit summarise statt mutate eine neuen Datensatz erstellt haette, dann haette man an dieser Stelle ihn jetzt an den clinton df joinen koennen.)
      clinton_only %>%
        group_by(to,from) %>%
        mutate(Count = n())



      # iii.) Remove duplicate edges

      clinton_only %>%
        group_by(to,from) %>%
        mutate(Count = n()) %>%
        as_tbl_graph(directed = F) %>%
        mutate(neighbors = centrality_degree()) %>%
        activate(edges) %>%  # Um duplicate edges filtern zu koennen muessen die edges erst aktiviert werden
        filter(!edge_is_multiple()) #Filtert alle edges die nicht mehrfach vorkommen



      # iv.) Plot the graph and remove all legends

      clinton_only %>%
        group_by(to,from) %>%
        mutate(Count = n()) %>%
        as_tbl_graph(directed = F) %>%
        mutate(neighbors = centrality_degree()) %>%
        activate(edges) %>%
        filter(!edge_is_multiple()) %>%
        #ggraph(layout = "graphopt") + #Layout aendern
        ggraph() +
        geom_edge_link(aes(width = Count), color = "gray46", alpha = 0.3, show.legend = FALSE) + # Kanten/Verbindungslinien
        geom_node_point(aes(size = log(neighbors)), show.legend = FALSE) + # Punkte
        geom_node_label(data = . %>% filter(neighbors > 500), aes(label = name), color = "steelblue4", size = 3, repel = TRUE) + # Labels (nur Text dann geom_node_text)
        theme_graph()






