# Problem Set 3
# Question 1


library(tidyverse)
library(readr)
library(RColorBrewer)
# 1 a)

data <- read_csv2("https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv",
                  skip = 9)
# consider only big parties
Parteien <- c("CDU","CSU", "GRÜNE", "SPD", "AfD", "DIE LINKE", "FDP")

# 1 b)

#filter out the state level and performance of all the big parties
data %>%
  filter(UegGebietsart == "BUND", Gruppenart == "Partei") -> laender

# col-graph for every state and election result colored in
laender %>%
  filter(Gruppenname %in% Parteien) %>%
  ggplot(aes(x = Gebietsname, y = Prozent, fill = Gruppenname)) +
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.y = element_text(size = 5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 6))

# faceting, to show the results for every state separately
laender %>%
  filter(Gruppenname %in% Parteien) %>%
  ggplot(aes(x = Gruppenname, y = Prozent)) +
  geom_col() +
  facet_wrap(~Gebietsname)

# 1 c)

# turn it around and show the parties' performance for every state
pal = colorRampPalette(brewer.pal(9, "Set1"))

laender %>%
  filter(Gruppenname %in% Parteien[-2]) %>%
  ggplot(aes(x = Gebietsname, y = Prozent, fill = factor(Gebietsname)))+
  scale_fill_manual(values = pal(16)) +
  geom_col() +
  facet_wrap(~Gruppenname) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.y = element_text(size = 5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 6),
        legend.position = "none")

# 1 d)
library(leaflet)
library(raster)

laender %>%
  group_by(Gebietsname) %>%
  filter(Prozent == max(Prozent)) -> winner

# load level 1 map for germany
germany <- getData("GADM", country="DEU", level=1)

# create dataframe with information we want to map onto the germany map
election = winner %>% ungroup() %>% dplyr::select(Gruppenname, Gebietsname, Prozent)

# merge the spdf with the election results
merge(germany, election, by.x = "NAME_1", by.y = "Gebietsname") -> germany_joined


pal <- colorFactor("Set1", NULL)
polygon_popup <- paste0("<strong>Bundesland: </strong>", germany_joined$NAME_1, "<br>",
                        "<strong>Gewinnerpartei: </strong>", germany_joined$Gruppenname, "<br>",
                        "<strong>Anteil: </strong>", round(germany_joined$Prozent, 1),
                        "<strong> % </strong>")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = germany_joined,
              fillOpacity = 0.65,
              fillColor= ~pal(Gruppenname),
              weight = 1,
              color = "white",
              popup = polygon_popup)
# 1 e)
# filter the data for the winner of every constituency with the respective share of votes
data %>%
  filter(Gebietsart == "Kreis", Gruppenart == "Partei") %>%
  group_by(Gebietsname) %>%
  filter(Prozent == max(Prozent)) -> kreise

# load the map of level 2 for germany
germanyKreise <- getData("GADM", country="DEU", level=2)

germanyKreise$NAME_2
# map is not up to data, in the meantime Göttingen and Osterode merged into one constituency
germanyKreise[["CC_2"]][229] = "03159"
germanyKreise[["CC_2"]][211] = "03159"
germanyKreise[["NAME_2"]][229] = "Göttingen"

# extract information about the elections into one dataframe
electionKreise = kreise %>% ungroup() %>% dplyr::select(Gruppenname, Gebietsname, Prozent, Gebietsnummer)

# merge the dataframe with the spatial_df
merge(germanyKreise, electionKreise, by.x = "CC_2", by.y = "Gebietsnummer") -> germanyKreise_joined


# create the colors for the seven parties
pals <- colorFactor(c("black","#6495ed","#32CD32", "red","#0000ee","#FF6961","yellow"), levels = Parteien, Parteien)

# normalize the percentages in order to fill in the opacity
normal <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

# produce the pop-up display
polygon_popup <- paste0("<strong>Kreis: </strong>", germanyKreise_joined$NAME_2, "<br>",
                        "<strong>Gewinnerpartei: </strong>", germanyKreise_joined$Gruppenname, "<br>",
                        "<strong>Anteil: </strong>", round(germanyKreise_joined$Prozent, 2),
                        "<strong> % </strong>")
# create the mapping via leaflet
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = germanyKreise_joined,
              fillOpacity = ~normal(Prozent, na.rm = TRUE),
              fillColor= ~pals(Gruppenname),
              weight = 2,
              color = "white",
              popup = polygon_popup)




