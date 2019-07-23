# Problem Set 3
# Question 1

#a
#load data, skip 9 lines, filter to Bundesland and Partei, handle Unicode formatting
library(tidyverse)

"https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv" %>%
  read_csv2(skip = 9) -> eu_elections

glimpse(eu_elections)

eu_elections %>%
  filter(Gebietsart == 'Bund', Gruppenart == 'Partei') %>%
  iconv(Gruppenname, from = latin1, to = ASCII, sub= "") -> eu_elections

#b
#visualize share of votes of largest parties ("CDU", "CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", “FDP")
#one plot and facet_grid
eu_elections %>%
  group_by(Gruppenname, Gebietsname) %>%
  summarise(sumProzent = sum(Prozent)) -> share_data

gbar = ggplot(share_data, aes(x=Gebietsname, y = Prozent, fill= Gruppenname))
gbar = gbar + geom_bar(position = "fill")
gbar = gbar + scale_fill_brewer(palette="Paired")
gbar

gfacet = ggplot(share_data, aes(x = Gruppenname, y = Prozent)) +
  geom_col() +
  facet_wrap(share_data$Gebietsname)

#c
#show distribution parties performance across all states
gline = ggplot(share_data, aes(x = Gebietsname, y = Prozent)) +
  geom_line()

#d
library(leaflet)
#Aggregate the data frame to find the winners of each state
eu_elections %>%
  group_by(Gebietsname) %>%
  filter(Anzahl = max(Anzahl)) -> agg_data

#Load the (level 1) SpatialPolygonsDataFrames (sp) for Germany
germany <- getData("GADM", country="Germany", level=1)

#Join both data frames to map bounding boxes and winners
left_join(agg_data, germany) -> joined_data

#Define a color palette using the colorFactor function.
pal <- colorFactor(palette = c("black", "blue", "red", "green", "yellow", "pink", "turquoise"),
                   domain = "CDU", "CSU", "SPD", "DIE GRUENEN", "FDP", "DIE LINKE", "AFD")

#create leaflet map
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = germany,
              fillColor= ~pal())

#e
#Wahlkreis-Ebene mit Opacity mit Share of Votes der winning Party

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.35, 39.7,
          zoom = 4) %>%
  addPolygons(data = germany,
              fillColor= ~pal(),
              fillOpacity = 0.4,
              weight = 2)

