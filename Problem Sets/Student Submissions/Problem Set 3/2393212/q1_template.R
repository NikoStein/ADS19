# Problem Set 3
# Question 1



library(tidyverse)
library(ggplot2)
library(leaflet)
library(sp)
library(raster)

# a)
getDataSetFilteredOnStateAndPartyLevel <- function() {
  url <- "https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv"
  electionResultCSV <- read_csv2(url, skip = 9)

  electionResultCSV %>%
    filter(Gebietsart == "Land") %>%
    filter(Gruppenart == "Partei") -> electionResultCSV

  electionResultCSV
}


# b)

# data set preparation
getDataSetFilteredOnStateAndPartyLevel() %>%
  select(Gebietsname, Gruppenname, Prozent) %>%
  filter(Gruppenname %in% c("CDU", "CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", "FDP")) -> dataSetExerciseB

stackedBarPlot <- ggplot(data=dataSetExerciseB, aes(x=Gebietsname, y=Prozent, fill=Gruppenname)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))

facetWrap <- ggplot(data=dataSetExerciseB, aes(x=Gruppenname, y=Prozent)) +
  geom_bar(stat="identity") +
  facet_wrap(vars(Gebietsname)) +
  theme(axis.text.x = element_text(angle = 90))


# c)
facetWrapPartys <- ggplot(data=dataSetExerciseB, aes(x=Gebietsname, y=Prozent)) +
  geom_bar(stat="identity") +
  facet_wrap(vars(Gruppenname)) +
  coord_flip()


# d)

# d) i) Aggregate the data frame to find the winners of each state
getWinnerOfEachState <- function() {
  getDataSetFilteredOnStateAndPartyLevel() %>%
    dplyr::select(Gebietsname, Gruppenname, Prozent) %>%
    group_by(Gebietsname) %>%
    filter(Prozent == max(Prozent)) -> winnersOfEachState
  winnersOfEachState
}

# d) ii) Load the (level 1) SpatialPolygonsDataFrames (sp) for Germany
germany <- getData("GADM", country="DEU", level=1)

# d) iii) Join both data frames to map bounding boxes and winners

mergedDataSets <- merge(germany, getWinnerOfEachState(), by.x = "NAME_1", by.y = "Gebietsname")

# d) iv) Define a color palette using the colorFactor function.
partyColors <- c("#121212", "#b9b9b9", "#d71f1f", "#1621c8", "#bd3074", "#78bc1b", "#ffcd0c")
factpal <- colorFactor(partyColors, levels = c("CDU", "CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", "FDP"))

# d) v) Create the map using leaflet
leaflet() %>%
  addTiles() %>%
  setView(lng = 10.451526, lat = 51.165691, zoom = 5) %>%
  addPolygons(data = mergedDataSets,
              fillColor= ~factpal(Gruppenname),
              fillOpacity = 0.6,
              weight = 1,
              color = "black")

# e)
# Repeat the steps above to create a similar map on a constituency level.
# Additionally, map the opacity of the constituencies to the share of votes of the winning party.

getDataSetFilteredOnConstituencyAndPartyLevel <- function() {
  url <- "https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv"
  constituencyResultCSV <- read_csv2(url, skip = 9)

  constituencyResultCSV %>%
    filter(Gebietsart == "Kreis") %>%
    filter(Gruppenart == "Partei") -> constituencyResultCSV

  constituencyResultCSV
}

getWinnerOfEachConstituency <- function() {
  getDataSetFilteredOnConstituencyAndPartyLevel() %>%
    dplyr::select(Gebietsname, Gruppenname, Prozent) %>%
    group_by(Gebietsname) %>%
    filter(Prozent == max(Prozent)) -> winnersOfEachConstituency
  winnersOfEachConstituency
}

germanyConstituencyLevel <- getData("GADM", country="DEU", level=2)

mergedDataSetsConstituencyLevel <- merge(germanyConstituencyLevel, getWinnerOfEachConstituency(), by.x = "NAME_2", by.y = "Gebietsname")

# opacity is missing
leaflet() %>%
  addTiles() %>%
  setView(lng = 10.451526, lat = 51.165691, zoom = 5) %>%
  addPolygons(data = mergedDataSetsConstituencyLevel,
              fillColor= ~factpal(Gruppenname),
              fillOpacity = 0.6,
              weight = 1,
              color = "black")






