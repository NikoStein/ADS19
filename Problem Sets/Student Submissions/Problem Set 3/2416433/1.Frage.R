# Problem Set 3
# Question 1


library(tidyverse)
library(leaflet)
library(raster)

# a)
df <- read_csv2(url("https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv"),
                skip = 9, col_names = T)

df %>%
  filter(Gebietsart == "Land" & Gruppenart == "Partei") -> df_final

# b)
df_final %>%
  filter(Gruppenname == "CDU" | Gruppenname == "CSU" | Gruppenname == "SPD" | Gruppenname == "AfD"
         | Gruppenname == "DIE LINKE" | Gruppenname == "GRÜNE" | Gruppenname == "FDP" ) %>%
  group_by(Gebietsname, Gruppenname) %>%
  summarise(Prozent = sum(Prozent)) -> df_graphics

# --> gruppieren UND alle Variablen mitnehmen
#df_final %>%
#  filter(Gruppenname == "CDU" | Gruppenname == "CSU" | Gruppenname == "SPD" | Gruppenname == "AfD"
#         | Gruppenname == "DIE LINKE" | Gruppenname == "GRÜNE" | Gruppenname == "FDP" ) %>%
#  group_by(Gebietsname, Gruppenname) %>%
#  filter(Prozent == sum(Prozent)) -> figure_aggregating_test


df_graphics %>%
  ggplot(aes(y = Prozent, x = Gebietsname, fill = Gruppenname))+
  geom_bar(stat = "identity", position = "stack")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))

df_graphics %>%
  ggplot(mapping = aes(x = Gruppenname, y = Prozent))+
  geom_bar(stat = "identity") +
  facet_wrap(~ Gebietsname, nrow = 4)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))

# c)
df_graphics %>%
  ggplot(.,mapping = aes(y = Prozent, x = Gebietsname))+
  geom_bar(stat = "identity")+
  coord_flip()+
  facet_wrap(~ Gruppenname, nrow = 3)

# d)
# i)
#df_graphics %>%
#  group_by(Gebietsname) %>%
#  top_n(1, Prozent) -> eu_elections_winner_states

df_graphics %>%
  group_by(Gebietsname) %>%
  filter(Prozent == max(Prozent)) -> eu_elections_winner_states

# ii)
germany_states <- getData("GADM", country="Germany", level=1)

# iii)
merge(germany_states, eu_elections_winner_states, by.x = "NAME_1", by.y = "Gebietsname") -> germany_states_merged_winners

# iv)


pal <- colorFactor(palette = c("grey", "blue", "black", "green", "red"),
              levels = c("CSU", "AfD", "CDU", "GRÜNE", "SPD"))

polygon_popup <- paste0("<strong>Name: </strong>", germany_states_merged_winners$NAME_1, "<br>",

                        "<strong>Wahlsieger: </strong>", germany_states_merged_winners$Gruppenname, "<br>",

                        "<strong>Prozent: </strong>", round(germany_states_merged_winners$Prozent,2))

#create leaflet map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = germany_states_merged_winners,
              fillColor= ~pal(Gruppenname),
              fillOpacity = 0.4,
              weight = 2,
              color = "Black",
              popup = polygon_popup)

# e)

df %>%
  filter(Gebietsart == "Kreis" & Gruppenart == "Partei") -> df_kreis

df_kreis %>%
  filter(Gruppenname == "CDU" | Gruppenname == "CSU" | Gruppenname == "SPD" | Gruppenname == "AfD"
         | Gruppenname == "DIE LINKE" | Gruppenname == "GRÜNE" | Gruppenname == "FDP") %>%
  group_by(Gebietsname, Gruppenname) %>%
  summarise(Prozent = sum(Prozent)) -> df_kreis_edit

df_kreis_edit %>%
  group_by(Gebietsname) %>%
  filter(Prozent == max(Prozent)) -> df_kreis_winner

germany_kreise <- getData("GADM", country="Germany", level=2)


merge(germany_kreise, df_kreis_winner, by.x = "NAME_2", by.y = "Gebietsname") -> germany_kreise_merged_winners

polygon_popup_kreis <- paste0("<strong>Name: </strong>", germany_kreise_merged_winners$NAME_2, "<br>",

                        "<strong>Wahlsieger: </strong>", germany_kreise_merged_winners$Gruppenname, "<br>",

                        "<strong>Prozent: </strong>", round(germany_kreise_merged_winners$Prozent,2))


leaflet() %>%
  addTiles() %>%
  addPolygons(data = germany_kreise_merged_winners,
              fillColor= ~pal(Gruppenname),
              fillOpacity = 0.4,
              weight = 2,
              color = "Black",
              popup = polygon_popup_kreis)


mypal <- colorQuantile(palette = "YlOrRd", domain = germany_kreise_merged_winners$Prozent, n = 9, reverse = TRUE)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = germany_kreise_merged_winners,
              fillColor= ~mypal(germany_kreise_merged_winners$Prozent),
              fillOpacity = 0.4,
              weight = 2,
              color = "Black",
              popup = polygon_popup_kreis) %>%
  addLegend(position = "bottomright", pal = mypal, values = germany_kreise_merged_winners$Prozent,
            title = "Wahlergebnisse",
            opacity = 1)


