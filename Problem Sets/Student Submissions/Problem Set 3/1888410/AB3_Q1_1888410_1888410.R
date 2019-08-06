# Problem Set 3
# Question 1

library(tidyverse)
library(grid)
library(gridExtra)

# 1. The ninth election to the European Parliament in Germany was held on 26 May 2019, electing members of the national
# Germany constituency to the European Parliament. Your task is to visualize the results of the election which can be downloaded
# here: https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv

  # a.)
  # Load the data directly from the link using the read_csv2 function. Note that you have to skip the first 9 lines to receive the data frame correctly. Subsequently, filter the
  # data set to data on a state (“Bundesland”) and party level. Note that you have to handle Unicode formatting.
  # (Hint: the final data set will have 640 rows and 18 columns)


data <- read_csv2("https://www.bundeswahlleiter.de/dam/jcr/5441f564-1f29-4971-9ae2-b8860c1724d1/ew19_kerg2.csv", skip = 9)

data %>%
  filter(Gebietsart == 'Land', Gruppenart == 'Partei') -> df

glimpse(df)
str(df)

  # b.)
  # First, visualize the share of votes the largest parties received in each state
  # ("CDU","CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", “FDP”). Create one figure aggregating all
  # the information in one plot (such as Figure 1) and one figure using facet_wrap (Figure 2).

  # plot Figure 1
  df %>%
    filter(Gruppenname %in% c("CDU","CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", "FDP")) %>%
    ggplot(aes(x = Gebietsname, y = Prozent, fill = Gruppenname)) + geom_col() + #scale_fill_brewer(palette = "Set1") + falls man eine vorgefertigte Palette verwenden will
    scale_fill_manual(limits = c("AfD", "CDU", "CSU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), values=c("#0489DB", "#000000", "#0099d5", "#800022","#FFEF00", "#1AA037", "#E3000F")) +
    labs(x = "", y = "Percentage", fill = "Parties") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> col_blot

  col_blot

  # plot Figure 2
  df %>%
    filter(Gruppenname %in% c("CDU","CSU", "SPD", "AfD", "DIE LINKE", "GRÜNE", "FDP")) %>%
    ggplot(aes(x = Gruppenname, y = Prozent)) + geom_col() + facet_wrap(~Gebietsname) +
    labs(x = "", y = "Percentage") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> facet_plot

  facet_plot

  # Zusammen in einem plot (Figure 1 + 2)
  grid.arrange(col_blot, facet_plot, ncol = 2, nrow = 1,
               top = textGrob("Share of votes the largest parties received in each state in % (European Parliament election 2019)",gp=gpar(fontsize=20,font=3)))




  # c.)
  # Obviously, a party’s performance largely depends on the state. Use a suited type of
  #visualization to show the distribution of each of the 5 parties’ performance across all states.

  df %>%
    filter(Gruppenname %in% c("CDU", "SPD", "AfD", "DIE LINKE", "GRÜNE", "FDP")) %>%
    ggplot(aes(x = Gruppenname, y = Prozent, fill = Gruppenname)) + geom_boxplot() +
    scale_fill_manual(limits = c("AfD", "CDU", "DIE LINKE", "FDP", "GRÜNE", "SPD"), values=c("#0489DB", "#000000", "#800022","#FFEF00", "#1AA037", "#E3000F")) +
    #xlab(label = "") +
    #ylab(label = 'Stimmen in %') +
    labs(x = "", y = "Stimmen in %", title = "", fill='Parteien') + #weitere Beschriftungen mit labs moeglich: subtitle = "", caption = ""
    #scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_text(size=14, face="bold"),
          legend.title = element_text(face="bold")) -> box_plot

  box_plot



  # d.)
  # Your next task is to leverage the leaflet package in order to visualize the election
  #winners in each state. Therefore, you have to:

    library(leaflet)
    library(raster)
    #library(sp)

    # i.) Aggregate the data frame to find the winners of each state

      df %>%
        dplyr::select(Gebietsname, Gruppenname, Anzahl, Prozent) %>% # Probleme mit dem select statement und dplyr package
        group_by(Gebietsname) %>%
        filter(Prozent == max(Prozent)) %>%
        rename(Land = Gebietsname) %>%
        arrange(Land)-> winners #alphabetisch aufsteigend nach Laender sortieren damit spaeter richtig "gejoint" wird



    # ii.) Load the (level 1) SpatialPolygonsDataFrames (sp) for Germany

      germany <- getData("GADM", country="DEU", level=1)



    # iii.) Join both data frames to map bounding boxes and winners

      germany$Gewinner = winners$Gruppenname
      germany$Stimmenanzahl = winners$Anzahl
      germany$Prozent = winners$Prozent



    # iv.) Define a color palette using the colorFactor function.

      #create a color palette to fill the polygons
      pal <- colorFactor(palette = c("#000000", "gray30", "#1AA037", "#0489DB", "#E3000F"),
                          levels = c("CDU", "CSU", "GRÜNE", "AfD", "SPD"))




    # v.) Create the map using leaflet

      #create a pop up (onClick)
      polygon_popup <- paste0("<strong>Name: </strong>", germany$NAME_1, "<br>",
                              "<strong>Partei: </strong>", germany$Gewinner, "<br>",
                              "<strong>Anteil: </strong>", round(germany$Prozent,2),"%")


      #create leaflet map
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = germany,
                    fillOpacity = 0.4,
                    fillColor= ~pal(Gewinner),
                    weight = 2,
                    color = "white",
                    popup = polygon_popup)





  # e.)
  # Repeat the steps above to create a similar map on a constituency level. Additionally,
  # map the opacity of the constituencies to the share of votes of the winning party. The
  # final result should look like the following:

  # Get data ###
  data %>%
      filter(Gebietsart == 'Kreis', Gruppenart == 'Partei') -> df_kreis

  df_kreis %>%
    dplyr::select(Gebietsnummer, Gebietsname, UegGebietsnummer, Gruppenname, Anzahl, Prozent) %>%
    group_by(UegGebietsnummer, Gebietsname) %>%
    filter(Prozent == max(Prozent)) %>%
    rename(Land = Gebietsname) -> winners_kreis

  # bekomme die Daten fuer die einzelnen Kreise in einem SpartialPolygonsDataFrame -> Zugriff auf data frame mit @ -> z.B. kreisdata
  kreis <- getData("GADM", country="DEU", level=2)


  #Joinen: hinzufuegen der Wahlsieger + Prozentanzahl mittels Join zum PolygonsDataFrame ueber die Gebietsnummer aka Postleitzahl
  kreis@data %>%
     left_join(winners_kreis, by = c("CC_2" = "Gebietsnummer")) -> kreis@data



  #Fehlende/Falsche Werte bearbeiten: ###
  #Neben dem Bodensee, fehlen die Werte bei kreis@data fuer Goettingen und Osterode am Harz.
  #aus winners_kreis bekommt man folgende Werte:

  # - Goettingen CDU 25.60708 Prozent -> Fehler in der Postleitzahl kreis@data: 03152 und bei winners_kreis: 03159.
  #   PLZ 03152 gibt es im winners_kreis Datensatz gar nicht, daher muss es sich um den richtigen Goettinger Wahlkreis handeln
  #   Goettingen ausbessern
  kreis@data$Gruppenname[211] <- "CDU"
  kreis@data$Prozent[211] <- 25.60708

  # - Osterode am Harz hat 03156 weder Osterode am Harz noch die PLZ sind im winners_kreis Datensatz zu finden.
  #   Im Internet nachgeschaut dort hat die CDU mit 29,40 Prozent die meisten Stimmen geholt.
  #   Osterode am Harz ausbessern
  kreis@data$Gruppenname[229] <- "CDU"
  kreis@data$Prozent[229] <- 29.40

  # Anmerkung:
  # Bei kreis@data unter Name_2 gibt es ein Problem mit dem Umlaut UE er wird als �  n dieser Spalte angezeigt .
  # Dies betrift die 5 Staedte -> Fuerth, Muenchen, Osnabrueck, Wuerzburg jeweils nur in Kombination mit (Kreisfreie Stadt)
  # Bei den labels wurde daher von der Spalte Name_2 auf Land gewechselt und somit muss die Codierung nicht unbedingt geaendert werden.



  # create plot###
  #create a color palette to fill the polygons (hier nur zusaetzliche NA Werte wie Bodensee (welches kein Wahlkreis ist) mit einer Farbe abfangen))
  pal <- colorFactor(palette = c("#000000", "gray30", "#1AA037", "#0489DB", "#E3000F"),
                     levels = c("CDU", "CSU", "GRÜNE", "AfD", "SPD"), na.color = "white")

  #pal <- colorQuantile("Greens", NULL, n = 5) #eine palette mit verschiedenen Farbintensivitaeten



  #create a pop up (onClick)
  polygon_popup <- paste0("<strong>Name: </strong>", kreis$Land, "<br>",
                          "<strong>Partei: </strong>", kreis$Gruppenname, "<br>",
                          "<strong>Anteil: </strong>", round(kreis$Prozent,2),"%")


  library(htmltools) # wird benoetigt um die labels schon beim hovern zu sehen und nicht erst beim klicken

  #create leaflet map
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = kreis,
                fillOpacity = 0.6,
                fillColor= ~pal(Gruppenname),
                weight = 2,
                color = "white",
                label = lapply(polygon_popup, HTML), #labels beim hovern statt click + alles unter highlight gehoert auch dazu
                highlight = highlightOptions(
                  weight = 5,
                  color = "#FF9933",
                  fillOpacity = 0.8,
                  bringToFront = TRUE))






