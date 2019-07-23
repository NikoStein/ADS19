# Problem Set 1
# Question 1


library(tidyverse)
vehicleData <- mtcars[1, ]
vehiclesData <- mtcars

#a)
createAd <- function(vehicleData) {
  print(vehicleData)
}

#######################################################

#b)
createFormattedAd <- function(vehicleData){
  # Die erste Zeile besteht einfach aus 26 Mal dem Symbol "*".
  line1 <- rep(x = "*", 26)

  # Bei der zweiten Zeile müssen wir gewährleisten, dass in Abhängigkeit der bereits
  # verwendeten Zeichen die korrekte Anzahl an Leerzeichen hinzufügen, damit sich das
  # rechte "*" an der korrekten Stelle befindet.
  # Hierbei ergibt sich numberBlanks folgendermaßen: Es müssen am Ende 26 Zeichen vorliegen.
  # Deshalb muss sum(nchar(line2)) + numberBlanks + 1 = 26 gelten. Die 1 bedingt sich durch
  # das rechte "*".
  line2 <- c("* ", rownames(vehicleData))
  numberBlanks <- 26 - sum(nchar(line2)) - 1
  line2[3 : (numberBlanks + 2)] <- " "
  line2[numberBlanks + 3] <- "*"

  # In den restlichen Zeilen müssen wir die Länge des ausgegebenen Wertes berücksichtigen.
  line3 <- c("* Horsepower: ", as.character(vehicleData["hp"]))
  numberBlanks <- 26 - sum(nchar(line3)) - 1
  line3[3 : (numberBlanks + 2)] <- " "
  line3[numberBlanks + 3] <- "*"

  line4 <- c("* Cylinders: ", as.character(vehicleData["cyl"]))
  numberBlanks <- 26 - sum(nchar(line4)) - 1
  line4[3 : (numberBlanks + 2)] <- " "
  line4[numberBlanks + 3] <- "*"

  line5 <- c("* Fuel Efficiency: ", as.character(round(vehicleData["mpg"])), "mpg")
  numberBlanks <- 26 - sum(nchar(line5)) - 1
  line5[4 : (numberBlanks + 3)] <- " "
  line5[numberBlanks + 4] <- "*"

  line6 <- c("* 1/4 mile time: ", as.character(floor(vehicleData["qsec"])), "sec")
  numberBlanks <- 26 - sum(nchar(line6)) - 1
  line6[4 : (numberBlanks + 3)] <- " "
  line6[numberBlanks + 4] <- "*"

  cat(line1, "\n",
      line2, "\n",
      line3, "\n",
      line4, "\n",
      line5, "\n",
      line6, "\n",
      line1, sep = "")
}
createFormattedAd(mtcars[6, ])

##################################################

# c)
createFormattedAdWithComparison <- function(vehicleData){

  # Wir brauchen eine größere Breite der Werbeanzeige, um das zusätzliche Symbol "Top %x.
  line1 <- rep(x = "*", 36)

  line2 <- c("* ", rownames(vehicleData))
  numberBlanks <- 36 - sum(nchar(line2)) - 1
  line2[3 : (numberBlanks + 2)] <- " "
  line2[numberBlanks + 3] <- "*"

  # In den restlichen Zeilen müssen wir die Länge des ausgegebenen Wertes berücksichtigen.
  line3 <- c("* Horsepower: ", as.character(vehicleData["hp"]))

  # Liegt der betrachtete Wert in den Top 10% des Datensatzes, so erhält er ein extra
  # Label mit "Top x%", wobei es sich bei x um den Wert der empirischen kumulatitiven
  # Verteilungsfunktion (ecdf) an der Stelle vehicleData["hp"] handelt.
  if(vehicleData[1, "hp"] >= quantile(x = mtcars[, "hp"], probs = 0.9) ) {
    label <- vehicleData["hp"] %>%
      ecdf(x = mtcars[, "hp"])() %>%
      (function(x) {(1 - x) * 100}) %>%
      round(digits = 0) %>%
      as.character()

    line3[length(line3) + 1] <- paste0(" Top ", label, "%")
  }

  numberBlanks <- 36 - sum(nchar(line3)) - 1
  line3[(length(line3) + 1): (numberBlanks + length(line3))] <- " "
  line3[length(line3) + 1] <- "*"

  #############

  line4 <- c("* Cylinders: ", as.character(vehicleData["cyl"]))
  numberBlanks <- 36 - sum(nchar(line4)) - 1
  line4[(length(line4) + 1): (numberBlanks + length(line4))] <- " "
  line4[length(line4) + 1] <- "*"

  #############

  line5 <- c("* Fuel Efficiency: ", as.character(round(vehicleData["mpg"])), "mpg")

  if(vehicleData[1, "mpg"] >= quantile(x = mtcars[, "mpg"], probs = 0.9) ) {
    vehicleData["mpg"] %>%
      ecdf(x = mtcars[, "mpg"])() %>%
      (function(x) {(1 - x) * 100}) %>%
      ceiling() %>%
      as.character() -> label

    line5[length(line5) + 1] <- paste0(" Top ", label, "%")
  }

  numberBlanks <- 36 - sum(nchar(line5)) - 1
  line5[(length(line5) + 1): (numberBlanks + length(line5))] <- " "
  line5[length(line5) + 1] <- "*"
  line5

  ############

  line6 <- c("* 1/4 mile time: ", as.character(floor(vehicleData["qsec"])), "sec")

  if(vehicleData[1, "qsec"] >= quantile(x = mtcars[, "qsec"], probs = 0.9) ) {
    vehicleData["qsec"] %>%
      ecdf(x = mtcars[, "qsec"])() %>%
      (function(x) {(1 - x) * 100}) %>%
      ceiling() %>%
      as.character() -> label

    line6[length(line6) + 1] <- paste0(" Top ", label, "%")
  }

  numberBlanks <- 36 - sum(nchar(line6)) - 1
  line6[(length(line6) + 1): (numberBlanks + length(line6))] <- " "
  line6[length(line6) + 1] <- "*"

  #####################

  if(any(colnames(vehicleData) == "Mileage")) {
  line7 <- c("* Mileage: ", as.character(floor(vehicleData["Mileage"])))

  numberBlanks <- 36 - sum(nchar(line7)) - 1
  line7[(length(line7) + 1): (numberBlanks + length(line7))] <- " "
  line7[length(line7) + 1] <- "*"

  } else {
    line7 <- NULL
  }

  #####################

  if(any(colnames(vehicleData) == "Price")) {
    line8 <- c("* Price: ", as.character(floor(vehicleData["Price"])))

    numberBlanks <- 36 - sum(nchar(line8)) - 1
    line8[(length(line8) + 1): (numberBlanks + length(line8))] <- " "
    line8[length(line8) + 1] <- "*"

  } else {
    line8 <- NULL
  }

  ###################

  if(is.null(c(line7, line8))) {
    cat(line1, "\n",
        line2, "\n",
        line3, "\n",
        line4, "\n",
        line5, "\n",
        line6, "\n",
        line1, sep = "")

  } else {
    cat(line1, "\n",
        line2, "\n",
        line3, "\n",
        line4, "\n",
        line5, "\n",
        line6, "\n",
        line7, "\n",
        line8, "\n",
        line1, sep = "")
  }

}

# BEISPIEL
createFormattedAdWithComparison(mtcars[7, ])

#####################################################

#d)
createFormattedAdsWithComparisons <- function(vehiclesData, n){
  auswahl <- sample(x = seq_len(nrow(vehiclesData)), size = n, replace = FALSE)

  # Die cat-Funktion muss nochmals angewendet werden, um nach jeder Ausgabe eine
  # neue Zeile zu beginnen.
  for(i in auswahl) {
    cat(createFormattedAdWithComparison(vehiclesData[i, ]), "\n")
  }
}

# BEISPIEL
createFormattedAdsWithComparisons(mtcars, 4)

####################################################

#e)

# Einlesen der Daten
carMileage <- read.csv(file = "Data/carMileage.csv", sep = ",", dec = ".")
carPrices <- read.csv(file = "Data/carPrices.csv", sep = ",", dec = ".")
carData <- mtcars

head(carMileage)
head(carPrices)
head(carData)

# carData besitzt eine andere Struktur als carMileage und carPrices
carDataModified <- cbind(rownames(carData), carData)
rownames(carDataModified) <- 1:nrow(carDataModified)
colnames(carDataModified)[1] <- "Type"
head(carDataModified)

# "by" gibt hierbei an, welche Spalte zum mergen verwendet werden soll. Man kann hierbei
# durch by.x und by.y unterschiedliche Spalten für den Datensatz x und den Datensatz y
# angeben.
carDataMerged <- merge(x = carDataModified, y = carMileage, by = "Type")
carDataMerged <- merge(x = carDataMerged, y = carPrices, by = "Type")
head(carDataMerged)

# Leider müssen wir carDataMerged wieder auf die Ursprungsstruktur von mtcars zurückbringen,
# da unsere Funktionen auf diese ausgelegt sind.
rownames(carDataMerged) <- carDataMerged[, "Type"]
carDataMerged <- carDataMerged[, -1]
head(carDataMerged)

# Die Ad-Funktionen berücksichtigen auch Price und Mileage. BEISPIEL:
carDataMerged %>%
  createFormattedAdsWithComparisons(n = 4)

# Meine Ad-Funktionen geben nichts aus, was ich sinnvoll in einem Vektor speichern könnte.
