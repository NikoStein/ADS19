# Problem Set 1
# Question 1

#a)
createAd <- function(vehicleData){

#Datensatz in dataframe speichern
  df <- mtcars
#Spalte Name hinzufügen
 df$names = row.names(df)

#Erste Zeile anzeigen
 df[1, ]

 # $ für Spalten


}

#b)
createFormattedAd <- function(vehicleData){
  #Zugriff auf gesuchte Zeilen
  name <- df[1, ]$names
  cylinder <- df[1, ]$cyl
  fuelefficiency <- df[1, ]$mpg
  horsepower <- df[1, ]$hp
  qsec <- df[1, ]$qsec
  str(df)

  paste(name, cylinder, horsepower, fuelefficiency, qsec)

  #Sterne replizieren in einem String ohne Leerzeichen dazwischen

  x <- paste(rep('*', 25), collapse = '')



  #Gesuchte Attribute mit Tab und Sternchen

  cat(paste0(x ,'\n', '* ', name, '*', '\n', '* ', 'Cylinders: ', cylinder, '* ', '\n', '* ', 'Horsepower: ', horsepower, '* ', '\n','* ', 'Fuel Efficiency: ', fuelefficiency,'mpg', '*', '\n','* ', '1/4 mile time: ', qsec, '*', '\n', x))


}

#c)
createFormattedAdWithComparison <- function(vehicleData){
  #Verteilungen der Horsepower für Fahrzeug
  quantile(df$hp)

  selectedcar <- df[2, ]
  perc <- ecdf(df[, 'hp'])(selectedcar['hp'])
  (1-perc)*100
  if (perc == 1)(
    top_string = paste(value, ' %')
  )
  if (1-perc > 0.1){
    value = floor((1-perc)*100)
    top_string = paste(value, ' %')
  }
  if (1-perc <= 0.1){
    value = floor((1-perc)*100)
    top_string = paste(value, ' %')
  }
  else {
    top_string = ''
  }
}

#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
  #replace: mit oder ohne zurücklegen (False = ohne)
  #Zufällig n(=10) Ads auswählen
  sample(nrow(df), 10, replace = FALSE)
  #Daten zu den ausgewählten Zahlen/Ads ausgeben
  df[sample(nrow(df)), 10, replace = FALSE]
}

#e)
#Einlesen der Dateien
prices <- read.csv2("/Users/.../ADS19/Problem Sets/01/Data/carPrices")
mileages <- read.csv2("/Users/.../Downloads/ADS19-master/Problem Sets/01/Data/carMileage")

#Zusammenführen beider Datein zu mtcars
new_df <- merge(prices, mileages)
complete_df <- merge(new_df, df)

#price und mile zu Ad hinzufügen
createFormattedAd(df)
 df <- cbind.data.frame(df, 'prices' = complete_df$prices)


