# Problem Set 1
# Question 1


library(tidyverse)

#a) Create a function which creates unformatted output of a single row data.frame
#   equivalent to the internal data set mtcars.

createAd <- function(vehicleData){
  df <- mtcars

  # neue Spalte names in df
  df$names = row.names(df)

  name <- df[vehicleData,]$names
  hp <- df[vehicleData,]$hp
  cyl <- df[vehicleData,]$cyl
  mpg <- df[vehicleData,]$mpg
  qsec <- df[vehicleData,]$qsec

  return(df[vehicleData,])
}

# Beispiel
createAd(32)

#b) Using sprintf or paste, length and repeat format your output analogue to the above.
createFormattedAd <- function(vehicleData){
  df <- mtcars

  df$names = row.names(df)

  name <- df[vehicleData,]$names
  hp <- df[vehicleData,]$hp
  cyl <- df[vehicleData,]$cyl
  mpg <- df[vehicleData,]$mpg
  qsec <- df[vehicleData,]$qsec

  # cat() wird benötigt, um Umbruch ('\n') zu verarbeiten
  cat(paste('', paste(rep('*', 30), collapse = ''), '\n',
            '*', name, paste(rep(' ', 30 - nchar(name) - 5), collapse = ''), '*', '\n',
            '* Horsepower:', hp, paste(rep(' ', 30 - nchar('Horsepower:') - nchar(hp) - 5 - 1), collapse = ''), '*', '\n',
            '* Cylinders:', cyl, paste(rep(' ', 30 - nchar('Cylinders:') - nchar(cyl) - 5 - 1), collapse = ''), '*', '\n',
            '* Fuel efficiency:', mpg, paste(rep(' ', 30 - nchar('Fuel efficiency:') - nchar(mpg) - 5 - 1), collapse = ''), '*', '\n',
            '* 1/4 mile time:', qsec, paste(rep(' ', 30 - nchar('1/4 mile time:') - nchar(qsec) - 5 - 1), collapse = ''), '*', '\n',
            paste(rep('*', 30), collapse = '')), '\n')
}

# lapply(df, function(x) max(nchar(x)))
# Die maximale Länge eines Strings ist 19 und befindet sich in der Spalte "names"
# Die maximale Länge von qsec ist 5

# Beispiel
createFormattedAd(20)

#c) To improve marketing chances, the vendor wants to include a relative ranking if the
#   fuel efficiency rating, horsepower or quarter mile time is in the top 10% of the data set.
#   In these cases, include “(Top x%)” behind the corresponding entry.
createFormattedAdWithComparisons <- function(vehicleData) {

  df <- mtcars

  df$names = row.names(df)

  name <- df[vehicleData,]$names
  hp <- df[vehicleData,]$hp
  cyl <- df[vehicleData,]$cyl
  mpg <- df[vehicleData,]$mpg
  qsec <- df[vehicleData,]$qsec
  # mehr mpg ist besser
  quantile_mpg <- quantile(df$mpg, 0.9)
  # mehr hp ist besser
  quantile_hp <- quantile(df$hp, 0.9)
  # weniger qsec ist besser
  quantile_qsec <- quantile(df$qsec, 0.1)


  selectedCar <- df[vehicleData,]

  # ecdf(df$hp): kumulierte Verteilungsfunktion von hp

  top_mpg <- ecdf(df[,'mpg'])(selectedCar['mpg']) # Ergebnis: Top ... %
  perc_mpg <- ((1-top_mpg)*100)

  top_hp <- ecdf(df[,'hp'])(selectedCar['hp'])
  perc_hp <- ((1-top_hp)*100)

  top_qsec <- ecdf(df[,'qsec'])(selectedCar['qsec'])
  perc_qsec <- ((top_qsec)*100)


  if (hp >= quantile_hp) {
    top_string_hp = paste('(Top ', perc_hp, '%)')
  }
  if (hp < quantile_hp) {
    top_string_hp = paste('')
  }



  if (mpg >= quantile_mpg) {
    top_string_mpg = paste('(Top ', perc_mpg, '%)')
  }
  if (mpg < quantile_mpg) {
    top_string_mpg = paste('')
  }



  if (qsec > quantile_qsec) {
    top_string_qsec = paste('')
  }
  if (qsec <= quantile_qsec) {
    top_string_qsec = paste('(Top ', perc_qsec, '%)')
  }

  space = 42

  hp_string <- paste('* Horsepower:', hp, top_string_hp)
  mpg_string <- paste('* Fuel efficiency:', mpg, top_string_mpg)
  qsec_string <- paste('* 1/4 mile time:', qsec, top_string_qsec)

  cat(paste('', paste(rep('*', space), collapse = ''), '\n',
            '*', name, paste(rep(' ', space - nchar(name) - 5), collapse = ''), '*', '\n',
            hp_string, paste(rep(' ', space - nchar(hp_string)- 3), collapse = ''), '*', '\n',
            '* Cylinders:', cyl, paste(rep(' ', space - nchar('Cylinders:') - nchar(cyl) - 6), collapse = ''), '*', '\n',
            mpg_string, paste(rep(' ', space - nchar(mpg_string) - 3), collapse = ''), '*', '\n',
            qsec_string, paste(rep(' ', space - nchar(qsec_string) - 3), collapse = ''), '*', '\n',
            paste(rep('*', space), collapse = '')), '\n')

}

# Beispiel
createFormattedAdWithComparisons(29)

#d) To automate the campaign, expand your function to take a data.frame with multiple cars
#   plus an additional integer argument n which specifies the number of ads that should be created.
#   Then randomly sample n rows and create the ads for these vehicles. Watch out not to create the
#   same ad twice
createFormattedAdsWithComparisons <- function(n){
  df <- mtcars

  df$names = row.names(df)

  name <- df$names
  hp <- df$hp
  cyl <- df$cyl
  mpg <- df$mpg
  qsec <- df$qsec

  # mit sample() zufällige Stichprobe ausgeben
  # ncol würde die Spalten durchzählen
  df <- df[sample(nrow(df), n, replace = FALSE),]

  for (vehicleData in 1:nrow(df)) {

    name <- df[vehicleData,]$names
    hp <- df[vehicleData,]$hp
    cyl <- df[vehicleData,]$cyl
    mpg <- df[vehicleData,]$mpg
    qsec <- df[vehicleData,]$qsec

    # mehr ist besser
    quantile_mpg <- quantile(df$mpg, 0.9)
    # mehr ist besser
    quantile_hp <- quantile(df$hp, 0.9)
    # weniger ist besser
    quantile_qsec <- quantile(df$qsec, 0.1)


    selectedCar <- df[vehicleData,]


    top_mpg <- ecdf(df[,'mpg'])(selectedCar['mpg'])
    perc_mpg <- ((1-top_mpg)*100)

    top_hp <- ecdf(df[,'hp'])(selectedCar['hp'])
    perc_hp <- ((1-top_hp)*100)

    top_qsec <- ecdf(df[,'qsec'])(selectedCar['qsec'])
    perc_qsec <- ((top_qsec)*100)


    if (hp >= quantile_hp) {
      top_string_hp = paste('(Top ', perc_hp, '%)')
    }
    if (hp < quantile_hp) {
      top_string_hp = paste('')
    }



    if (mpg >= quantile_mpg) {
      top_string_mpg = paste('(Top ', perc_mpg, '%)')
    }
    if (mpg < quantile_mpg) {
      top_string_mpg = paste('')
    }



    if (qsec > quantile_qsec) {
      top_string_qsec = paste('')
    }
    if (qsec <= quantile_qsec) {
      top_string_qsec = paste('(Top ', perc_qsec, '%)')
    }



    space = 50

    hp_string <- paste('* Horsepower:', hp, top_string_hp)
    mpg_string <- paste('* Fuel efficiency:', mpg, top_string_mpg)
    qsec_string <- paste('* 1/4 mile time:', qsec, top_string_qsec)

    #for (n in 1:n) {

    cat(paste('', paste(rep('*', space), collapse = ''), '\n',
              '*', name, paste(rep(' ', space - nchar(name) - 5), collapse = ''), '*', '\n',
              hp_string, paste(rep(' ', space - nchar(hp_string)- 3), collapse = ''), '*', '\n',
              '* Cylinders:', cyl, paste(rep(' ', space - nchar('Cylinders:') - nchar(cyl) - 6), collapse = ''), '*', '\n',
              mpg_string, paste(rep(' ', space - nchar(mpg_string) - 3), collapse = ''), '*', '\n',
              qsec_string, paste(rep(' ', space - nchar(qsec_string) - 3), collapse = ''), '*', '\n',
              paste(rep('*', space), collapse = ''), '\n'))
  }
}

# Beispiel
createFormattedAdsWithComparisons(10)

#e) For the purpose of car sales, the data set is clearly missing two essential data points –
#   price and mileage. Combine the provided data files carMileage.csv and carPrices.csv with mtcars
#   and include unformatted price and mileage statements in your Ad function. Create a vector or list with all the ads of cars in this expanded mtcars.

prices <- read.csv2('Data/carPrices.csv')
mileages <- read.csv2('Data/carMileage.csv')

prices %>%
  rowwise() %>%
  mutate(Type = str_split(string = Type.Price, ',', simplify = TRUE)[1]) %>%
  mutate(Price = str_split(string = Type.Price, ',', simplify = TRUE)[2]) %>%
  select(-Type.Price) -> prices

mileages %>%
  rowwise() %>%
  mutate(Type = str_split(string = Type.Mileage, ',', simplify = TRUE)[1]) %>%
  mutate(Mileage = str_split(string = Type.Mileage, ',', simplify = TRUE)[2]) %>%
  select(-Type.Mileage) -> mileages

df <- mtcars
df$Type <- row.names(df)

# Achtung: Auf Spaltenbezeichnungen achten!
new_df <- merge(prices, mileages, by="Type")
new_df <- merge(df, new_df, by="Type")

createFormattedAdWithAdditionalData <- function(vehicleData) {


  name <- new_df[vehicleData,]$Type
  hp <- new_df[vehicleData,]$hp
  cyl <- new_df[vehicleData,]$cyl
  mpg <- new_df[vehicleData,]$mpg
  qsec <- new_df[vehicleData,]$qsec
  mileage <- new_df[vehicleData,]$Mileage
  price <- new_df[vehicleData,]$Price

  space = 30

  cat(paste('', paste(rep('*', space), collapse = ''), '\n',
            '*', name, paste(rep(' ', space - nchar(name) - 5), collapse = ''), '*', '\n',
            '* Horsepower:', hp, paste(rep(' ', space - nchar('Horsepower:') - nchar(hp) - 5 - 1), collapse = ''), '*', '\n',
            '* Cylinders:', cyl, paste(rep(' ', space - nchar('Cylinders:') - nchar(cyl) - 5 - 1), collapse = ''), '*', '\n',
            '* Fuel efficiency:', mpg, paste(rep(' ', space - nchar('Fuel efficiency:') - nchar(mpg) - 5 - 1), collapse = ''), '*', '\n',
            '* 1/4 mile time:', qsec, paste(rep(' ', space - nchar('1/4 mile time:') - nchar(qsec) - 5 - 1), collapse = ''), '*','\n',
            '* Mileage:', mileage, paste(rep(' ', space - nchar('Mileage:') - nchar(mileage) - 5 - 1), collapse = ''), '*', '\n',
            '* Price:', price, paste(rep(' ', space - nchar('Price:') - nchar(price) - 5 - 1), collapse = ''), '*', '\n',
            paste(rep('*', space), collapse = '')), '\n')
}

for (i in 1:nrow(new_df)) {
  createFormattedAdWithAdditionalData(i)
}


