# Problem Set 1
# Question 1


#a)
library(tidyverse)

x <- sample(1:32, 1)
df <- mtcars
df$names <- row.names(df)

createAd <- function(x){
  return(df[x, ])
}

createAd(x)

#b)
createFormattedAd <- function(x){
#  line1 <- sprintf("* %s",rownames_to_column(df)[x,1])
  line1 <- sprintf("* %s",df[x, ]$names)
  line2 <- sprintf("* Horsepower: %s",df[x, ]$hp)
  line3 <- sprintf("* Cylinders: %s", df[x, ]$cyl)
  line4 <- sprintf("* Fuel Efficiency: %s mpg", df[x, ]$mpg)
  line5 <- sprintf("* 1/4 mile time: %s sec",df[x, ]$qsec)
  lines <- c(line1, line2, line3, line4, line5)
  line6 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line6,lines,line6)
  return(tibble(lines_ins))
}
createFormattedAd(x)


#c)
createFormattedAdWithComparison <- function(x){
  seleted.car <- df[x, ]
# ----- hp ----
  perc <- ecdf(df[,'hp'])(seleted.car['hp'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.hp <- floor(perc.neu)
    top.string.hp <- sprintf("(Top %s%s)", top.10.hp, "%")
    line2 <- sprintf("* Horsepower: %s%s",df[x, ]$hp,top.string.hp)
  }
  else{
    line2 <- sprintf("* Horsepower: %s",df[x, ]$hp)
  }

# ----- mpg -----
  perc <- ecdf(df[,'mpg'])(seleted.car['mpg'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.mpg <- floor(perc.neu)
    top.string.mpg <- sprintf("(Top %s%s)", top.10.mpg, "%")
    line4 <- sprintf("* Fuel Efficiency: %s mpg %s", df[x, ]$mpg, top.string.mpg)
  }
  else{
    line4 <- sprintf("* Fuel Efficiency: %s mpg", df[x, ]$mpg)
  }

# ----- qsec -----
  perc <- ecdf(df[,'qsec'])(seleted.car['qsec'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.qsec <- floor(perc.neu)
    top.string.qsec <- sprintf("(Top %s%s)", top.10.qsec, "%")
    line5 <- sprintf("* 1/4 mile time: %s sec %s",df[x, ]$qsec, top.string.qsec)
  }
  else{
    line5 <- sprintf("* 1/4 mile time: %s sec",df[x, ]$qsec)
  }

  line1 <- sprintf("* %s",df[x, ]$names)
  line3 <- sprintf("* Cylinders: %s", df[x, ]$cyl)
  lines <- c(line1, line2, line3, line4, line5)
  line6 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line6,lines,line6)
  return(tibble(lines_ins))
}

createFormattedAdWithComparison(x)

#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
  number.ads <- n
  ads <- sample(1:32, number.ads)
  ads
  return(map(ads, createFormattedAdWithComparison))
}

createFormattedAdsWithComparisons(df,5)

#e)
# Your code
setwd("C:/Users/Christoph/Documents/Test_R/ADS19/Problem Sets/01/Data")
car.Prices <- read.csv('carPrices.csv',header = TRUE,",")
car.mileage <- read.csv('carMileage.csv',header = TRUE,",")
new.df <- merge(car.Prices, car.mileage)
df.final <- merge(df, new.df, by.x = 'names', by.y = 'Type')

createFormattedAdWithComparisonandPriceandMileage <- function(x){
  seleted.car <- df[x, ]
  # ----- hp ----
  perc <- ecdf(df[,'hp'])(seleted.car['hp'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.hp <- floor(perc.neu)
    top.string.hp <- sprintf("(Top %s%s)", top.10.hp, "%")
    line2 <- sprintf("* Horsepower: %s%s",df[x, ]$hp,top.string.hp)
  }
  else{
    line2 <- sprintf("* Horsepower: %s",df[x, ]$hp)
  }

  # ----- mpg -----
  perc <- ecdf(df[,'mpg'])(seleted.car['mpg'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.mpg <- floor(perc.neu)
    top.string.mpg <- sprintf("(Top %s%s)", top.10.mpg, "%")
    line4 <- sprintf("* Fuel Efficiency: %s mpg %s", df[x, ]$mpg, top.string.mpg)
  }
  else{
    line4 <- sprintf("* Fuel Efficiency: %s mpg", df[x, ]$mpg)
  }

  # ----- qsec -----
  perc <- ecdf(df[,'qsec'])(seleted.car['qsec'])
  perc.neu <- (1-perc)*100

  if(perc.neu <= 10){
    top.10.qsec <- floor(perc.neu)
    top.string.qsec <- sprintf("(Top %s%s)", top.10.qsec, "%")
    line5 <- sprintf("* 1/4 mile time: %s sec %s",df[x, ]$qsec, top.string.qsec)
  }
  else{
    line5 <- sprintf("* 1/4 mile time: %s sec",df[x, ]$qsec)
  }

  line1 <- sprintf("* %s",df[x, ]$names)
  line3 <- sprintf("* Cylinders: %s", df[x, ]$cyl)
  line7 <- sprintf("* Price: %s €", df.final[x, ]$Price)
  line8 <- sprintf("* Mileage: %s km", floor(df.final[x, ]$Mileage))
  lines <- c(line1, line2, line3, line4, line5, line7, line8)
  line6 <- paste(rep("*",max(nchar(lines))), collapse = "")
  lines_ins <- c(line6,lines,line6)
  return(tibble(lines_ins))
}

createFormattedAdWithComparisonandPriceandMileage(x)


createFormattedAdsWithComparisonsandPriceandMileage <- function(vehicleData, n){
  number.ads <- n
  ads <- sample(1:32, number.ads)
  ads
  return(map(ads, createFormattedAdWithComparisonandPriceandMileage))
}

createFormattedAdsWithComparisonsandPriceandMileage(df,5)









