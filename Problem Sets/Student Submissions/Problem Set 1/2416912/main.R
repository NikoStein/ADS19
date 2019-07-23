
### PREPARATION
library(tidyverse)
mtcars$hp <- mtcars$hp * 0.735
#attributes(mtcars)
source('q1_template.R')
df <- data.frame(mtcars)
df$names = row.names(df)

## which car are you looking for - exact entry required
testCar = "Toyota Corolla"
id = which(row.names(mtcars) == testCar)

## Data of the searched car
auto <- c(df[id, ]$names)
horsepower <- c(df[id, ]$hp)
cylinders <- c(df[id, ]$cyl)
fuel <- c(df[id, ]$mpg)
qSec <- c(df[id, ]$qsec)
vehicleDataB <- data.frame(auto, horsepower, cylinders, fuel, qSec)

## e)
prices <- read.csv('./Problem Sets/01/Data/carPrices.csv')
mileage <- read.csv('./Problem Sets/01/Data/carMileage.csv')

de <- merge(prices, mileage, by.x = "Type", by.y = "Type", all=TRUE)
de[is.na(de)] <- 0

finalCars <- merge(mtcars, de,by.x = 0, by.y = "Type", all=TRUE)

## a)
createAd("1")
createAd(1)

## b)
createFormattedAd(vehicleDataB)

## c)
createFormattedAdWithComparison(finalCars[20,])

## d)
createFormattedAdsWithComparisons(finalCars, 10)

## e)
allCars <- list(createFormattedAdsWithComparisons(finalCars, 32))


