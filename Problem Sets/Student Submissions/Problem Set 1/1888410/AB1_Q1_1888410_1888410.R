# Problem Set 1
# Question 1

library(tidyverse)

head(mtcars)
#create a type column on the data frame mtcars with the row names
mtcars$Type = row.names(mtcars)
head(mtcars)


#a)
createAd <- function(vehicleData){
  # Your code

  return(vehicleData)
}


#createAd(mtcars[1,])
#oder so
createAd(mtcars['Ferrari Dino',])




#b)
createFormattedAd <- function(vehicleData){

# Your code
 cat(paste(paste(rep('*',30), collapse = ''),
           '\n*', row.names(vehicleData),
           '\n*Horsepowers:' ,vehicleData$hp,
           '\n*Cylinders: ', vehicleData$cyl ,
           '\n*Fuel Efficiency: ', vehicleData$mpg, 'mpg',
           '\n*1/4 mile time: ',
           vehicleData$qsec, 'sec \n', paste(rep('*',30), collapse = '')))

}

createFormattedAd(mtcars[2,])
#oder so
#createFormattedAd(mtcars['Ferrari Dino',])





#c)
createFormattedAdWithComparison <- function(vehicleData){
  # Your code
}

"
# Top 10%
head(df)
quantile(df$hp)


selectedCar <- df[2,]
perc <- ecdf(df[,'hp'])(selectedCar['hp'])


if (perc==1){
  value = (1-perc)*100
  top_string = paste(value, '%')
}

if (1-perc<1){
  value = floor(1-perc)*100
  top_string = paste(value, '%')
}
if (1-perc<=0.1){
  top_string = ' '
}


top_string
"




#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
  # Your code
}

"
#Stichproben ausgeben lassen

sample(df,10, replace = FALSE)
sample(nrow(df), 2, replace = FALSE)

df[sample(nrow(df), 2, replace = FALSE),]
"







#e)
# Your code

#Working directory: getwd()
#Set directory: z.B. setwd('~/ADS_19/Problem Sets')

mileage <- read.csv("~/ADS_19/Problem Sets/01/Data/carMileage.csv")
price <- read.csv("~/ADS_19/Problem Sets/01/Data/carPrices.csv")

#merge der beiden DF's auf mtcars
mtcars<- merge(mtcars,mileage,by="Type")
mtcars<- merge(mtcars,price,by="Type")

#Kurzer check (sollten gleich sein):
#mtcars[mtcars$Price == max(mtcars$Price),]
#price[price$Price == max(price$Price),]

head(mtcars)


