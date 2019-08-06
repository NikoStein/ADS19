# Problem Set 1
# Question 1

library(tidyverse)
x = mtcars[29,]
mtcars$names
mtcars[1,0]


#a)

createAd <- function(vehicleData){

  df = data.frame(vehicleData) #vehicleData wird in ein df-Frame umgewandelt
  return(df) #Funktion gibt df zurueck
}

print(createAd(x))



#b)
createFormattedAd <- function(vehicleData){
  df = data.frame(vehicleData) #Set Variable to Dataframe of Input Data



  print(strrep("*",28)) #maxlength?
  print(sprintf("* %s %s*", rownames(df), strrep(" ",24-nchar(rownames(df))))) #Prints string with variables from Dataframe and adds spaces depending on length of inputdata
  print(sprintf("* Horsepower: %s %s*",df$hp, strrep(" ",12-nchar(df$hp)))) #Leerzeiche-Anzahl könnte man besser lösen
  print(sprintf("* Cylinders: %s %s*", df$cyl, strrep(" ",13-nchar(df$cyl))))
  print(sprintf("* Fuel Efficiency: %smpg %s*", df$mpg, strrep(" ",4-nchar(df$mpg))))
  print(sprintf("* 1/4 mile time: %ssec %s*", round(df$qsec, 0), strrep(" ",8-nchar(df$qsec))))
  print(strrep("*", 28))
}



createFormattedAd(x)


#c)
createFormattedAdWithComparison <- function(vehicleData){

  df = data.frame(vehicleData)

  percentiles <- function(mtcars,vehicleData) ecdf(mtcars)(vehicleData) #ecdf-Funktion um das Percentile des Wertes zu bekommen

  perc <- function(mtcars, vehicleData){
    if(percentiles(mtcars,vehicleData) > 0.90){ #filtert die Top 10% (größer 0.90)
      return(sprintf("Top %s%%",round((1 - percentiles(mtcars, vehicleData))*100,0))) #Formatiert bspw. 0.93 in Top 7%
    }
    if(percentiles(mtcars,vehicleData)==1){ #Beim "besten" Wert wird der String Top 1%
      return(paste("Top 1%"))
    }
    else {
      return("       ") #Ansonsten Leerzeichen wegen Formatierung // auch hier könnte besser gelöst sein
    }
  }

  perc_qsec <- function(mtcars, vehicleData){  #FÜr quartermile time muss der Filter auf die niedrigsten 10% aller Werte umgeändert werden
    if(percentiles(mtcars,vehicleData) < 0.10){
      return(sprintf("Top %s%%",round((percentiles(mtcars, vehicleData))*100,0)))
    }
    if(percentiles(mtcars,vehicleData)==0){
      return(paste("Top 1%"))
    }
    else {
      return("       ")
    }
  }


  qhp <- perc(mtcars$hp,df$hp) #"Top X%"-String wird in variable gespeichert
  qmpg <- perc(mtcars$mpg,df$mpg)
  qmsec <- perc_qsec(mtcars$qsec,df$qsec)


  print("**************************************")
  print(sprintf("* %s %s*", rownames(df), strrep(" ",34-nchar(rownames(df))))) #Prints string with variables from Dataframe and adds spaces depending on length of inputdata
  print(sprintf("* Horsepower: %s %s %s*",df$hp,qhp, strrep(" ",15-nchar(df$hp))))
  print(sprintf("* Cylinders: %s %s*", df$cyl, strrep(" ",23-nchar(df$cyl))))
  print(sprintf("* Fuel Efficiency: %smpg %s %s*", df$mpg, qmpg, strrep(" ",6-nchar(df$mpg))))
  print(sprintf("* 1/4 mile time: %ssec %s %s*", round(df$qsec, 0),qmsec, strrep(" ",11-nchar(df$qsec))))
  print("**************************************")

}


createFormattedAdWithComparison(x)



#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
  df = data.frame(vehicleData)
  for(i in 1:n){ #for-Schleife die durch die Anzahl (n) der gewünschten Werbeanzeigen durchläuft
    createFormattedAdWithComparison(df[i,])
  }
}

#createFormattedAdsWithComparisons(mtcars,7) Test-Aufruf

#e)

mileage <- data.frame(read.csv("E:/GitHub/ADS_01/Problem Sets/01/Data/carMileage.csv"))
prices <- data.frame(read.csv("E:/GitHub/ADS_01/Problem Sets/01/Data/carPrices.csv"))
new_df <- merge(mtcars, mileage, by.x= 0, by.y = 1)
newnew_df <- merge(new_df,prices, by = 1)


new_createFormattedAdWithComparison <- function(vehicleData){

  df = data.frame(vehicleData)

  percentiles <- function(mtcars,vehicleData) ecdf(mtcars)(vehicleData) #ecdf-Funktion um das Percentile des Wertes zu bekommen

  perc <- function(mtcars, vehicleData){
    if(percentiles(mtcars,vehicleData) > 0.90){ #filtert die Top 10% (größer 0.90)
      return(sprintf("Top %s%%",round((1 - percentiles(mtcars, vehicleData))*100,0))) #Formatiert bspw. 0.93 in Top 7%
    }
    if(percentiles(mtcars,vehicleData)==1){ #Beim "besten" Wert wird der String Top 1%
      return(paste("Top 1%"))
    }
    else {
      return("       ") #Ansonsten Leerzeichen wegen Formatierung // auch hier könnte besser gelöst sein
    }
  }

  perc_qsec <- function(mtcars, vehicleData){  #FÜr quartermile time muss der Filter auf die niedrigsten 10% aller Werte umgeändert werden
    if(percentiles(mtcars,vehicleData) < 0.10){
      return(sprintf("Top %s%%",round((percentiles(mtcars, vehicleData))*100,0)))
    }
    if(percentiles(mtcars,vehicleData)==0){
      return(paste("Top 1%"))
    }
    else {
      return("       ")
    }
  }


  qhp <- perc(mtcars$hp,df$hp) #"Top X%"-String wird in variable gespeichert
  qmpg <- perc(mtcars$mpg,df$mpg)
  qmsec <- perc_qsec(mtcars$qsec,df$qsec)


  print("**************************************")
  print(sprintf("* %s %s*", rownames(df), strrep(" ",34-nchar(rownames(df))))) #Prints string with variables from Dataframe and adds spaces depending on length of inputdata
  print(sprintf("* Horsepower: %s %s %s*",df$hp,qhp, strrep(" ",15-nchar(df$hp))))
  print(sprintf("* Cylinders: %s %s*", df$cyl, strrep(" ",23-nchar(df$cyl))))
  print(sprintf("* Fuel Efficiency: %smpg %s %s*", df$mpg, qmpg, strrep(" ",6-nchar(df$mpg))))
  print(sprintf("* 1/4 mile time: %ssec %s %s*", round(df$qsec, 0),qmsec, strrep(" ",11-nchar(df$qsec))))
  print(sprintf("* Mileage: %s *", df$Mileage)) #Printet alle Mileage / Price-Werte?!?! Fehlerhaft
  print(sprintf("* Price: %s *", df$Price))
  print("**************************************")

}

new_createFormattedAdWithComparison(newnew_df[3,])
