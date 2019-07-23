# Problem Set 1
# Question 1


#a)
#Load Dataset
data("mtcars")

#Create function
createAd <- function(vehicleData){
 mtcars[vehicleData,]
}

#Test
createAd("Datsun 710")




#b)

df <- data.frame(row.names(mtcars), mtcars$hp, mtcars$cyl, mtcars$mpg, mtcars$qsec, row.names = 1)


createFormattedAd <- function(vehicleData){
  paste(cat(paste(rep("*", 26), collapse = ""), "\n*", vehicleData, paste(rep(" ", 7-length(vehicleData))),
       "*\n* Horsepower: ", df[vehicleData,1], paste(rep(" ", 4-length(df[vehicleData,1]))),
       "*\n* Cylinders: ", df[vehicleData,2], paste(rep(" ", 6-length(df[vehicleData,2]))),
       "*\n* Fuel Efficiency: ", df[vehicleData,3], paste(rep(" ", 1-length(df[vehicleData,3]))),
       "*\n* 1/4 mile time: ", df[vehicleData,4], paste(rep(" ", 2-length(df[vehicleData,4]))),
       "*\n", paste(rep("*", 26), collapse ="")))
}


#Test
createFormattedAd("Datsun 710")





#c)

# Verteilungsfunktion aufstellen
percentilehp <- ecdf(mtcars$hp)


createFormattedAdWithComparison <- function(vehicleData){
  paste(cat(paste(rep("*", 26), collapse = ""), "\n*", vehicleData, paste(rep(" ", 7-length(vehicleData))),
            "*\n* Horsepower: ", df[vehicleData,1], if(df[vehicleData,1]>quantile(mtcars$hp, 0.9)){
              paste("Top ", (1-percentilehp(df[vehicleData,1]))*100 , "%", collapse = "")
            },
            paste(rep(" ", 4-length(df[vehicleData,1]))),
            "*\n* Cylinders: ", df[vehicleData,2], paste(rep(" ", 6-length(df[vehicleData,2]))),
            "*\n* Fuel Efficiency: ", df[vehicleData,3], paste(rep(" ", 1-length(df[vehicleData,3]))),
            "*\n* 1/4 mile time: ", df[vehicleData,4], paste(rep(" ", 2-length(df[vehicleData,4]))),
            "*\n", paste(rep("*", 26), collapse ="")))
}

#Test
createFormattedAdWithComparison("Camaro Z28")




#d)

#Nur Teil der Funktion fertig

createFormattedAdsWithComparisons <- function(vehiclesData, n){
    sampledf  <- vehiclesData[sample(nrow(vehiclesData), n, replace = FALSE),]
    sampledf

}

# Test

createFormattedAdsWithComparisons(df, 6)


#e)
# Your code
