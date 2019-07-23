# Problem Set 1
# Question 1


#a) Find the car values over their numeric row id, add all the values to mydata and paste these the values
#   -- if no numeric value ->  guide
createAd <- function(vehicleData) {
  if (length(vehicleData) > 0 & is.numeric(vehicleData)) {
    mydata <- data.frame(mtcars[vehicleData, ])

    names(mydata) <- c(names(mtcars))

    print(names(mydata))
    paste(mydata)
  } else
    print("**Alert**: Type in a numeric Row  index!!")
}

#b) Formatted output of Name, Horsepower, Cylinders, Fuel Efficiency, and 1/4 mile time of a car

createFormattedAd <- function(vehicleDataB) {
  cat('\n', paste(rep('*', 26), collapse = ""), '\n')
  cat(sprintf(' * %s         *\n', vehicleDataB[1,]$auto))
  cat(sprintf(' * Horsepower: %s         *\n' , round(vehicleDataB[1,]$horsepower,0)))
  cat(sprintf(' * Cylinders: %s           *\n' , vehicleDataB[1,]$cylinders))
  cat(sprintf(' * Fuel Efficiency: %smpg *\n' , round(vehicleDataB[1,]$fuel, 0)))
  cat(sprintf(' * 1/4 mile time %ssec    *\n' , round(vehicleDataB[1,]$qSec, 0)))
  cat('', paste(rep('*', 26), collapse = ""), '\n')

  # repeat {
  #   x <- x + 1
  #   sprintf('%s \n' , vehicleDataB[1,]$auto)
  #   if (x == ){
  #     break
  #   }
  # }
}

#c)
createFormattedAdWithComparison <- function(vehicleDataC) {
  mpgQuantile <- quantile(mtcars[,]$mpg, c(0.9))
  hpQuantile <- quantile(mtcars$hp, c(0.9))
  qSecQuantile <- quantile(mtcars$qsec, c(0.1))

  cat('\n', paste(rep('*', 26), collapse = ""), '\n')
  cat(sprintf(' * %s         *\n', row.names(vehicleDataC)))

  if (vehicleDataC$hp >= hpQuantile) {
    cat(sprintf(' * Horsepower: %s         * Top 10%% \n', round(vehicleDataC$hp, 0)))
  } else {
    cat(sprintf(' * Horsepower: %s         *\n' , round(vehicleDataC$hp, 0)))
  }

  cat(sprintf(' * Cylinders: %s           *\n' , vehicleDataC$cyl))
  cat(sprintf(' * Price: %s           *\n' , round(as.numeric(vehicleDataC$Price,0))))
  cat(sprintf(' * Mileage: %s         *\n' , round(as.numeric(vehicleDataC$Mileage,0))))

  if (vehicleDataC$mpg >= mpgQuantile) {
    cat(sprintf(
      ' * Fuel Efficiency: %smpg * Top 10%% \n' ,
      round(vehicleDataC$mpg, 0)
    ))
  } else {
    cat(sprintf(' * Fuel Efficiency: %smpg *\n' , round(vehicleDataC$mpg, 0)))
  }

  if (vehicleDataC$qsec <= qSecQuantile) {
    cat(sprintf(
      ' * 1/4 mile time %ssec    * Top 10%% \n' ,
      round(vehicleDataC$qsec, 0)
    ))
  } else {
    cat(sprintf(' * 1/4 mile time %ssec    *\n' , round(vehicleDataC$qsec, 0)))
  }

  cat('', paste(rep('*', 26), collapse = ""), '\n')
}

#d)
createFormattedAdsWithComparisons <- function(vehiclesData, n) {
  ds <- data.frame(sample_n(vehiclesData, n, replace = FALSE))
  for(i in 1:n) {
    createFormattedAdWithComparison(ds[i,])
  }
}

#e)
# Your code
#by statement richtige zeilen auswählen
