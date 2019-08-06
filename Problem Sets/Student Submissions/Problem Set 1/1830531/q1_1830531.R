# Problem Set 1
# Question 1

library(tidyverse)
#a)
createAd <- function(vehicleData){
  vehicleData
}


#b)
createFormattedAd <- function(vehicleData){
  names <- row.names(vehicleData)
  hp    <- vehicleData[,"hp"]
  cyl   <- vehicleData[,"cyl"]
  fe    <- vehicleData[,"mpg"]
  qm    <- vehicleData[,"qsec"]
  s     <- strrep("*", 27)
  lnames<- strrep(" ", 24-nchar(names))
  lhp   <- strrep(" ", 24-nchar(hp)-12)
  lcyl  <- strrep(" ", 24-nchar(cyl)-11)
  lfe  <- strrep(" ", 24-nchar(round(fe))-20)
  lqm  <- strrep(" ", 24-length(round(qm))-19)
  cat(sprintf("%s\n* %s%s*\n* Horsepower: %s%s* \n* Cylinders: %s%s*\n* Fuel Efficiency: %smpg%s*\n* 1/4 mile time: %ssec%s*\n%s",
              s, names,lnames, hp, lhp, cyl, lcyl, round(fe), lfe, round(qm), lqm, s))
}


#c)
createFormattedAdWithComparison <- function(vehicleData){
  names <- row.names(vehicleData)
  hp    <- vehicleData[,"hp"]
  cyl   <- vehicleData[,"cyl"]
  fe    <- vehicleData[,"mpg"]
  qm    <- vehicleData[,"qsec"]
  s     <- strrep("*", 35)

  lnames<- strrep(" ", 32-nchar(names))
  lhp   <- strrep(" ", 32-nchar(hp)-13)
  lcyl  <- strrep(" ", 32-nchar(cyl)-11)
  lfe  <- strrep(" ", 32-nchar(round(fe))-21)
  lqm  <- strrep(" ", 32-length(round(qm))-20)

  if (ecdf(mtcars$hp)(vehicleData[,"hp"]) == 1){
    topadHP <- "(Top 1%)"
    lhp  <- strrep(" ", 8)
  } else if (ecdf(mtcars$hp)(vehicleData[,"hp"]) >= 0.9){
    topadHP <- paste0("(Top", " ", round((1-ecdf(mtcars$hp)(vehicleData[,"hp"]))*100), "%)")
    lhp  <- strrep(" ", 8)
  } else {
    topadHP = ""
  }

  if (ecdf(mtcars$mpg)(vehicleData[,"mpg"]) == 1){
    topadFe <- "(Top 1%)"
    lfe  <- strrep(" ", 1)
  } else if (ecdf(mtcars$mpg)(vehicleData[,"mpg"]) >= 0.9){
    topadFe <- paste0("(Top", " ", round((1-ecdf(mtcars$mpg)(vehicleData[,"mpg"]))*100), "%)")
    lfe  <- strrep(" ", 1)
  } else {
    topadFe = ""
  }

  if (ecdf(mtcars$qsec)(vehicleData[,"qsec"]) == 0){
    topadQm <- "(Top 1%)"
    lqm  <- strrep(" ", 3)
  } else if (ecdf(mtcars$qsec)(vehicleData[,"qsec"]) <= 0.1){
    topadQm <- paste0("(Top", " ", round(ecdf(mtcars$qsec)(vehicleData[,"qsec"])*100), "%)")
    lqm  <- strrep(" ", 3)
  } else {
    topadQm = ""
  }
  cat(sprintf("%s\n* %s%s*\n* Horsepower: %s %s%s* \n* Cylinders: %s%s*\n* Fuel Efficiency: %smpg %s%s*\n* 1/4 mile time: %ssec %s%s*\n%s",
              s, names,lnames, hp, topadHP, lhp, cyl, lcyl, round(fe), topadFe, lfe, round(qm),topadQm, lqm, s))

}

#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
    ssize <- n
    vehicleData %>%
      rownames_to_column() %>%
      sample_n(ssize, replace = FALSE) %>%
      column_to_rownames() -> vehicleDataS
  for (i in 1:nrow(vehicleDataS)) {
      names <- row.names(vehicleDataS)[i]
      hp    <- vehicleDataS[i,"hp"]
      cyl   <- vehicleDataS[i,"cyl"]
      fe    <- vehicleDataS[i,"mpg"]
      qm    <- vehicleDataS[i,"qsec"]
      s     <- strrep("*", 35)

      lnames<- strrep(" ", 32-nchar(names))
      lhp   <- strrep(" ", 32-nchar(hp)-13)
      lcyl  <- strrep(" ", 32-nchar(cyl)-11)
      lfe  <- strrep(" ", 32-nchar(round(fe))-21)
      lqm  <- strrep(" ", 32-length(round(qm))-20)

      if (ecdf(mtcars$hp)(vehicleDataS[i,"hp"]) == 1){
        topadHP <- "(Top 1%)"
        lhp  <- strrep(" ", 8)
      } else if (ecdf(mtcars$hp)(vehicleDataS[i,"hp"]) >= 0.9){
        topadHP <- paste0("(Top", " ", round((1-ecdf(mtcars$hp)(vehicleDataS[i,"hp"]))*100), "%)")
        lhp  <- strrep(" ", 8)
      } else {
        topadHP = ""
      }

      if (ecdf(mtcars$mpg)(vehicleDataS[i,"mpg"]) == 1){
        topadFe <- "(Top 1%)"
        lfe  <- strrep(" ", 1)
      } else if (ecdf(mtcars$mpg)(vehicleDataS[i,"mpg"]) >= 0.9){
        topadFe <- paste0("(Top", " ", round((1-ecdf(mtcars$mpg)(vehicleDataS[i,"mpg"]))*100), "%)")
        lfe  <- strrep(" ", 1)
      } else {
        topadFe = ""
      }

      if (ecdf(mtcars$qsec)(vehicleDataS[i,"qsec"]) == 0){
        topadQm <- "(Top 1%)"
        lqm  <- strrep(" ", 3)
      } else if (ecdf(mtcars$qsec)(vehicleDataS[i,"qsec"]) <= 0.1){
        topadQm <- paste0("(Top", " ", round(ecdf(mtcars$qsec)(vehicleDataS[i,"qsec"])*100), "%)")
        lqm  <- strrep(" ", 3)
      } else {
        topadQm = ""
      }
      cat(sprintf("%s\n* %s%s*\n* Horsepower: %s %s%s* \n* Cylinders: %s%s*\n* Fuel Efficiency: %smpg %s%s*\n* 1/4 mile time: %ssec %s%s*\n%s\n",
                  s, names,lnames, hp, topadHP, lhp, cyl, lcyl, round(fe), topadFe, lfe, round(qm),topadQm, lqm, s))
    }
  }



#e)
mileage <- read_csv("Problem Sets/01/Data/carMileage.csv")
prices <- read_csv("Problem Sets/01/Data/carPrices.csv")
PrMi <- merge(mileage, prices)
mtcars <- rownames_to_column(mtcars)
mtcars_new <- merge(mtcars, PrMi, by.x = "rowname", by.y = "Type")
