# Problem Set 1
# Question 1


#a)
library(tidyverse) #tidyverse laden - immer gut zu Anfang
vehicleData <- "Maserati Bora"#Variable definieren
df <- mtcars #df definieren: einfacher zu lesen + schreiben
createAd <- function(vehicleData){
  df[vehicleData, ] # Variable=Zeile, " " Spalte -> da leer: alle Spalten; optional 1:X -> Spalte 1-X
}
createAd(vehicleData)

#b)
#mtcars [1,1:7] ersten sieben Spalten ausgeben
#select(mtcars, 4, 2, 1, 7) spalten 4,2,1,7 aus mtcars ausgeben

#Moeglichkeit1 - cat() prints arguements
cat(" *", vehicleData,"\n",
    "* Horsepower:",mtcars[vehicleData, 4],"\n", #Zeilenname = Autoname, 4 =Spalte
    "* Cylinders:",mtcars[vehicleData, 2],"\n",
    "* Fuel Efficiency:", mtcars[vehicleData, 1],"\n",
    "* 1/4 mile time:", mtcars[vehicleData, 7],"\n")




createFormattedAd <- function(vehicleData){
#variablen definieren:
      hp<-df[vehicleData, 4]
      cyl<-df[vehicleData, 2]
      mpg<-df[vehicleData, 1]
      sec<-round(df[vehicleData, 7], 0)
  cat(" *", vehicleData,"\n",
      sprintf(" * Horsepower: %i*", hp),"\n",
      sprintf("* Cylinders: %i*", cyl),"\n",
      sprintf("* Fuel Efficiency: %impg*", mpg),"\n",
      sprintf("* 1/4 mile time: %isec*", sec))
}
createFormattedAd(vehicleData)

#sprintf Datentypen: i= XX; g=XX.XX f=XX.XXXXXX; s=string
#c)
createFormattedAdWithComparison <- function(vehicleData){
#Topbedingung setzen: hier kein quantile(), sondern ecdf
#tophp
      hpprozent=round(100*(1-(ecdf(df$hp)(df[vehicleData, "hp"])))) #ecdf(Vergleichsreihe)(Datenpunkt)
      if (hpprozent ==0){
        tophp="(best car)"
      } else if (hpprozent < 10){
        tophp=sprintf("(Top %iProzent)", hpprozent)
      } else {
        tophp=""
      }
#topmpg
      mpgprozent<-round(100*(1-(ecdf(df$mpg)(df[vehicleData, "mpg"]))))
      if (mpgprozent ==0){
        topmpg="(best car)"
      } else if (mpgprozent < 10){
        topmpg=sprintf("(Top %iProzent)", mpgprozent)
      } else {
        topmpg=""
      }
#topsec
      secprozent<-round(100*(ecdf(df$qsec)(df[vehicleData, "qsec"])))
      if (secprozent ==0){
        topsec="(best car)"
      } else if (secprozent < 10){
        topsec=sprintf("(Top %iProzent)", secprozent)
      } else {
        topsec=""
      }
#print the statements
  cat(" *", vehicleData,"*","\n",
      sprintf("* Horsepower: %i %s*", hp, tophp),"\n",
      sprintf("* Cylinders: %i*", cyl),"\n",
      sprintf("* Fuel Efficiency: %impg %s*", mpg, topmpg),"\n",
      sprintf("* 1/4 mile time: %isec %s*", sec, topsec))
}
createFormattedAdWithComparison("Toyota Corolla")

#d)
createFormattedAdsWithComparisons <- function(vehicleData, n){
rows<-nrow(df)
dfsample<-sample(c(1:rows), n, replace = FALSE)
# Variable<-samplefunktion(Eingangsvektor mit Einträgen=Zeilen, n Ziehungen, replace = False> ohne zurücklegen)
m<-1

  while (m<=n) {
#Auto auswählen
vehicleData<-row.names(df[dfsample[m], ]) #aus Zufallszahlen erster "Vektoreintrag"
#variablen Definieren
hp<-df[vehicleData, 4]
cyl<-df[vehicleData, 2]
mpg<-round(df[vehicleData, 1])
sec<-round(df[vehicleData, 7], 0)
hpprozent=round(100*(1-(ecdf(df$hp)(df[vehicleData, "hp"])))) #ecdf(Vergleichsreihe)(Datenpunkt)
    if (hpprozent ==0){
      tophp="(best car)"
    } else if (hpprozent < 10){
      tophp=sprintf("(Top %iProzent)", hpprozent)
    } else {
      tophp=""
    }
    #topmpg
    mpgprozent<-round(100*(1-(ecdf(df$mpg)(df[vehicleData, "mpg"]))))
    if (mpgprozent ==0){
      topmpg="(best car)"
    } else if (mpgprozent < 10){
      topmpg=sprintf("(Top %iProzent)", mpgprozent)
    } else {
      topmpg=""
    }
    #topsec
    secprozent<-round(100*(ecdf(df$qsec)(df[vehicleData, "qsec"])))
    if (secprozent ==0){
      topsec="(best car)"
    } else if (secprozent < 10){
      topsec=sprintf("(Top %iProzent)", secprozent)
    } else {
      topsec=""
    }
    #print the statements
    cat("\n","\n", "*", vehicleData,"*","\n",
        sprintf("* Horsepower: %i %s*", hp, tophp),"\n",
        sprintf("* Cylinders: %i*", cyl),"\n",
        sprintf("* Fuel Efficiency: %impg %s*", mpg, topmpg),"\n",
        sprintf("* 1/4 mile time: %isec %s*", sec, topsec, "\n","\n"))
  m<-m+1
  }
}

createFormattedAdsWithComparisons(vehicleData, 5)

  #e)
getwd() #shows workingdictory
setwd("C:/Users/.../Problem Sets/01/Data") #sets new wd
prices <- read.csv("C:/Users/.../Problem Sets/01/Data/carPrices.csv")
mileage <- read.csv("C:/Users/.../Problem Sets/01/Data/carMileage.csv")
#read.csv() je nach Tabellenart anpassen
new_df<-merge(prices, mileage)

new_df["Type", "Price"]
#selbst nicht gemacht, da ich mit df mit Reihennamen für die Funktionen genutzt habe

select(new_df$Type=vehicleData, Price)
#ggf Attribut zum "mergen" angeben
select(vehicleData)
