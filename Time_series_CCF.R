library (lubridate)
library(forecast)
library(zoo)
library(xts)
library(imputeTS)
library(tseries)

# ALWAYS USE / instead of \ when defining path in R.
path <- 'F:/Semester 1 Spring 2023/Machine Learning for CVEN/Project_5'  # Path of the working directory CHANGE IT TO YOURS
setwd(path)  # set working directory
getwd()  # check the current working directory

#Reads data from a CSV file,
j17 = read.csv("j17.csv")
comal = read.csv('Comal.csv')
san_marcos = read.csv('San_Marcos.csv')
x<-nrow(j17)
y<-nrow(comal)
z<-nrow(san_marcos)

#Extract begin and end dates  #%Y-%m-%d #%m/%d/%Y
#endJ <- as.Date(j17$DailyHighDate[1],format='%Y-%m-%d')
#bgnJ <- as.Date(j17$DailyHighDate[x<-length(j17$DailyHighDate)],format='%Y-%m-%d') 
#datexJ <- seq.Date(bgnS,endS,'day')
pdatexJ <- as.Date(j17$DailyHighDate,format='%Y-%m-%d')

#Extract begin and end dates Comal
#bgnC <- as.Date("1932")
#endC <-  "09/04/2023"
#datexC <- seq.Date(bgnS,endS,'day')
pdatexC <- as.Date(comal$datetime,format='%d/%m/%Y')

#Extract begin and end dates  San Marcos
bgnS <- as.Date(san_marcos$datetime[1],format='%d/%m/%Y')
endS <- as.Date(san_marcos$datetime[x<-length(san_marcos$datetime)],format='%d/%m/%Y') 
datexS <- seq.Date(bgnS,endS,'day')
pdatexS <- as.Date(san_marcos$datetime,format='%d/%m/%Y')

#Create a zoo object J17
WL.zooJ <-zoo(j17$WaterLevelElevation, datexS)
dum.zooJ <- zoo(,datexS)
WL.zoomJ <- merge(dum.zooJ, WL.zooJ)
ggplot_na_distribution(WL.zooJ)
plot(WL.zooJ, main='J17', xlab='Year', ylab='Water Level (feet)')

#Create a zoo object Comal
WL.zooC <-zoo(comal$discharge_cfs, datexS)
dum.zooC <- zoo(,datexS)
WL.zoomC <- merge(dum.zooC, WL.zooC)

#Create a zoo object San Marcos
WL.zooS <-zoo(san_marcos$discharge_cfs, datexS)
dum.zooS <- zoo(,datexS)
WL.zoomS <- merge(dum.zooS, WL.zooS)

#Interpolate J17
WL.tsJ <-as.ts(WL.zoomJ)
WL.tsfJ <- na_kalman(WL.tsJ,model = "StructTS")
WL.zoofJ <- zoo(WL.tsfJ,datexS)

#Interpolate Comal
WL.tsC <-as.ts(WL.zoomC)
WL.tsfC <- na_kalman(WL.tsC,model = "StructTS")
WL.zoofC <- zoo(WL.tsfC,datexS)

#Interpolate San Marcos
WL.tsS <-as.ts(WL.zoomS)
WL.tsfS <- na_kalman(WL.tsS,model = "StructTS")
WL.zoofS <- zoo(WL.tsfS,datexS)

plot(WL.zooJ, main='J17', xlab='Year', ylab='Water Level (feet)')
plot(WL.zoofC, main='Comal', xlab='Year', ylab='Water Level (feet)')
plot(WL.zoofS, main='San Marcos', xlab='Year', ylab='Water Level (feet)')

#Cross correlation function
ccf(WL.zoofJ, WL.zoofC)
ccf(WL.zoofJ, WL.zoofS)
ccf(WL.zoofC, WL.zoofS)
