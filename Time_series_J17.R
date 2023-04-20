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
end <- as.Date(j17$DailyHighDate[1],format='%Y-%m-%d')
bgn <- as.Date(j17$DailyHighDate[x<-length(j17$DailyHighDate)],format='%Y-%m-%d') 
datex <- seq.Date(bgn,end,'day')
pdatex <- as.Date(j17$DailyHighDate,format='%Y-%m-%d')

#Check if missing value exit
theo <- length(datex)
actu <- length(j17$DailyHighDate)
if (theo>actu) {
  print ("Missing Values")
} else {
  print("No missing values")
}

#Create a zoo object
WL.zoo <-zoo(j17$WaterLevelElevation, pdatex)
dum.zoo <- zoo(,datex)
WL.zoom <- merge(dum.zoo, WL.zoo)
plot(WL.zoom, xlab='Year', ylab='Water Level (feet)')
summary(WL.zoom)

#Interpolate
WL.ts <-as.ts(WL.zoom)
WL.tsf <- na_kalman(WL.ts,model = "StructTS")
WL.zoof <- zoo(WL.tsf,datex)
plot(WL.zoof, xlab='Year', ylab='Water Level (feet)')
summary(WL.zoof)

#10-day moving average
WL10d <- rollmean(WL.zoof, 10, align = 'right')
plot(WL10d, xlab='Year', ylab='Water Level (feet)')

#Aggregate to monthly values using mean
WLmon <- apply.monthly(as.xts(WL.zoof),mean)
plot(WLmon, xlab='Year', ylab='Water Level (feet)')

#calculate autocorrelation
acf(WL.zoof,lag.max=NULL,main='ACF for J17', type = c("correlation"))
pacf(WL.zoof, main='PACF J17', type="o")

#decompose time series
a<- ts(WLmon, frequency = 12)
deco <- decompose(a)
plot(deco)
