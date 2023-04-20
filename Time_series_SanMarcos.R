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
j17 = read.csv("J17.csv")
comal = read.csv('Comal.csv')
san_marcos = read.csv('San_Marcos.csv')
x<-nrow(j17)
y<-nrow(comal)
z<-nrow(san_marcos)

#Extract begin and end dates  #%Y-%m-%d #%m/%d/%Y
bgn <- as.Date(san_marcos$datetime[1],format='%d/%m/%Y')
end <- as.Date(san_marcos$datetime[x<-length(san_marcos$datetime)],format='%d/%m/%Y') 
datex <- seq.Date(bgn,end,'day')
pdatex <- as.Date(san_marcos$datetime,format='%d/%m/%Y')

#Check if missing value exit
theo <- length(datex)
actu <- length(san_marcos$datetime)
if (theo>actu) {
  print ("Missing Values")
} else {
  print("No missing values")
}

#Create a zoo object
WL.zoo <-zoo(san_marcos$discharge_cfs, pdatex)
dum.zoo <- zoo(,datex)
WL.zoom <- merge(dum.zoo, WL.zoo)
plot(WL.zoom, xlab='Year', ylab='Discharge (cfs)')
summary(WL.zoom)

#Interpolate
# WL.ts <-as.ts(WL.zoom)
# WL.tsf <- na_kalman(WL.ts,model = "StructTS")
# WL.zoof <- zoo(WL.tsf,datex)
# plot(WL.zoof, xlab='Year', ylab='Discharge (cfs)')
# summary(WL.zoof)

#10-day moving average
WL10d <- rollmean(WL.zoom, 10, align = 'right')
plot(WL10d, main = "10-day moving average San Marcos", xlab='Year', ylab='Discharge (cfs)')

#Aggregate to monthly values using mean
WLmon <- apply.monthly(as.xts(WL.zoom),mean)
plot(WLmon, main="Monthly average San Marcos",xlab='Year', ylab='Discharge (cfs)')

#calculate autocorrelation
acf(WL.zoom,lag.max=NULL,main='ACF for San Marcos', type = c("correlation"))
pacf(WL.zoom, main='PACF San Marcos', type="o")

#decompose time series
a<- ts(WLmon, frequency = 12)
deco <- decompose(a)
plot(deco)
