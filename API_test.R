#Filename:      API_test
#Author:        P. Rivet
#Date:          18/04/22
#Description:   Looking up data from Quandl for analysis. Thanks to Eric @ AAARL(aaarl.ca)

rm(list=ls())
install.packages("Quandl")
install.packages("ggplot2")

library(Quandl)
library(ggplot2)

Quandl.api_key("DEn-L3cxxk9wNsLABTr-")

startdate<-"2012-3-4"
enddate<-"2017-09-14"

#retrieving data
skew<-Quandl("CBOE/SKEW", start_date=startdate,end_date=enddate, type='xts')
plot(skew)

housingmarket<-Quandl("YALE/RBCI")
summary(housingmarket)
plot(housingmarket)

#plotting correlations
install.packages("corrplot")
library(corrplot)

housingDF<-data.frame(housingmarket$`Cost Index`, housingmarket$`U.S. Population (Millions)`, housingmarket$`Long Rate`)
housingDF<-housingDF[-c(1,2,3),]#remove first 3 rows as they contain NAs
COR<-cor(housingDF)
corrplot(COR, method="number")#plot numeric values
corrplot(COR, method="ellipse")#plot w/ pretty shapes
