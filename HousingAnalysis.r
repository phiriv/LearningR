#playing around with housing data
#thanks to Eric @ AARL --> https://www.aaarl.ca/
#install.packages('readxl')

library(readxl)
Housing_Training_Set_csv <- read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/Housing Training Set.csv.xlsx")
View(Housing_Training_Set_csv)

#install.packages('readr')
library(readr)

Housing_Training_Set_csv2 <- read_csv("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/Housing Training Set.csv.xlsx")
View(Housing_Training_Set_csv)

housing<-Housing_Training_Set_csv

#previewing the top/bottom of the dataset
head(housing)
tail(housing)

#selecting certain rows
housing[1:3,]

#picking a column
housing$SalePrice

#CASE SENSITIVITY EXISTS
View(housing)

housing[10,5]
housing$LotArea[10]

#these should be self-evident
summary(housing)
colnames(housing)

plot(housing$LotArea, housing$SalePrice, main="Sales Price vs. Lot Area", col="red", type="s")
plot(housing$MSSubClass, housing$OverallCond, main="Overall Condition vs. MS Sub Class", col="purple")
plot(housing$LotFrontage, housing$BsmtFinSF1, main="Basement Finished Square Feet vs. Lot Frontage", col="green")
#convert non-numeric cols for plotting
plot(as.factor(housing$Alley),housing$BsmtUnfSF)

#unsquared correlation coeff
cor(housing$SalePrice,housing$LotArea)

#remove outliers
housingCleaned<-housing[which(housing$LotArea<50000),]
plot(housingCleaned$LotArea,housingCleaned$SalesPrice)
cor(housingCleaned$SalePrice,housingCleaned$LotArea)

#adding cols
housingCleaned$Age<-2018-housingCleaned$YearBuilt

housingCleaned$Age[2]
housingCleaned$YearBuilt[2]
plot(housingCleaned$Age, housingCleaned$SalePrice)

#gimme the usual
mean(housingCleaned$SalePrice)
median(housingCleaned$SalePrice)
mode(housingCleaned$SalePrice)#SCAM!

# Create the function for mode ourselves
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(housingCleaned$SalePrice)

#le regulier
min(housingCleaned$SalePrice)
max(housingCleaned$SalePrice)
sd(housingCleaned$SalePrice)

#install.packages('psych')
library('psych')
describe(housingCleaned$SalePrice)

library(readxl)
housingTrain <- read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/Housing Training Set.csv.xlsx")
View(housingTrain)

housingTest <- read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/Housing Testing Set.csv.xlsx")
View(housingTest)

#linear model function 
reg01<-lm(SalePrice~LotArea, data=housingTrain)
plot(reg01)
summary(reg01)

#add factors
reg02<-lm(SalePrice~LotArea+as.factor(Fireplaces)+OverallQual, data=housingTrain)
summary(reg02)

reg03<-lm(SalePrice~LotFrontage + as.factor(Foundation) + as.factor(BsmtQual) + as.factor(BsmtCond) + as.factor(BsmtExposure) + TotalBsmtSF + GarageArea + as.factor(GarageQual), data=housingTrain)
plot(reg03)
summary(reg03)

#predict stuff
prd01<-predict(reg01,data.frame(LotArea=c(1000,50)), interval="confidence")
prd01

prd02<-predict(reg01, housingTest, interval="confidence")
prd02

#doesn't work due to unexpected vals
prd03<-predict(reg03, housingTest, interval="confidence")

install.packages('forecast')
install.packages('TTR')

library(readr)
library(forecast)
library(TTR)

#data obtained from investing.com
usdCad<-read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/USD CAD Historical Data.csv.xlsx")
summary(usdCad)
View(usdCad)


#declare time series and plot
usdCad$Price<-ts(usdCad$Price, start=c(2006,01,01),frequency=52)
plot(usdCad$Price, main="USD/CAD", xlab="Date", ylab="Exchange Rate",col="green")
exDecomp<-decompose(usdCad$Price)
plot(exDecomp)

#aggregation of seasons by year on the same plot
seasonplot(usdCad$Price)
tsdisplay(usdCad$Price)

#seasonal decomposition using another function
plot(stl(usdCad$Price, s.window="period"))

#moving averages
usdcad_sma_10<-SMA(usdCad$Price, 10)
usdcad_sma_25<-SMA(usdCad$Price, 25)
usdcad_sma_100<-SMA(usdCad$Price, 100)

plot(usdCad$Price)
plot(usdcad_sma_10, type='l')
plot(usdcad_sma_25, type='l')
plot(usdcad_sma_100, type='l')

#forecasting
usdcad_sma_forecast<-forecast(usdcad_sma_10, 10)
plot(usdcad_sma_forecast)

#changing the timeline affects the results!
usdcad_sma_forecast2<-forecast(usdcad_sma_100, 100)
plot(usdcad_sma_forecast2)

#verify dat
usdCad2017=read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/usdcad_2017.csv.xlsx")
plot(usdcad_sma_forecast2$mean)
par(new=1)
plot(usdCad2017$Price[1:16], col='red', type='l')

#autoregression model (better for non-stationary predictions, i.e. random processes)
usdcad_AR<-ar.ols(usdCad$Price, order.max=202)
usdcad_AR
summary(usdcad_AR)
usdcad_ar_forecast<-forecast(usdcad_AR,17)
plot(usdcad_ar_forecast)#blue region indicates prediction

#quick sanity check
plot(usdcad_ar_forecast$mean)
par(new=TRUE)
plot(usdCad2017$Price[1:17], col='red', type='l')

#Let's take a look at some oil price data
oily=read_excel("J:/Projects/Professional/Autodidactics/Data Analytics AARL/Part02/Crude Oil WTI Futures Historical Data.csv.xlsx")
oily$Price<-ts(oily$Price, start=c(2006,01,01),frequency = 52)
plot(oily$Price, type="l")


#install.packages("vars")#Vector Auto Regression (generalization of AR w/ >1 variable)
library(vars)
endo<-data.frame(oily$Price, usdCad$Price)#endogenous analysis (internal)
var1<-VAR(y=endo, p=5)
summary(var1)

var1_predict<-predict(var1, n.ahead=17)#attempt to look at the near future
plot(var1_predict$fcst$usdCad.Price[,1],type='l')

par(new=TRUE)
plot(usdCad2017$Price[1:17], col='green', type='l')#verify


