RNGversion(vstr = 3.6)

library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
getwd()
setwd('/Users/shashank/Desktop/Semester 2/Frameworks 2/Week 10')
goog = readRDS('goog.RDS')

#section 1

#q1
autoplot(goog)
head(goog)
class(goog)
#a1: xts

#q2
goog["2010-06"]
#a2: 221.0374

#q3
mean(goog["2010",1])
#a3: 260.9768

#q4
nrow(goog)
#a4: 142

#q5
cor(goog, lag(goog), use = 'complete.obs')
#a5: 0.9923893

#q6
google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))
nrow(train)
#a6: 108

#q7
ggAcf(train)
#a7:lag 1 has strongest autocorrelation

###################

#section 2
#q1
average_model = meanf(train,h = 34)
average_model
autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)
window(average_model$mean,c(2018,10))
#a1: 355.776 (on train not test)

#q2
accuracy(average_model)
#a2: 144.5429

#q3
accuracy(average_model, x =google)
#a3: 588.3496

#q4
naive_model = naive(train,h=34)
naive_model
autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(test)
window(naive_model$mean,c(2018,10))
#a4: 758.88

#q5
accuracy(naive_model, x =google)
#a5: 230.50860

###################

#section 3
#q1
ets_aaa = ets(train,model = 'AAA')
ets_aaa
#a1: Additive

#q2
#a2: Additive

#q3
#a3: AICc = 1255.624

#q4
acf(x = google,lag.max = 1,plot=F)
ggAcf(x = google)
checkresiduals(ets_aaa)
#a4: p-value significant

#q5
ets_aaa_forecast = forecast(ets_aaa,h=34)
ets_aaa_forecast
window(ets_aaa_forecast$mean,c(2018,10))
#a5: 1028.494

#q6
accuracy(ets_aaa_forecast,x = google)
autoplot(train)+
  autolayer(ets_aaa_forecast,series="ETS - AAA",PI=F)+
  autolayer(test)
#a6: 102.20154


###################

#section 4
#q1
auto_arima_model = auto.arima(train)
auto_arima_model

autoplot(train)
#a1: ARIMA(0,1,0)  1st 0 is the number of autoregressive lag variables

#a2: 1 is the differencing 

#a3: 0 is the number of ordinary lags

#a4: 0

#q5
acf(x = google,lag.max = 1,plot=F)
ggAcf(x = google)
checkresiduals(auto_arima_model)
#a5: p value insignificant hence residual white noise

#q6
auto_arima_model_forecast = forecast(auto_arima_model,h=34)
auto_arima_model_forecast
#a6: 920.8568

#q7
accuracy(auto_arima_model_forecast, x =google)
#a7: 143.69172 

#q8
BoxCox.lambda(train)
#a8: 0.4748787

#q9
arima_model = Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda= BoxCox.lambda(train))
arima_model
#a9: 343.16


#q10
checkresiduals(arima_model, lag =24)
#a10: p value insignificant hence residual white noise

#q11
arima_model_forecast = forecast(arima_model,h=34)
arima_model_forecast
#a6: 1165.1741

#q12
accuracy(arima_model_forecast, x =google)
#a12: 56.19252
