install.packages('tseries')
install.packages('forecast')

library(tseries)
library(forecast)

setwd("D:/Google Drive/Work/SA/Training/Resources/R Training/Time series 2")
calls <- read.csv("data_for_forecasting.csv")
View(calls)
#step1 plot series
plot(calls$callsoffered, type="l")

#build baseline
forecast1 <- meanf(calls$callsoffered,3)
forecast2 <- naive(calls$callsoffered,3)

summary(forecast1)
summary(forecast2)

accuracy(forecast1,c(4573,3632,2976))
accuracy(forecast2,c(4573,3632,2976))

#define interval of forecast 
#For 80%- predvalue +- (1.28*stddev of residuals)
#For 95%- predvalue +- (1.96*stddev of residuals)
calls2 <- ts(calls$callsoffered,start=1995,freq=12)
plot(stl(calls2, s.window="periodic"))

#univariate seasonal forecasting using arima 
fit <- auto.arima(calls$callsoffered, seasonal = TRUE)
summary(fit)
accuracy(forecast(fit))
plot(forecast(fit,h=10),include=80)

fit <- Arima(calls$callsoffered,order=c(5,1,5))
summary(fit)

fit <- Arima(calls$callsoffered,order=c(5,1,5),seasonal = list(order = c(1,1,0),period=7))
summary(fit)
plot(forecast(fit,h=10),include=80)
#do not get bothered by the 'unable to fit' message as it just means that the algorithm based on Hyndman and Khandakar algorithm tires several combination of orders and one of them might not be converging to a solution
#drift is similar to having a constant in the equation. Here it signifies that after the diffrencing the mean of series is not 0 so drift adjusts for it.

#run dynamic regression models
#build a few effects
calls[,"dow"] <-  weekdays(calls$nwDt)
calls[,"mon_fl"] <- ifelse(calls[,"dow"] == "Monday" , 1,0)
#use them in arima
fit <- Arima(calls$callsoffered, xreg=calls$mon_fl,order=c(5,1,5))
summary(fit)



