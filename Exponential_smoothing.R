getwd()
setwd("C:/Users/sanya/Desktop/Time Series/R_Time-Series/datasets")

train = read.csv(file='train_data.csv', header = TRUE)
test = read.csv(file='test_data.csv', header = TRUE)


# Needed Libraries for Analysis #
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)

# Creation of Time Series Data Object #
# ts(y-variable, start, frequency)
# Frequency- the level of granularity, monthly-12, quarterly-4... 
# the data must be sorted on time variable 
Temp.ts <- ts(train$TEMP, start = 1, frequency = 24)
plot(Temp.ts)

Test.ts <- ts(test$TEMP, start = 27, frequency = 24)
plot(Test.ts)


# Time Series Decomposition # 
# you must have the frequency specified in the above code if you want the decomposed model to give you seasons 
# s.window - to specify the span of seasons
# stl stands for Seasonal Decomposition of Time Series by Loess

model <- stl(Temp.ts, s.window = 5)
plot(model)
model


# plotting the time series data Temp.ts
# lwd is line width
plot(Temp.ts, col = "grey", main = "TEMPERATURE", xlab = "", ylab = "Hourly Temp", lwd = 3)

# fitting a trend line on the times series plot
lines(model$time.series[,2], col = "red", lwd = 2)

# calculating seasonally adjusted data, remove seasonality from data
seasonal_comp = Temp.ts-model$time.series[,1]

# plotting the time series data passenger
plot(Temp.ts, col = "grey", main = "US Airline Passengers - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)

# plotting the seasonally adjusted values on the time series plot
lines(seasonal_comp, col = "red", lwd = 2)


# Building a Single Exponential Smoothing Model - Steel Data #

Temp.ses = ses(Temp.ts, initial = "optimal", h= 24)
summary(Temp.ses)

plot(Temp.ses, main = "Temp forecast with Simple ESM Forecast", xlab = "Date", ylab = "Temp-hourly")
abline(v = 26, col = "red", lty = "dashed")


# Ljung-Box Test for Steel ES Model #

SES.acf = acf(Temp.HWES$residuals, lag.max = 20)
SES.acf = acf(Temp.LDES$residuals, lag.max = 20)
SES.acf = acf(Temp.LES$residuals, lag.max = 20)
SES.acf = acf(Temp.ses$residuals, lag.max = 20)


Box.test(Temp.HWES$residuals, lag = 20, type = "Ljung")
Box.test(Temp.LDES$residuals, lag = 20, type = "Ljung")
Box.test(Temp.LES$residuals, lag = 20, type = "Ljung")
Box.test(Temp.ses$residuals, lag = 20, type = "Ljung")


plot.ts(Temp.HWES$residuals)
plot.ts(Temp.LDES$residuals)
plot.ts(Temp.LES$residuals)
plot.ts(Temp.ses$residuals)


White.LB <- rep(NA, 10)

for(i in 1:10){
  White.LB[i] <- Box.test(Temp.HWES$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB

White.LB <- pmin(White.LB, 0.2)

barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


White.LB <- rep(NA, 10)

for(i in 1:10){
  White.LB[i] <- Box.test(Temp.LDES$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB

White.LB <- pmin(White.LB, 0.2)

barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


White.LB <- rep(NA, 10)

for(i in 1:10){
  White.LB[i] <- Box.test(Temp.LES$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB

White.LB <- pmin(White.LB, 0.2)

barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


White.LB <- rep(NA, 10)

for(i in 1:10){
  White.LB[i] <- Box.test(Temp.ses$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB

White.LB <- pmin(White.LB, 0.2)

barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")



# Building a Linear Exponential Smoothing Model - Steel Data #
Temp.LES <- holt(Temp.ts, initial = "optimal", h = 24)
summary(Temp.LES)

plot(Temp.LES, main = "Temp- Linear Exponential Smoothing", xlab = "Date", ylab = "Temp")
abline(v = 1992, col = "red", lty = "dashed")



# damped trend model
Temp.LDES <- holt(Temp.ts, initial = "optimal", h = 24, damped = TRUE)
summary(Temp.LDES)

plot(Temp.LDES, main = "Temp with Linear Damped ESM Forecast", xlab = "Date", ylab = " Temp- hourly")
abline(v = 26, col = "red", lty = "dashed")


# Building a Holt-Winters ESM #
Temp.HWES<- hw(Temp.ts, seasonal = "additive")
summary(Temp.HWES)

plot(Temp.HWES, main = "Temp with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Temp-hourly")
abline(v = 26, col = "red", lty = "dashed")


# Building a Holt-Winters ESM- multiplicative #
Temp.HWES.mul<- hw(Temp.ts, seasonal = "multiplicative")
summary(Temp.HWES.mul)

plot(Temp.HWES.mul, main = "Temp with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Temp-hourly")
abline(v = 26, col = "red", lty = "dashed")


#forecasted values 
Temp.HWES$fitted

#Checking the Accuracy statistics 
accuracy(Temp.HWES, Test.ts)
accuracy(Temp.LDES, Test.ts)
accuracy(Temp.LES, Test.ts)
accuracy(Temp.ses, Test.ts)
accuracy(Temp.HWES.mul, Test.ts)
