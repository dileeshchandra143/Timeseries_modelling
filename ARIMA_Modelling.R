

#loading and partitioning the datasets
az_data = AZ_sales[1:260,]
train_az = az_data[1:244,]
test_az = az_data[245:260,]

# Libararies
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(urca)

#checking the start and end frequency
start(train_az$SALES_PH)
end(train_az$SALES_PH)
frequency(train_az$SALES_PH)


# creating time series and plotting them
sales_ph = ts(train_az$SALES_PH, start = 1, end = 244)
plot(sales_ph)

sales_tu = ts(train_az$SALES_TU, start = 1, end = 244)
plot(sales_tu)

Test.ph = ts(test_az$SALES_PH, start = 245)
plot(Test.ph)

Test.tu = ts(test_az$SALES_TU, start = 245)
plot(Test.tu)

Full_ph = ts(az_data$SALES_PH, start = 1, end = 260)
plot(Full_ph)

Full_tu = ts(az_data$SALES_TU, start = 1)
plot(Full_tu)


# ESM

# Building a Linear Exponential Smoothing Model - Pheonix
LES.ph <- holt(sales_ph, initial = "optimal", h = 16)
summary(LES.ph)

plot(LES.ph, main = "Ph sales with Linear ESM Forecast", xlab = "Date", ylab = "")


# Building the damped trend model - Tuscan
LES.tu <- holt(sales_tu, initial = "optimal", h = 16, damped = TRUE)
summary(LES.tu)

plot(LES.tu, main = "Ph sales with Linear ESM Forecast-damped", xlab = "Date", ylab = "")


#MAPE calculation - ESM models
accuracy(LES.ph, Test.ph)
accuracy(LES.tu, Test.tu)


# stationarity through plots

####### -Phoenix

ndiffs(sales_ph)

plot.ts(sales_ph)
acf(sales_ph)
pacf(sales_ph)

plot.ts(diff(sales_ph), main = "Plot of the differenced series (Phoenix Sales)", ylab = "Phoenix Sales", xlab = "Weeks")
acf(diff(sales_ph))
pacf(diff(sales_ph))

####### -Tuscon

ndiffs(sales_tu)

plot.ts(sales_tu)
acf(sales_tu)
pacf(sales_tu)

plot.ts(diff(sales_tu))
acf(diff(sales_tu))
pacf(diff(sales_tu))

#### using the urca package for ADF testing ####

df.tu = ur.df(sales_tu, type = "trend", lags = 9, selectlags = "AIC")
summary(df.tu)



##########################

# Building an Autoregressive Model - Pheonix #
AR.Model_ph <- Arima(sales_ph, order = c(1, 1, 0))
summary(AR.Model_ph)

#ACF and PACF plots of residuals
Acf(AR.Model_ph$residuals, main = "")$acf
Pacf(AR.Model_ph$residuals, main = "")$acf

#white noise tests
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(AR.Model_ph$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}
White.LB

# white noise plots
White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values (Phoenix Sales)", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# forecast - Pheonix
Ph.forecast = forecast(AR.Model_ph, h = 16)
plot(Ph.forecast, main="Weekly Sales Forecast for Phoenix from 5/22/2017 to 9/4/2017", xlab="Weeks", ylab="Weekly Sales")

# predict function (works the same as the forecast function)
pred = predict(AR.Model_ph, n.ahead=16)
ts.plot(sales_ph, pred$pred, lty = c(1,3))

# Mape on the validation set - Pheonix
accuracy(Ph.forecast, Test.ph)
accuracy(LES.ph, Test.ph)



##########################

# Building an Autoregressive Model - Tuscon (deterministics trend- fit lin reg)
model = arima(sales_tu, order = c(0,0,0), xreg = c(1:244))
plot(model$residuals, main = "Plot of the differenced series (Tuscan Sales)", ylab = "Tuscan Sales", xlab = "Weeks")

model2 = arima(sales_tu, order = c(1,0,2), xreg = c(1:244))
f = fitted( arima(sales_tu, order = c(1,0,2), xreg = c(1:244)))
summary(model)
plot(f)
f_f = forecast(f, h=16)

plot(f_f)

#ACF and PACF plots of residuals
Acf(model2$residuals, main = "")$acf
Pacf(model2$residuals, main = "")$acf

# white noise test
White.LB_model <- rep(NA, 10)
for(i in 1:10){
  White.LB_model[i] <- Box.test(model2$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}
White.LB_model

# white noise plots
White.LB_model <- pmin(White.LB_model, 0.2)
barplot(White.LB_model, main = "Ljung-Box Test P-values (Tucson Sales)", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# forecast - Tuscon- deterministic 
forecast.tu = forecast(model2, h=16)

pred_model = predict(model2, n.ahead = 16, newxreg = c(245:260))
ts.plot(sales_tu, pred_model$pred, lty = c(1,3))

pred_model$pred

a = as.data.frame(pred_model$pred)

a.ts = ts(a, start = 245, end = 260)

# Mape on the validation set - Tuscon- deterministic 

accuracy(a.ts, Test.tu)
accuracy(LES.tu, Test.tu)


