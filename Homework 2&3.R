library(data.table)
library(zoo)
library(xts)
library(forecast)
library(stats)
elec <- fread("/Users/nevalgunal/Downloads/RealTimeConsumption.csv", skip=0) #import data
date_elec <- as.Date.character(elec$Date, format="%d.%m.%Y", tryFormats = "%Y-%m-%d")
elec$Date <- date_elec
rename_elec <- c("date", "hour", "cons")
setnames(elec, names(elec), rename_elec)
elec$cons <- as.numeric(gsub(",", "", elec$cons))
elec$cons <- elec$cons/1000
elec[,hour:=NULL]
setDF(elec)
str(elec)
head(elec)

#decompose the series at different levels
elec_hourly<- xts(elec$cons,  order.by = date_elec)
elec_hourly_ts <- ts(elec_hourly)
plot.xts(elec_hourly, main="Hourly Electric Consumption in GWh")
acf(elec_hourly_ts, main="ACF Values for Different Lags in Hourly Electric Consumption Data")

elec_daily <- apply.daily(elec_hourly, FUN = "sum")
elec_daily_ts <-ts(elec_daily)
plot.xts(elec_daily, main="Daily Electric Consumption in GWh")
acf(elec_daily_ts, lag.max = 30, main="ACF Values for Different Lags in Daily Electric Consumption Data")

elec_weekly<- apply.weekly(elec_daily, FUN = "sum")
elec_weekly_ts <- ts(elec_weekly)
plot.xts(elec_weekly,main="Weekly Electric Consumption in GWh")
acf(elec_weekly_ts, lag.max = 70, main="ACF Values for Different Lags in Weekly Electric Consumption Data")

elec_monthly <- apply.monthly(elec_daily, FUN="sum")
elec_monthly_ts <- ts(elec_monthly)
plot.xts(elec_monthly, main="Monthly Electric Consumption in Gwh")
acf(elec_monthly_ts, lag.max = 30, main="ACF Values for Different Lags in Monthly Electric Consumption Data")

elec_quarterly <- apply.quarterly(elec_daily, FUN = "sum")
elec_quarterly_ts <- ts(elec_quarterly)
plot.xts(elec_quarterly, main="Quarterly Electric Consumption in GWh")
acf(elec_quarterly_ts, lag.max = 10, main="ACF Values for Different Lags in Quarterly Electric Consumption Data")

elec_yearly <- apply.yearly(elec_daily, FUN = "sum")
elec_yearly_ts <- ts(elec_yearly)
plot.xts(elec_yearly, main="Yearly Electric Consumption in GWh")
acf(elec_yearly_ts, main="ACF Values for Different Lags in Yearly Electric Consumption Data")


elec_df <- setDF(data.table(elec$cons))
elec_df
parameter_yearly <- as.numeric(as.Date("2020-04-26") - as.Date("2016-01-01"))
parameter_freq <- as.integer((parameter_yearly*168)/360)
print(parameter_freq)
elec_frequency <- ts(elec_df,frequency = 168, start = c(2016,0), end = c(2016,735))
plot.ts(elec_hourly_ts, main="Electric Consumption Data with 168 Frequency",ylab="Gwh")

#seasonplot
seasonplot(elec_frequency,ylab="Gwh", xaxt="n",
           main="Electric Consumption Data for Different Years", labelgap = 4, 
           year.labels=TRUE, year.labels.left=TRUE, col=2:6, pch=18)

# monthplot
monthplot(elec_monthly,ylab="Gwh",xlab="Month",xaxt="n",
          main="Average Values of Electric Consumption Data for Different Months")
axis(1,at=1:12,labels=month.abb,cex=0.8)

#decompose
elec_dec_multip<-decompose(elec_frequency,type="multiplicative")
plot(elec_dec_multip)
ggAcf(elec_dec_multip$random, main="ACF Values of Random Part of Multiplicative Decomposition")

elec_dec_additive<-decompose(elec_frequency,type="additive")
plot(elec_dec_additive)
ggAcf(elec_dec_additive$random, main="ACF Values of Random Part of Additive Decomposition")

#further check
test1 <- Box.test(elec_dec_multip$random)
print(test1) # p is very small, less than 0.05. So it is a white noise.
test2 <- Box.test(elec_dec_additive$random)
print(test2) # again, p is very small, less than 0.05. So it is a white noise.

#deseasonalize
elec_deseasonalized <- elec_frequency/elec_dec_multip$seasonal
ts.plot(elec_deseasonalized, col=topo.colors(10), ylab="Gwh", main="Deseasonilized Electricity Consumption in Gwh")
acf(elec_deseasonalized)
#there is still

#detrend
detrend <- elec_deseasonalized/elec_dec_multip$trend
ts.plot(detrend, col=topo.colors(10), ylab="Gwh", main="Deseasonilized and Detrended Electricity Consumption in Gwh")
acf(detrend, na.action = na.pass)
plot(elec_dec_multip$random, main="Random Noise of Electric Consumption Data", ylab="Random noise")

ts.plot(elec_frequency, xlab = "Year", ylab = "Gwh",main="Electric Consumption in GWh")
model_ar1 <- arima(elec_frequency, order=c(1,0,0))
ar1_aic <- AIC(model_ar1)
ar1_bic <- BIC(model_ar1)
model_ar2 <- arima(elec_frequency, order=c(2,0,0))
ar2_aic <- AIC(model_ar2)
ar2_bic <- BIC(model_ar2)
model_ar3 <- arima(elec_frequency, order=c(3,0,0))
ar3_aic <- AIC(model_ar3)
ar3_bic <- BIC(model_ar3)

print(cbind(ar1_aic, ar1_bic))
print(cbind(ar2_aic, ar2_bic))
print(cbind(ar3_aic, ar3_bic))
#second is better


model_ma1 <- arima(elec_frequency, order=c(0,0,1))
ma1_aic <-AIC(model_ma1)
ma1_bic <-BIC(model_ma1)
model_ma2 <- arima(elec_frequency, order=c(0,0,2))
ma2_aic <-AIC(model_ma2)
ma2_bic <-BIC(model_ma2)
model_ma3 <- arima(elec_frequency, order=c(0,0,3))
ma3_aic <-AIC(model_ma3)
ma3_bic <-BIC(model_ma3)

print(cbind(ma1_aic,ma1_bic))
print(cbind(ma2_aic,ma2_bic))
print(cbind(ma3_aic,ma3_bic))
#third is better

#ar or ma
print(cbind(ar1_aic, ar1_bic))
print(cbind(ma3_aic,ma3_bic))
#model_ma3 is better

model_ma3 <- arima(elec_frequency, order=c(0,0,3))
print(model_ma3)
model_fitted_ma3 <- elec_frequency - residuals(model_ma3)
elec_fig <- window(elec_frequency, start=c(2020))
ts.plot(elec_fig, xlab = "Year", ylab = "Gwh",main="Electricity Consumption in Gwh")
points(model_fitted_ma3, type = "l", col = 4, lty = 6)
model_fc_ar1 <- predict(model_ma3, n.ahead = 24)
print(model_fc_ar1)
model_fc <- predict(model_ma3, n.ahead = 24)$pred
model_fc_se <- predict(model_ma3, n.ahead = 24)$se
points(model_fc, type = "l", col = 2)
points(model_fc - 1.96*model_fc_se, type = "l", col = 2, lty = 2)
points(model_fc + 1.96*model_fc_se, type = "l", col = 2, lty = 2)

