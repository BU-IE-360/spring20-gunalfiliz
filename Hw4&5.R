library(data.table)
library(zoo)
library(xts)
library(forecast)
library(stats)
library(lubridate)
library(corrplot)
library(dplyr)

# import data
traf <- fread("/Users/nevalgunal/Downloads/Metro_Interstate_Traffic_Volume.csv")
rename_traf <- c("holi", "temp", "rain", "snow", "clouds", "main", "desc", "time", "vol")
setnames(traf, names(traf), rename_traf)
summary(traf)
#numeric
traf$temp <- as.numeric(traf$temp)
traf$rain <- as.numeric(traf$rain)
traf$snow <- as.numeric(traf$snow)
traf$clouds <- as.numeric(traf$clouds)
traf$vol <- as.numeric(traf$vol)
traf$time <- as.POSIXct(traf$time, format="%Y-%m-%d %H:%M:%S")
#there are some duplicates
plot(traf$time, traf$vol)
traf_dup = !duplicated(traf$time)
traf_notdup=traf[traf_dup,]
#get in order
traf_notdup=traf_notdup[order(time)]
# add the missing data between 2014-08-08 01:00:00 and 
#2015-06-11 20:00:00
add_traf<- traf_notdup[time<"2014-06-11 20:00:00"]
add_traf<- add_traf[time>"2013-08-08 01:00:00"]
add_time<- as.POSIXct(add_traf$time)
add_time<- add_time %m+% years(1)
add_traf$time<-add_time
traf_part1 <- traf_notdup[time<="2014-08-08 01:00:00"]
traf_part3 <- traf_notdup[time>="2015-06-11 20:00:00"]
traf_total<- rbind(traf_part1, add_traf, traf_part3)

# add missing data with the data of previous hour
traf_loop<-traf_total
for (i in 1:nrow(traf_loop)+1){
  if (traf_loop$time[i]-hours(1) != traf_loop$time[i-1]){
    add_new_data<-traf_loop[i-1] 
    add_new_data$time<-add_new_data$time+hours(1)
    part1<-traf_loop[time<traf_loop$time[i]]
    part2<-traf_loop[time>=traf_loop$time[i]]
    traf_loop <- rbind(part1,add_new_data,part2)
  }
}


# add lagged variables for future work
traf_frame<- as.data.table(traf_loop)
temp_lagged<-lag(traf_frame$temp, k=2)
temp_lagged[1]<-traf_frame$temp[1]
holi_lagged<-lag(traf_frame$holi, k=2)
holi_lagged[1]<-traf_frame$holi[1]
rain_lagged<-lag(traf_frame$rain, k=2)
rain_lagged[1]<-traf_frame$rain[1]
snow_lagged<-lag(traf_frame$snow, k=2)
snow_lagged[1]<-traf_frame$snow[1]
clouds_lagged<-lag(traf_frame$clouds, k=2)
clouds_lagged[1]<-traf_frame$clouds[1]
vol_lagged<-lag(as.vector(traf_frame$vol), k=2)
vol_lagged[1]<-as.vector(traf_frame$vol)[1]
traf_frame<-cbind(traf_frame,temp_lagged, holi_lagged,rain_lagged,snow_lagged,clouds_lagged,vol_lagged)


# holi_index<-NULL
# holi_index<-data.frame(holi_index)
# for (i in 1:nrow(traf_frame)){
#   if (traf_frame$holi[i]=="None"){
#     bool=0
#     holi_index=rbind(holi_index, bool)
#   }else{
#     bool=1
#     holi_index=rbind(holi_index, bool)
#   }
# }
# setnames(holi_index, names(holi_index), c("holi_ind"))
# 
# holi_index_lagged<-NULL
# holi_index_lagged<-data.frame(holi_index_lagged)
# for (i in 1:nrow(traf_frame)){
#   if (traf_frame$holi_lagged[i]=="None"){
#     bool_lagged=0
#     holi_index_lagged=rbind(holi_index_lagged, bool_lagged)
#   }else{
#     bool_lagged=1
#     holi_index_lagged=rbind(holi_index_lagged, bool_lagged)
#   }
# }
# setnames(holi_index_lagged, names(holi_index_lagged), c("holi_lag_ind"))
# #adding indexed holi columns
# traf_frame<-cbind(traf_frame,holi_index,holi_index_lagged)

#check correlation for future work
c_t<-cor(traf_frame$vol, traf_frame$temp)
c_r<-cor(traf_frame$vol, traf_frame$rain)
c_s<-cor(traf_frame$vol, traf_frame$snow)
c_c<-cor(traf_frame$vol, traf_frame$clouds)
c_t_lag<-cor(traf_frame$vol, traf_frame$temp_lagged)
c_r_lag<-cor(traf_frame$vol, traf_frame$rain_lagged)
c_s_lag<-cor(traf_frame$vol, traf_frame$snow_lagged)
c_c_lag<-cor(traf_frame$vol, traf_frame$clouds_lagged)
cor_vol<-cbind(c_t,c_r,c_s,c_c,c_t_lag,c_r_lag,c_s_lag,c_c_lag)
corrplot(cor_vol)
#no significant correlation

#decompose the series at different levels to find the frequency of the data
traf_hourly_xts<-xts(traf_loop$vol, order.by = traf_loop$time)
traf_hourly_ts<-ts(traf_hourly_xts)
plot.xts(traf_hourly_xts, main="Hourly Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE, col=topo.colors(5))
acf(traf_hourly_ts, main="ACF Values for Hourly Traffic Volume Data", col=topo.colors(5))
# it gives negative relation in every 12 hours and positive relation in every 24 hours
traf_daily_xts<-apply.daily(traf_hourly_xts, FUN="sum")
traf_daily_ts<-ts(traf_daily_xts)
plot.xts(traf_daily_xts, main="Daily Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE,  cex.axis=0.55, col=topo.colors(5))
acf(traf_daily_ts,main="ACF Values for Daily Traffic Volume Data", col=topo.colors(5))
# it gives positive relation in every 7 days
traf_weekly_xts<-apply.weekly(traf_daily_xts, FUN="sum")
traf_weekly_ts<-ts(traf_weekly_xts)
plot.xts(traf_weekly_xts, main="Weekly Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE,  cex.axis=0.55, col=topo.colors(5))
acf(traf_weekly_ts,main="ACF Values for Weekly Traffic Volume Data", lag.max = 108, col=topo.colors(5))
# no significant relation(no pattern)
traf_monthly_xts<-apply.monthly(traf_weekly_xts, FUN="sum")
traf_monthly_ts<-ts(traf_monthly_xts)
plot.xts(traf_monthly_xts, main="Monthly Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE,  cex.axis=0.55, col=topo.colors(5))
acf(traf_monthly_ts,main="ACF Values for Monthly Traffic Volume Data", lag.max = 100, col=topo.colors(5))
#no significant relation
traf_quarterly_xts<-apply.quarterly(traf_weekly_xts, FUN="sum")
traf_quarterly_ts<-ts(traf_quarterly_xts)
plot.xts(traf_quarterly_xts, main="Quarterly Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE,  cex.axis=0.55, col=topo.colors(5))
acf(traf_quarterly_ts,main="ACF Values for Quarterly Traffic Volume Data", lag.max = 100, col=topo.colors(5))
#no significant relation
traf_yearly_xts<-apply.yearly(traf_weekly_xts, FUN="sum")
traf_yearly_ts<-ts(traf_yearly_xts)
plot.xts(traf_yearly_xts, main="Yearly Traffic Volume", xlab="Time", ylab="Traffic Volume", yaxis.right = FALSE,  cex.axis=0.55, type="o", col=topo.colors(5))
acf(traf_yearly_ts,main="ACF Values for Yearly Traffic Volume Data", lag.max = 100, col=topo.colors(5))
#no significant relation
# it is found that frequency is 24*7=168
traf_with_freq<-ts(traf_loop$vol, frequency=168)
plot(traf_with_freq, main="Traffic Volume Data with 168 Frequency",ylab="Traffic Volume", col=topo.colors(5))
# monthplot
monthplot(traf_monthly_xts,ylab="Traffic Volume",xlab="Month",xaxt="n",
          main="Average Values of Traffic Volume Data for Different Months", col=topo.colors(5))
axis(1,at=1:12,labels=month.abb,cex=0.8)


# create test and train data
train_traf=traf_loop[time<"2018-01-01 00:00:00"]
summary(train_traf)
test_traf=traf_loop[time>"2017-12-31 23:00:00"]
test_traf=test_traf[time<"2019-01-01 00:00:00"]
summary(test_traf)

# create zoo series
train_traf<-train_traf[order(time)]
traf_series<-zoo(train_traf[,list(vol,temp,rain,snow,clouds)], train_traf$time)
plot(traf_series, pch=5, cex=0.3, col=topo.colors(5), main = "Traffic Data Zoo Series")

# observe the trend in train data with LR model
for_trend <- train_traf[, list(time, vol)]
for_trend[, time_index:=1:.N]
for_trend
trend_lr <- lm(vol~time_index, for_trend)
summary(trend_lr)
for_trend[, lr_trend:=trend_lr$fitted.values]
matplot(for_trend[,list(vol, lr_trend)], type = "l")
# very little increase in volume over time

# use moving average approach
window_size=168 # buna frequencye göre karar vermemiz gerekiyor
for_trend[, ma_trend:=frollmean(vol, 2*window_size+1, align = "center")]
head(for_trend,30)
matplot(for_trend[,list(vol, lr_trend, ma_trend)], type = "l")
# plot
vol_with_trend=zoo(for_trend[, list(vol, lr_trend, ma_trend)], for_trend$time)
plot(vol_with_trend)

# Seasonality
for_trend[, detrend_lr_traf:=vol-lr_trend]
for_trend[, detrend_ma_traf:=vol-ma_trend]
head(for_trend)
for_trend$Date <- as.Date(for_trend$time)
for_trend$Time <- format(as.POSIXct(for_trend$time) ,format = "%H:%M:%S")
for_trend[,hour:=hour(time)]
for_trend[, hour_effect_ma:=mean(detrend_ma_traf, na.rm=TRUE), by=list(hour)]
#plot(for_trend[1:48]$hour_effect_lr,type="l")
vol1=for_trend[,list(time,vol,ma_trend,hour_effect_ma)]
vol1=vol1[168*10+1:168*10+24*14]
vol_with_trend_seasonality=zoo(vol1[,list(vol,ma_trend,hour_effect_ma)], vol1$time)
plot(vol_with_trend_seasonality)
for_trend[,weekday:=weekdays(time)]
for_trend[,daily_hour_effect_ma:=mean(detrend_ma_traf, na.rm=TRUE), by=list(weekday)]
for_trend[,residual_daily_hourly_ma:=detrend_ma_traf-daily_hour_effect_ma]
vol1=for_trend[,list(time,vol,ma_trend,daily_hour_effect_ma,residual_daily_hourly_ma)]
vol1=vol1[168*10+1:168*10+24*14]
vol_with_trend_seasonality=zoo(vol1[,list(vol,ma_trend, daily_hour_effect_ma, residual_daily_hourly_ma)], vol1$time)
plot(vol_with_trend_seasonality)

# by using categorical variable
lr_sea_decomp_ser=for_trend[, list(time, weekday, hour, vol, detrend_lr_traf, lr_trend)]
lr_sea_decomp_ser[, daily_hourly_effect_lr:=paste(weekday, hour, sep='-')] # a categorical predicter
lr_sea_decomp_ser[, time_index:=1:.N]
seas_lm=lm(detrend_lr_traf~-1+daily_hourly_effect_lr, lr_sea_decomp_ser) #without intercept
coef(seas_lm)
se=data.table(variable=attr(coef(seas_lm), "names"), value=as.numeric(coef(seas_lm)))
lr_sea_decomp_ser[, month:=as.factor(month(time))]
lr_sea_decomp_ser[,hour:=as.factor(hour)]
res<-lr_sea_decomp_ser$detrend_lr_traf-coef(se)
seas_lm2<-lm(detrend_lr_traf~-1+weekday*hour, lr_sea_decomp_ser) #multiple reg
summary(seas_lm2)

#modeling seasonality with Fourier Series(for continuity)
y=ts(lr_sea_decomp_ser$detrend_lr_traf, freq= 168)
plot(y)
f_series<-fourier(y, K=4)
str(f_series)
matplot(f_series[1:168, 1:2])
fit=lm(y~+f_series)


# # modeling data with MA
# model_ma=for_trend$detrend_ma_traf+for_trend$daily_hour_effect_ma+for_trend$residual_daily_hourly_ma
# checkresiduals(model_ma)
# model_lr=lr_sea_decomp_ser$detrend_lr_traf+f_series+res
# checkresiduals(model_lr)
# # I do not think that I can model the stationary data properly. Therefore, I do not continue with these models.

# #ARIMA
# traf_multip<-decompose(traf_with_freq, type="multiplicative")
# plot(traf_multip)
# ggAcf(traf_multip$random, main="ACF Values of Random Part of Multiplicative Decomposition")
# 
# traf_additive<-decompose(traf_with_freq,type="additive")
# plot(traf_additive)
# ggAcf(traf_additive$random, main="ACF Values of Random Part of Additive Decomposition")
# 
# #further check
# test1 <- Box.test(traf_multip$random)
# print(test1) # p is very small, less than 0.05. So it is a white noise.
# test2 <- Box.test(traf_additive$random)
# print(test2) # again, p is very small, less than 0.05. So it is a white noise.
# 
# #deseasonalize
# traf_deseasonalized <- traf_with_freq-traf_additive$seasonal
# ts.plot(traf_deseasonalized, col=topo.colors(10), ylab="Vol", main="Deseasonilized Traffic Volume")
# acf(traf_deseasonalized)
# #there is still trend
# 
# #detrend
# detrend <- traf_deseasonalized-traf_additive$trend
# ts.plot(detrend, col=topo.colors(10), ylab="Vol", main="Deseasonilized and Detrended Traffic Volume", cex=0.1)
# acf(detrend, na.action = na.pass)
# plot(traf_additive$random, main="Random Noise of Traffic Volume Data", ylab="Random noise")
# plot(detrend)

# Modeling Using Linear Regression
lr_sea_decomp_ser[, temp_lag:=traf_frame$temp_lag[1:45999]]
lr_sea_decomp_ser[, rain_lag:=traf_frame$rain_lag[1:45999]]
lr_sea_decomp_ser[, snow_lag:=traf_frame$snow_lag[1:45999]]
lr_sea_decomp_ser[, clouds_lag:=traf_frame$clouds_lag[1:45999]]
lr_sea_decomp_ser[, temp:=traf_frame$temp[1:45999]]
lr_sea_decomp_ser[, rain:=traf_frame$rain[1:45999]]
lr_sea_decomp_ser[, snow:=traf_frame$snow[1:45999]]
lr_sea_decomp_ser[, clouds:=traf_frame$clouds_lag[1:45999]]
lr_1=lm(vol~weekday+time_index+hour, data=lr_sea_decomp_ser)
summary(lr_1)
lr_2=lm(vol~weekday+time_index+hour+temp+rain+snow+clouds, data=lr_sea_decomp_ser)
summary(lr_2)
lr_3=lm(vol~weekday+time_index+hour+temp_lag+rain_lag+snow_lag+clouds_lag, data=lr_sea_decomp_ser)
summary(lr_3)
accuracy(lr_1)
accuracy(lr_2)
accuracy(lr_3)
#lr_2 is better

#ARIMA
arima_1=auto.arima(traf_frame$vol[1:1000])
summary(arima_1)
AIC(arima_1)
arima_2=auto.arima(traf_frame$vol[1:1000], xreg = c(traf_frame$temp,traf_frame$rain,traf_frame$clouds,traf_frame$snow)[1:1000])
summary(arima_2)
AIC(arima_2)
arima_3=auto.arima(traf_frame$vol_lagged[1:1000], xreg = c(traf_frame$temp_lagged,traf_frame$rain_lagged,traf_frame$clouds_lagged,traf_frame$snow_lagged)[1:1000])
summary(arima_3)
AIC(arima_3)
#Arima_3 is better

predict_arima<-forecast(arima_3, h=10, xreg=c(traf_frame$temp_lagged,traf_frame$rain_lagged,traf_frame$clouds_lagged,traf_frame$snow_lagged)[1:1000])
predict_lr<-predict(lr_3, newdata=lr_sea_decomp_ser, h=10)[1:10]
predict_final<-(predict_arima$mean+predict_lr[1:10])/2
