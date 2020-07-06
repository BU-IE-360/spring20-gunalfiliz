# FACIAL CLEANSER

yuz <- subset(data, product_content_id == "32939029")
yuz[yuz == -1] <- NA
yuz <- yuz[event_date<"2020-06-14"]
yuz <- yuz[event_date>"2019-11-22"]

yuz[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation
# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(yuz,h)
# yuz <- yuz[event_date>"2020-01-23"]
# yuz <- yuz[event_date>"2020-03-06"]

for (n in a:b) {
  
  yuz_cut <- yuz[1:(nrow(yuz) - n)] # creating the train data
  
  # Auto Arima yuz
  yuz_cut <- yuz_cut[event_date>"2020-01-23"]
  model1 <- auto.arima(yuz_cut$sold_count)
  predictions <- forecast(model1, h=2)$mean

  # Dynamic Regression Model 1    yuz
  dates<-as.Date(yuz_cut$event_date)
  yuz_sold_xts<-xts(yuz_cut$sold_count,order.by = dates)
  yuz_sold_ts<-ts(yuz_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(yuz_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(yuz_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(yuz_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(yuz_cut$price,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(yuz_cut$category_visits,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(yuz_cut$visit_count,yuz_cut$basket_count,yuz_cut$favored_count, yuz_cut$category_visits)
  arima_model_with_var <- auto.arima(yuz_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count, test_data$category_visits)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Dynamic Regression Model 2   yuz
  yuz_cut <- yuz_cut[event_date>"2020-03-06"]
  dates<-as.Date(yuz_cut$event_date)
  yuz_sold_xts<-xts(yuz_cut$sold_count,order.by = dates)
  yuz_sold_ts<-ts(yuz_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(yuz_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(yuz_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(yuz_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(yuz_cut$price,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(yuz_cut$category_visits,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(yuz_cut$price,yuz_cut$basket_count,yuz_cut$category_visits)
  arima_model_with_var <- auto.arima(yuz_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$price,test_data$basket_count,test_data$category_visits)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  
  # Convergence Rate    yuz
  yuz_cut <- yuz_cut[event_date>"2020-03-06"]
  test_data[,ratebas:=as.vector(naive(yuz_cut$ratebas,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(yuz_cut$basket_count,h=h)$mean)]
  predictions <- test_data$basket_count * test_data$ratebas

  # Ensemble
  # Calculate the mean of Dynamic Regression 2 and Convergence Rate's predictions
  predictions <- (predictions1+predictions2)/2
  
  allpred <- rbind(allpred, as.numeric(predictions)) # creating a matrix of all predictions
  
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

yuz_cut1 <- yuz[(nrow(yuz) - b + 1):(nrow(yuz) - a + 1)] # creating the test data for predictions of the 1st day
yuz_cut2 <- yuz[(nrow(yuz) - b + 2):(nrow(yuz) - a + 2)] # creating the test data for predictions of the 2nd day

pred_yuz <- allpred2
tru_yuz <- yuz_cut2$sold_count

mape1_yuz <- 100*(sum(abs(yuz_cut1$sold_count-allpred1))/ sum(yuz_cut1$sold_count))
mape2_yuz <- 100*(sum(abs(yuz_cut2$sold_count-allpred2))/ sum(yuz_cut2$sold_count))
mape1_yuz
mape2_yuz

# WET WIPES

mendil <- subset(data, product_content_id == "4066298")
mendil <- mendil[event_date<"2020-06-14"]

mendil[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation
# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(mendil,h)
mendil <- mendil[event_date>"2020-01-23"]
mendil[mendil == -1] <- NA

for (n in a:b) {
  mendil_cut <- mendil[1:(nrow(mendil) - n)] # creating the train data
  # Auto Arima mendil
  model1 <- auto.arima(mendil_cut$sold_count)
  predictions <- forecast(model1, h=2)$mean
  
  # Dynamic Regression Model 1    mendil
  dates<-as.Date(mendil_cut$event_date)
  mendil_sold_xts<-xts(mendil_cut$sold_count,order.by = dates)
  mendil_sold_ts<-ts(mendil_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(mendil_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(mendil_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(mendil_cut$favored_count,h=h)$mean)]
  #test_data[,price:=as.vector(naive(mendil_cut$price,h=h)$mean)]
  test_data[,category_sold:=as.vector(naive(mendil_cut$category_sold,h=h)$mean)]
  # test_data[,category_brand_sold:=as.vector(naive(mendil_cut$category_brand_sold,h=h)$mean)]
  # test_data[,category_visits:=as.vector(naive(mendil_cut$category_visits,h=h)$mean)]
  test_data[,ty_visits:=as.vector(naive(mendil_cut$ty_visits,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(mendil_cut$visit_count,mendil_cut$basket_count,mendil_cut$favored_count)
  arima_model_with_var <- auto.arima(mendil_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count)

  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Dynamic Regression Model 2   mendil
   
  dates<-as.Date(mendil_cut$event_date)
  mendil_sold_xts<-xts(mendil_cut$sold_count,order.by = dates)
  mendil_sold_ts<-ts(mendil_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  
  test_data[,visit_count:=as.vector(naive(mendil_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(mendil_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(mendil_cut$favored_count,h=h)$mean)]
  test_data[,category_sold:=as.vector(naive(mendil_cut$category_sold,h=h)$mean)]
  test_data[,ty_visits:=as.vector(naive(mendil_cut$ty_visits,h=h)$mean)]
  
  # Fitting ARIMA with Regressors
  
  xreg_data<-cbind(mendil_cut$basket_count,mendil_cut$visit_count)
  arima_model_with_var <- auto.arima(mendil_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$visit_count)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  # 
  # 
  # 
  
  #Convergence Rate    mendil
  test_data$ratebas  <- as.vector(naive(mendil_cut$ratebas,h=h)$mean)
  test_data[,ratebas:=as.vector(naive(mendil_cut$ratebas,h=h)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(mendil_cut$basket_count,
                                                         xreg= cbind(mendil_cut$favored_count,
                                                                     mendil_cut$visit_count,
                                                                     mendil_cut$category_visits)),h=h,
                                              xreg=cbind(test_data$favored_count,
                                                         test_data$visit_count, test_data$category_visits))$mean)]
  predictions <- test_data$basket_count * test_data$ratebas
  predictions <- (predictions1+predictions2)/2
  
  allpred <- rbind(allpred, as.numeric(predictions)) # creating a matrix of all predictions
  
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

mendil_cut1 <- mendil[(nrow(mendil) - b + 1):(nrow(mendil) - a + 1)] # creating the test data for predictions of the 1st day
mendil_cut2 <- mendil[(nrow(mendil) - b + 2):(nrow(mendil) - a + 2)] # creating the test data for predictions of the 2nd day

pred_mendil <- allpred2
tru_mendil <- mendil_cut2$sold_count


mape1_mendil <- 100*(sum(abs(mendil_cut1$sold_count-allpred1))/ sum(mendil_cut1$sold_count))
mape2_mendil <- 100*(sum(abs(mendil_cut2$sold_count-allpred2))/ sum(mendil_cut2$sold_count))
mape1_mendil
mape2_mendil

# EARPHONE

kulak <- subset(data, product_content_id == "6676673")
kulak <- kulak[event_date<"2020-06-14"]

kulak[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation

# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(kulak,h)
kulak <- kulak[event_date>"2020-01-23"]

for (n in a:b) {
  kulak_cut <- kulak[1:(nrow(kulak) - n)] # creating the train data
  
  # Auto Arima kulak
  model1 <- auto.arima(kulak_cut$sold_count)
  predictions <- forecast(model1, h=2)$mean
  
  # Dynamic Regression Model 1         
  
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(kulak_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(kulak_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(kulak_cut$favored_count,h=h)$mean)]
  
  # Forecasting regressors using auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(kulak_cut$basket_count,xreg=cbind(kulak_cut$visit_count,kulak_cut$favored_count)),h=h,xreg=cbind(test_data$visit_count,test_data$favored_count))$mean)]
  test_data[,favored_count:=as.vector(forecast(auto.arima(kulak_cut$favored_count,xreg=cbind(kulak_cut$basket_count)),h=h,xreg=cbind(test_data$basket_count))$mean)]
  test_data[,visit_count:=as.vector(forecast(auto.arima(kulak_cut$visit_count,xreg=cbind(kulak_cut$favored_count)),h=h,xreg=cbind(test_data$favored_count))$mean)]
  
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(kulak_cut$visit_count,kulak_cut$basket_count,kulak_cut$favored_count)
  arima_model_with_var <- auto.arima(kulak_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count)
  
  
  predictions1 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  allpred <-rbind(allpred,as.numeric(predictions1) )
  
  #Dynamic Regression Model 2
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(kulak_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(kulak_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(kulak_cut$favored_count,h=h)$mean)]
  
  
  # Forecasting regressors using auto arima with xreg
  test_data[,visit_count:=as.vector(forecast(auto.arima(kulak_cut$visit_count,xreg=kulak_cut$favored_count),h=h,xreg=test_data$favored_count)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(kulak_cut$basket_count,xreg=cbind(kulak_cut$favored_count,kulak_cut$visit_count)),h=h,xreg=cbind(test_data$favored_count,test_data$visit_count))$mean)]
  test_data[,favored_count:=as.vector(forecast(auto.arima(kulak_cut$favored_count,xreg=cbind(kulak_cut$basket_count)),h=h,xreg=cbind(test_data$basket_count))$mean)]
  
  
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(kulak_cut$visit_count,kulak_cut$basket_count,kulak_cut$favored_count)
  arima_model_with_var <- auto.arima(kulak_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count)
  
  predictions2 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  allpred <- rbind(allpred, as.numeric(predictions2)) # creating a matrix of all predictions
  
  #Convergence Rate
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(kulak_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(kulak_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(kulak_cut$favored_count,h=h)$mean)]
  
  #Forecasting Regressors Using auto.arima
  test_data[,ratebas:= as.vector(forecast(auto.arima(kulak_cut$ratebas),h=h)$mean)]
  
  # auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(kulak_cut$basket_count,xreg=cbind(kulak_cut$visit_count,kulak_cut$favored_count)),h=h,xreg=cbind(test_data$visit_count,test_data$favored_count))$mean)]
  
  #prediction by convergence rate
  predictions_bas =test_data$basket_count *test_data$ratebas
  allpred <-rbind(allpred,as.numeric(predictions_bas) )
  
  #Ensemble
  
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(kulak_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(kulak_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(kulak_cut$favored_count,h=h)$mean)]
  
  #Forecasting Regressors Using auto.arima
  test_data[,ratebas:= as.vector(forecast(auto.arima(kulak_cut$ratebas),h=h)$mean)]
  
  # auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(kulak_cut$basket_count,xreg=cbind(kulak_cut$visit_count,kulak_cut$favored_count)),h=h,xreg=cbind(test_data$visit_count,test_data$favored_count))$mean)]
  test_data[,favored_count:=as.vector(forecast(auto.arima(kulak_cut$favored_count,xreg=cbind(kulak_cut$basket_count)),h=h,xreg=cbind(test_data$basket_count))$mean)]
  test_data[,visit_count:=as.vector(forecast(auto.arima(kulak_cut$visit_count,xreg=cbind(kulak_cut$favored_count)),h=h,xreg=cbind(test_data$favored_count))$mean)]
  
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(kulak_cut$visit_count,kulak_cut$basket_count,kulak_cut$favored_count)
  arima_model_with_var <- auto.arima(kulak_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count)
  
  #predictions by ensembling convergence rate and dynamic regression 1
  predictions_bas =test_data$basket_count *test_data$ratebas
  predictions1 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  predictions <-(predictions_bas + as.numeric(predictions1))/2
  
  allpred <-rbind(allpred,as.numeric(predictions) )
  
  
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

kulak_cut1 <- kulak[(nrow(kulak) - b + 1):(nrow(kulak) - a + 1)] # creating the test data for predictions of the 1st day
kulak_cut2 <- kulak[(nrow(kulak) - b + 2):(nrow(kulak) - a + 2)] # creating the test data for predictions of the 2nd day

pred_kulak <- allpred2
tru_kulak <- kulak_cut2$sold_count

# MAPE Calculation
if( any(kulak_cut1$sold_count==0) | any(kulak_cut2$sold_count==0) |any(allpred1==0) | any(allpred2==0) ){
  allpred1 <- allpred1+1
  allpred2 <- allpred2+1
  kulak_cut1 <- kulak_cut1+1
  kulak_cut2 <- kulak_cut2+1
}
mape1_kulak <- 100*(sum(abs(kulak_cut1$sold_count-allpred1))/ sum(kulak_cut1$sold_count))
mape2_kulak <- 100*(sum(abs(kulak_cut2$sold_count-allpred2))/ sum(kulak_cut2$sold_count))
mape1_kulak
mape2_kulak

# VACUUM CLEANER

supur <- subset(data, product_content_id == "7061886")
supur <- supur[event_date<"2020-06-14"]
supur[supur == -1] <- NA


supur[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation
# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(supur,h)

supur <- supur[event_date>"2020-01-23"]

for (n in a:b) {
  
  supur_cut <- supur[1:(nrow(supur) - n)] # creating the train data
  
  # Auto Arima supur
  model1 <- auto.arima(supur_cut$sold_count)
  predictions1 <- forecast(model1, h=2)$mean
  
  # # Dynamic Regression Model 1-2    supur
  
  dates<-as.Date(supur_cut$event_date)
  supur_sold_xts<-xts(supur_cut$sold_count,order.by = dates)
  supur_sold_ts<-ts(supur_sold_xts,frequency = 1)
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(supur_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(supur_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(supur_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(supur_cut$price,h=h)$mean)]
  test_data[,category_sold:=as.vector(naive(supur_cut$category_sold,h=h)$mean)]
  test_data[,category_brand_sold:=as.vector(naive(supur_cut$category_brand_sold,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(supur_cut$category_visits,h=h)$mean)]
  test_data[,ty_visits:=as.vector(naive(supur_cut$ty_visits,h=h)$mean)]
  
  
  # Fitting ARIMA with Regressors 1
  xreg_data<-cbind(supur_cut$visit_count,supur_cut$basket_count,supur_cut$favored_count, supur_cut$price)
  arima_model_with_var <- auto.arima(supur_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count, test_data$price)
  predictions2 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Fitting ARIMA with Regressors 2
  xreg_data<-cbind(supur_cut$basket_count,supur_cut$favored_count, supur_cut$price)
  arima_model_with_var <- auto.arima(supur_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count, test_data$price)
  predictions3 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  
  # Convergence Rate    supur
  test_data$ratebas  <- as.vector(naive(supur_cut$ratebas,h=h)$mean)
  test_data[,ratebas:=as.vector(naive(supur_cut$ratebas,h=h)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(supur_cut$basket_count,
                                                         xreg= cbind(supur_cut$favored_count,
                                                                     supur_cut$visit_count,
                                                                     supur_cut$category_visits)),h=h,
                                              xreg=cbind(test_data$favored_count,
                                                         test_data$visit_count, test_data$category_visits))$mean)]
  predictions4 = test_data$basket_count * test_data$ratebas
  
  predictions5 <- (as.numeric(predictions4)+as.numeric(predictions1))/2
  
  predictions <- cbind(as.numeric(predictions1),as.numeric(predictions2),as.numeric(predictions3),as.numeric(predictions4),as.numeric(predictions5))
  
  allpred <- rbind(allpred, as.numeric(predictions)) # creating a matrix of all predictions
  
}

allpred1_arima <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2_arima <- rev(allpred[, 2]) # for predictions of the 2nd day
allpred1_dynamic_reg_1 <- rev(allpred[, 3]) # for predictions of the 1st day
allpred2_dynamic_reg_1 <- rev(allpred[, 4]) # for predictions of the 2nd day
allpred1_dynamic_reg_2 <- rev(allpred[, 5]) # for predictions of the 1st day
allpred2_dynamic_reg_2 <- rev(allpred[, 6]) # for predictions of the 2nd day
allpred1_convergence_rate <- rev(allpred[, 7]) # for predictions of the 1st day
allpred2_convergence_rate <- rev(allpred[, 8]) # for predictions of the 2nd day
allpred1_ensemble <- rev(allpred[, 9]) # for predictions of the 1st day
allpred2_ensemble <- rev(allpred[, 10]) # for predictions of the 2nd day

supur_cut1 <- supur[(nrow(supur) - b + 1):(nrow(supur) - a + 1)] # creating the test data for predictions of the 1st day
supur_cut2 <- supur[(nrow(supur) - b + 2):(nrow(supur) - a + 2)] # creating the test data for predictions of the 2nd day

pred_supur <- allpred2_dynamic_reg_1
tru_supur <- supur_cut2$sold_count

mape1_supur <- 100*(sum(abs(supur_cut1$sold_count-allpred1_arima))/ sum(supur_cut1$sold_count))
mape2_supur <- 100*(sum(abs(supur_cut2$sold_count-allpred2_arima))/ sum(supur_cut2$sold_count))
mape1_supur
mape2_supur
mape1_supur_dr1 <- 100*(sum(abs(supur_cut1$sold_count-allpred1_dynamic_reg_1))/ sum(supur_cut1$sold_count))
mape2_supur_dr1 <- 100*(sum(abs(supur_cut2$sold_count-allpred2_dynamic_reg_1))/ sum(supur_cut2$sold_count))
mape1_supur_dr1
mape2_supur_dr1
mape1_supur_dr2 <- 100*(sum(abs(supur_cut1$sold_count-allpred1_dynamic_reg_2))/ sum(supur_cut1$sold_count))
mape2_supur_dr2 <- 100*(sum(abs(supur_cut2$sold_count-allpred2_dynamic_reg_2))/ sum(supur_cut2$sold_count))
mape1_supur_dr2
mape2_supur_dr2
mape1_supur_con_rate <- 100*(sum(abs(supur_cut1$sold_count-allpred1_convergence_rate))/ sum(supur_cut1$sold_count))
mape2_supur_con_rate <- 100*(sum(abs(supur_cut2$sold_count-allpred2_convergence_rate))/ sum(supur_cut2$sold_count))
mape1_supur_con_rate
mape2_supur_con_rate
mape1_supur_ensemble <- 100*(sum(abs(supur_cut1$sold_count-allpred1_ensemble))/ sum(supur_cut1$sold_count))
mape2_supur_ensemble <- 100*(sum(abs(supur_cut2$sold_count-allpred2_ensemble))/ sum(supur_cut2$sold_count))
mape1_supur_ensemble
mape2_supur_ensemble

accuracies <-cbind( rbind(mape1_supur,mape2_supur) ,rbind(mape1_supur_dr1,mape2_supur_dr1)
                    ,rbind(mape1_supur_dr2,mape2_supur_dr2),rbind(mape1_supur_con_rate,mape2_supur_con_rate),
                    rbind(mape1_supur_ensemble,mape2_supur_ensemble))

colnames(accuracies) <- c("auto_arima", "dynamic_regression_1", "dynamic_regression_2", "convergence_rate", "ensemble")
accuracies

# TIGHTS

tayt <- subset(data, product_content_id == "31515569")
tayt <- tayt[event_date<"2020-06-14"]
tayt[tayt == -1] <- NA

tayt[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation

# Initialization of parameters
test_data <- NULL
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(tayt,h)

for (n in a:b) {
  tayt_cut <- tayt[1:(nrow(tayt) - n)] # creating the train data
  
  # Auto Arima tayt
  tayt_cut <- tayt_cut[event_date>"2020-01-23"]
  model <- auto.arima(tayt_cut$sold_count)
  predictions <- forecast(model, h=2)$mean
  
  # Dynamic Regression Model 1    tayt
  tayt_cut <- tayt_cut[event_date>"2020-03-06"]
  dates<-as.Date(tayt_cut$event_date)
  tayt_sold_xts<-xts(tayt_cut$sold_count,order.by = dates)
  tayt_sold_ts<-ts(tayt_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(tayt_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(tayt_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(tayt_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(tayt_cut$price,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(tayt_cut$basket_count,tayt_cut$favored_count)
  arima_model_with_var <- auto.arima(tayt_sold_ts,xreg=xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Dynamic Regression Model 2   tayt
  tayt_cut <- tayt_cut[event_date>"2020-03-06"]
  dates<-as.Date(tayt_cut$event_date)
  tayt_sold_xts<-xts(tayt_cut$sold_count,order.by = dates)
  tayt_sold_ts<-ts(tayt_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,basket_count:=as.vector(naive(tayt_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(tayt_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(tayt_cut$price,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(tayt_cut$basket_count,tayt_cut$favored_count, tayt_cut$price, tayt_cut$category_visits, tayt_cut$visit_count
                  , tayt_cut$category_brand_sold)
  arima_model_with_var <- auto.arima(tayt_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count, test_data$price, test_data$category_visits, test_data$visit_count, test_data$category_brand_sold)
  predictions1 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean

  # Convergence Rate    tayt
  tayt_cut <- tayt_cut[event_date>"2020-03-06"]
  test_data[,ratebas:=as.vector(naive(tayt_cut$ratebas,h=h)$mean)]
  test_data[,visit_count:=as.vector(naive(tayt_cut$visit_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(tayt_cut$favored_count,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(tayt_cut$category_visits,h=h)$mean)]
  # test_data[,basket_count:=as.vector(naive(tayt_cut$basket_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(tayt_cut$basket_count,
                                                         xreg= cbind(tayt_cut$favored_count,
                                                                     tayt_cut$visit_count,
                                                                     tayt_cut$category_visits)),
                                              h=h, xreg=cbind(test_data$favored_count,
                                                              test_data$visit_count,
                                                              test_data$category_visits))$mean)]
  predictions <- test_data$basket_count * test_data$ratebas

  # Ensemble
  # Calculate the mean of Dynamic Regression 1 and Convergence Rate's predictions
  predictions <- (predictions1+predictions)/2
  
  allpred <- rbind(allpred, (as.numeric(predictions))) # creating a matrix of all predictions
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

tayt_cut1 <- tayt[(nrow(tayt) - b + 1):(nrow(tayt) - a + 1)] # creating the test data for predictions of the 1st day
tayt_cut2 <- tayt[(nrow(tayt) - b + 2):(nrow(tayt) - a + 2)] # creating the test data for predictions of the 2nd day

pred_tayt <- allpred2
tru_tayt <- tayt_cut2$sold_count

mape1_tayt <- 100*(sum(abs(tayt_cut1$sold_count-allpred1))/ sum(tayt_cut1$sold_count))
mape2_tayt <- 100*(sum(abs(tayt_cut2$sold_count-allpred2))/ sum(tayt_cut2$sold_count))
mape1_tayt
mape2_tayt

# TOOTHBRUSH

dis <- subset(data, product_content_id == "32939029")
dis <- dis[event_date<"2020-06-14"]
dis[dis == -1] <- NA

dis[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation

# Initialization of parameters
test_data <- NULL
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(dis,h)

# dis <- dis[event_date>"2020-01-23"]
# dis <- dis[event_date>"2020-03-06"]

for (n in a:b) {
  dis_cut <- dis[1:(nrow(dis) - n)] # creating the train data
  
  # Auto Arima Dis
  dis_cut <- dis_cut[event_date>"2020-01-23"]
  model <- auto.arima(dis_cut$sold_count)
  predictions <- forecast(model, h=2)$mean

  # Dynamic Regression Model 1    Dis
  dis_cut <- dis_cut[event_date>"2020-03-06"]
  dates<-as.Date(dis_cut$event_date)
  dis_sold_xts<-xts(dis_cut$sold_count,order.by = dates)
  dis_sold_ts<-ts(dis_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(dis_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(dis_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(dis_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(dis_cut$price,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(dis_cut$visit_count,dis_cut$basket_count,dis_cut$favored_count, dis_cut$price)
  arima_model_with_var <- auto.arima(dis_sold_ts,xreg=xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count, test_data$price)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean

  # Dynamic Regression Model 2   Dis
  dis_cut <- dis_cut[event_date>"2020-03-06"]
  dates<-as.Date(dis_cut$event_date)
  dis_sold_xts<-xts(dis_cut$sold_count,order.by = dates)
  dis_sold_ts<-ts(dis_sold_xts,frequency = 1)
  # Forecasting Regressors using Naive
  test_data[,basket_count:=as.vector(naive(dis_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(dis_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(dis_cut$price,h=h)$mean)]
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(dis_cut$basket_count,dis_cut$favored_count, dis_cut$price)
  arima_model_with_var <- auto.arima(dis_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count, test_data$price)
  predictions <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Convergence Rate    Dis
  dis_cut <- dis_cut[event_date>"2020-03-06"]
  test_data[,ratebas:=as.vector(naive(dis_cut$ratebas,h=h)$mean)]
  test_data[,visit_count:=as.vector(naive(dis_cut$visit_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(dis_cut$favored_count,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(dis_cut$category_visits,h=h)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(dis_cut$basket_count,
                                                         xreg= cbind(dis_cut$favored_count,
                                                                     dis_cut$visit_count,
                                                                     dis_cut$category_visits)),
                                              h=h, xreg=cbind(test_data$favored_count,
                                                              test_data$visit_count,
                                                              test_data$category_visits))$mean)]
  predictions <- test_data$basket_count * test_data$ratebas
  
  # Ensemble
  # Calculate the mean of Dynamic Regression 1 and Convergence Rate's predictions
  # predictions <- (predictions1+predictions)/2
  
  allpred <- rbind(allpred, (as.numeric(predictions))) # creating a matrix of all predictions
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

dis_cut1 <- dis[(nrow(dis) - b + 1):(nrow(dis) - a + 1)] # creating the test data for predictions of the 1st day
dis_cut2 <- dis[(nrow(dis) - b + 2):(nrow(dis) - a + 2)] # creating the test data for predictions of the 2nd day

pred_dis <- allpred2
tru_dis <- dis_cut2$sold_count

mape1_dis <- 100*(sum(abs(dis_cut1$sold_count-allpred1))/ sum(dis_cut1$sold_count))
mape2_dis <- 100*(sum(abs(dis_cut2$sold_count-allpred2))/ sum(dis_cut2$sold_count))
mape1_dis
mape2_dis

# BIKINI

bikini <- subset(data, product_content_id == "5926527")
bikini <- bikini[event_date<"2020-06-14"]
bikini[bikini == -1] <- NA


bikini[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation
# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(bikini,h)

bikini <- bikini[event_date>"2020-01-23"]

for (n in a:b) {
  
  bikini_cut <- bikini[1:(nrow(bikini) - n)] # creating the train data
  
  # Auto Arima bikini
  model1 <- auto.arima(bikini_cut$sold_count)
  predictions1 <- forecast(model1, h=2)$mean
  
  # # Dynamic Regression Model 1-2    bikini
  
  dates<-as.Date(bikini_cut$event_date)
  bikini_sold_xts<-xts(bikini_cut$sold_count,order.by = dates)
  bikini_sold_ts<-ts(bikini_sold_xts,frequency = 1)
  
  # Forecasting Regressors using Naive
  test_data[,visit_count:=as.vector(naive(bikini_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(bikini_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(bikini_cut$favored_count,h=h)$mean)]
  test_data[,price:=as.vector(naive(bikini_cut$price,h=h)$mean)]
  test_data[,category_sold:=as.vector(naive(bikini_cut$category_sold,h=h)$mean)]
  test_data[,category_brand_sold:=as.vector(naive(bikini_cut$category_brand_sold,h=h)$mean)]
  test_data[,category_visits:=as.vector(naive(bikini_cut$category_visits,h=h)$mean)]
  test_data[,ty_visits:=as.vector(naive(bikini_cut$ty_visits,h=h)$mean)]
  
  
  # Fitting ARIMA with Regressors 1
  xreg_data<-cbind(bikini_cut$visit_count,bikini_cut$basket_count,bikini_cut$favored_count, bikini_cut$price)
  arima_model_with_var <- auto.arima(bikini_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$visit_count,test_data$basket_count,test_data$favored_count, test_data$price)
  predictions2 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  # Fitting ARIMA with Regressors 2
  xreg_data<-cbind(bikini_cut$basket_count,bikini_cut$favored_count, bikini_cut$price)
  arima_model_with_var <- auto.arima(bikini_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count, test_data$price)
  predictions3 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  
  # Convergence Rate    bikini
  test_data$ratebas  <- as.vector(naive(bikini_cut$ratebas,h=h)$mean)
  test_data[,ratebas:=as.vector(naive(bikini_cut$ratebas,h=h)$mean)]
  test_data[,basket_count:=as.vector(forecast(auto.arima(bikini_cut$basket_count,
                                                         xreg= cbind(bikini_cut$favored_count,
                                                                     bikini_cut$visit_count,
                                                                     bikini_cut$category_visits)),h=h,
                                              xreg=cbind(test_data$favored_count,
                                                         test_data$visit_count, test_data$category_visits))$mean)]
  
  predictions4 = test_data$basket_count * test_data$ratebas
  
  predictions5 <- (as.numeric(predictions4)+as.numeric(predictions1))/2
  
  
  
  predictions <- cbind(as.numeric(predictions1),as.numeric(predictions2),as.numeric(predictions3),as.numeric(predictions4),as.numeric(predictions5))
  
  allpred <- rbind(allpred, as.numeric(predictions)) # creating a matrix of all predictions
  
}

allpred1_arima <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2_arima <- rev(allpred[, 2]) # for predictions of the 2nd day
allpred1_dynamic_reg_1 <- rev(allpred[, 3]) # for predictions of the 1st day
allpred2_dynamic_reg_1 <- rev(allpred[, 4]) # for predictions of the 2nd day
allpred1_dynamic_reg_2 <- rev(allpred[, 5]) # for predictions of the 1st day
allpred2_dynamic_reg_2 <- rev(allpred[, 6]) # for predictions of the 2nd day
allpred1_convergence_rate <- rev(allpred[, 7]) # for predictions of the 1st day
allpred2_convergence_rate <- rev(allpred[, 8]) # for predictions of the 2nd day
allpred1_ensemble <- rev(allpred[, 9]) # for predictions of the 1st day
allpred2_ensemble <- rev(allpred[, 10]) # for predictions of the 2nd day

bikini_cut1 <- bikini[(nrow(bikini) - b + 1):(nrow(bikini) - a + 1)] # creating the test data for predictions of the 1st day
bikini_cut2 <- bikini[(nrow(bikini) - b + 2):(nrow(bikini) - a + 2)] # creating the test data for predictions of the 2nd day

pred_bikini <- allpred2_ensemble
tru_bikini <- bikini_cut2$sold_count




mape1_bikini <- 100*(sum(abs(bikini_cut1$sold_count-allpred1_arima))/ sum(bikini_cut1$sold_count))
mape2_bikini <- 100*(sum(abs(bikini_cut2$sold_count-allpred2_arima))/ sum(bikini_cut2$sold_count))
mape1_bikini
mape2_bikini
mape1_bikini_dr1 <- 100*(sum(abs(bikini_cut1$sold_count-allpred1_dynamic_reg_1))/ sum(bikini_cut1$sold_count))
mape2_bikini_dr1 <- 100*(sum(abs(bikini_cut2$sold_count-allpred2_dynamic_reg_1))/ sum(bikini_cut2$sold_count))
mape1_bikini_dr1
mape2_bikini_dr1
mape1_bikini_dr2 <- 100*(sum(abs(bikini_cut1$sold_count-allpred1_dynamic_reg_2))/ sum(bikini_cut1$sold_count))
mape2_bikini_dr2 <- 100*(sum(abs(bikini_cut2$sold_count-allpred2_dynamic_reg_2))/ sum(bikini_cut2$sold_count))
mape1_bikini_dr2
mape2_bikini_dr2
mape1_bikini_con_rate <- 100*(sum(abs(bikini_cut1$sold_count-allpred1_convergence_rate))/ sum(bikini_cut1$sold_count))
mape2_bikini_con_rate <- 100*(sum(abs(bikini_cut2$sold_count-allpred2_convergence_rate))/ sum(bikini_cut2$sold_count))
mape1_bikini_con_rate
mape2_bikini_con_rate
mape1_bikini_ensemble <- 100*(sum(abs(bikini_cut1$sold_count-allpred1_ensemble))/ sum(bikini_cut1$sold_count))
mape2_bikini_ensemble <- 100*(sum(abs(bikini_cut2$sold_count-allpred2_ensemble))/ sum(bikini_cut2$sold_count))
mape1_bikini_ensemble
mape2_bikini_ensemble

accuracies <-cbind( rbind(mape1_bikini,mape2_bikini) ,rbind(mape1_bikini_dr1,mape2_bikini_dr1)
                    ,rbind(mape1_bikini_dr2,mape2_bikini_dr2),rbind(mape1_bikini_con_rate,mape2_bikini_con_rate) 
                    ,rbind(mape1_bikini_ensemble,mape2_bikini_ensemble))

colnames(accuracies) <- c("auto_arima", "dynamic_regression_1", "dynamic_regression_2", "convergence_rate", "ensemble")
accuracies


# COAT

mont <- subset(data, product_content_id == "3904356")
mont <- mont[event_date<"2020-06-14"]

mont[,ratebas:= sold_count/basket_count] # needed for convergence rate calculation
mont[,ratefav:= sold_count/favored_count]
mont[,ratevis:= sold_count/visit_count]
# Initialization of parameters
test_data <- NULL 
allpred <- NULL
a <- 2 # minimum number of days to cut
b <- 30 # maximum number of days to cut
h <- 2 # number of days to predict

test_data=tail(mont,h)
mont <- mont[event_date>"2020-01-23"]

for (n in a:b) {
  mont_cut <- mont[1:(nrow(mont) - n)] # creating the train data
  # Auto Arima mont
  model1 <- auto.arima(mont_cut$sold_count)
  predictions <- forecast(model1, h=2)$mean
  
  # Dynamic Regression Model     mont
  
  # Forecasting Regressors using Naive
  
  test_data[,visit_count:=as.vector(naive(mont_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(mont_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(mont_cut$favored_count,h=h)$mean)]
  
  # auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(mont_cut$basket_count,xreg=cbind(mont_cut$favored_count)),h=h,xreg=cbind(test_data$favored_count))$mean)]
  test_data[,favored_count:=as.vector(forecast(auto.arima(mont_cut$favored_count,xreg=cbind(mont_cut$basket_count,mont_cut$visit_count,mont_cut$price)),h=h,xreg=cbind(test_data$basket_count,test_data$visit_count,test_data$price))$mean)]
  test_data[,visit_count:=as.vector(forecast(auto.arima(mont_cut$visit_count,xreg=mont_cut$favored_count),h=h,xreg=test_data$favored_count)$mean)]
  
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(mont_cut$basket_count,mont_cut$favored_count,mont_cut$visit_count)
  arima_model_with_var <- auto.arima(mont_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count,test_data$visit_count)
  
  predictions1 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  allpred <- rbind(allpred, as.numeric(predictions1)) # creating a matrix of all predictions
  
  #Convergence Rate
  # Forecasting Regressors using Naive
  
  test_data[,visit_count:=as.vector(naive(mont_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(mont_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(mont_cut$favored_count,h=h)$mean)]
  
  #Forecasting Regressors Using auto.arima
  test_data[,ratebas:= as.vector(forecast(auto.arima(mont_cut$ratebas),h=h)$mean)]
  
  # auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(mont_cut$basket_count,xreg=cbind(mont_cut$favored_count)),h=h,xreg=cbind(test_data$favored_count))$mean)]
  
  predictions_bas =test_data$basket_count *test_data$ratebas
  allpred <- rbind(allpred, as.numeric(predictions_bas)) # creating a matrix of all predictions
  #Ensemble
  # Forecasting Regressors using Naive
  
  test_data[,visit_count:=as.vector(naive(mont_cut$visit_count,h=h)$mean)]
  test_data[,basket_count:=as.vector(naive(mont_cut$basket_count,h=h)$mean)]
  test_data[,favored_count:=as.vector(naive(mont_cut$favored_count,h=h)$mean)]
  
  #Forecasting Regressors Using auto.arima
  test_data[,ratebas:= as.vector(forecast(auto.arima(mont_cut$ratebas),h=h)$mean)]
  
  # auto arima with xreg
  test_data[,basket_count:=as.vector(forecast(auto.arima(mont_cut$basket_count,xreg=cbind(mont_cut$favored_count)),h=h,xreg=cbind(test_data$favored_count))$mean)]
  test_data[,favored_count:=as.vector(forecast(auto.arima(mont_cut$favored_count,xreg=cbind(mont_cut$basket_count,mont_cut$visit_count,mont_cut$price)),h=h,xreg=cbind(test_data$basket_count,test_data$visit_count,test_data$price))$mean)]
  test_data[,visit_count:=as.vector(forecast(auto.arima(mont_cut$visit_count,xreg=mont_cut$favored_count),h=h,xreg=test_data$favored_count)$mean)]
  
  
  # Fitting ARIMA with Regressors
  xreg_data<-cbind(mont_cut$basket_count,mont_cut$favored_count,mont_cut$visit_count)
  arima_model_with_var <- auto.arima(mont_sold_ts,xreg =xreg_data,stepwise=F)
  xreg_for<-cbind(test_data$basket_count,test_data$favored_count,test_data$visit_count)
  
  predictions1 <- forecast(arima_model_with_var,h=h,xreg=xreg_for)$mean
  
  predictions_bas =test_data$basket_count *test_data$ratebas
  predictions <-(predictions_bas + as.numeric(predictions1))/2
  
  allpred <- rbind(allpred, as.numeric(predictions)) # creating a matrix of all predictions
  
}

allpred1 <- rev(allpred[, 1]) # for predictions of the 1st day
allpred2 <- rev(allpred[, 2]) # for predictions of the 2nd day

mont_cut1 <- mont[(nrow(mont) - b + 1):(nrow(mont) - a + 1)] # creating the test data for predictions of the 1st day
mont_cut2 <- mont[(nrow(mont) - b + 2):(nrow(mont) - a + 2)] # creating the test data for predictions of the 2nd day

pred_mont <- allpred2
tru_mont <- mont_cut2$sold_count

# MAE Calculation

mae1_mont<-mean(abs(mont_cut1$sold_count-allpred1))
mae2_mont<-mean(abs(mont_cut2$sold_count-allpred2))
mae1_mont
mae2_mont

pred_mat <- cbind(pred_yuz, pred_mendil, pred_kulak, pred_supur, pred_tayt, pred_dis, pred_bikini,pred_mont)
tru_mat <- cbind(tru_yuz, tru_mendil, tru_kulak, tru_supur, tru_tayt, tru_dis, tru_bikini, tru_mont)

wmape <- rowSums(abs(tru_mat - pred_mat)) / rowSums(tru_mat)
