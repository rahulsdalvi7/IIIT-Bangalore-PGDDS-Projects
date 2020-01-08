# ---------------------------------------------------------------------------------
# Retail-Giant Sales Forecasting - Timeseries group Assignment
# ---------------------------------------------------------------------------------
# Business goal:  Forecast the sales and the demand for the next 6 months for 
# "Global Mart" - an online store super giant, using sale transaction data from 
# January 2011 to December 2014, in order to  manage the revenue and inventory 
# accordingly.
# ---------------------------------------------------------------------------------

# install.packages("dplyr")

library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(stringi)

# Let's load the data
global.superstore <- read.csv("Global Superstore.csv", stringsAsFactors = F)

# Take a look at the data
View(global.superstore)
str(global.superstore)

  # So we have 51290 obeservation with 24 variables
  # we have 17 character variable and 7 numerical variable

# Let's check if we have NA in dataset
sapply(global.superstore, function(x) sum(is.na(x)))
  # there are no NAs in dataset except for the postal code which has 41296
 

# We need only few variables for our analysis so lets filter those out
global.superstore.filtered <- global.superstore[c("Order.Date", "Segment", "Market", "Sales", "Quantity", "Profit")]

# Converting Order.Date in data format
  global.superstore.filtered$Order.Date <- dmy(global.superstore.filtered$Order.Date)
  
# Lets convert day to first day for aggregating the result
  mday(global.superstore.filtered$Order.Date) <- 01

# Aggregate the dataset by segment market monthly wise
  global.superstore.aggregate <- 
    group_by(global.superstore.filtered, Segment, Market, Order.Date) %>% 
    summarise(Sales.agg=sum(Sales), Quantity.agg=sum(Quantity), Profit.agg=sum(Profit))
  
# Let's check which has highest profit
  ggplot(group_by(global.superstore.aggregate,Segment, Market) %>%  
           summarise(Profit.sum=sum(Profit.agg)),aes(x=factor(Market),y=Profit.sum,fill=factor(Segment))) + 
    geom_bar(stat="identity",position="dodge")+xlab("Market") + 
    ylab("Monthly Profit Sum") + 
    ggtitle("Monthly Profit Sum Vs. Market Segment")  +
    geom_text(aes(label=paste(format(round(Profit.sum / 1e3, 1), trim = TRUE), sep="", "k")), vjust=1.5, color='black', position=position_dodge(.9), size=4)
    # We have two APAC-Consumer and EU-Consumer but we want most consistent profitable market segment

# Lets calculate the coffiecient of variation 
  global.superstore.profitable <- 
    group_by(global.superstore.aggregate,Segment, Market) %>% 
    summarise(CV=sd(Profit.agg)/mean(Profit.agg)) %>% 
    arrange(CV) 
  
# Let check which has the less variation
  ggplot(global.superstore.profitable,aes(x=factor(Market),y=CV,fill=factor(Segment))) + 
    geom_bar(stat="identity",position="dodge")+xlab("Market") + 
    ylab("Coeff. of variance of Monthly Profit") + 
    ggtitle("Coeff. of variance in Monthly Profit Vs. Market Segment") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_text(aes(label=round(CV,2)), vjust=1.5, color='black', position=position_dodge(.9), size=4)
  
  # If we check APAC-Consumer and EU-Consumer has less variation with most profitability
  # Let's subset the data based on these two
  
# Subset the dataset for Segment== "Consumer", Market == "EU"
  consumer.eu <- 
    filter(global.superstore.aggregate, Segment=="Consumer", Market=="EU") %>% 
    arrange(Order.Date)
  
  # Subset the dataset for Segment=="Consumer", Market=="APAC"
  consumer.apac <- 
    filter(global.superstore.aggregate, Segment=="Consumer", Market=="APAC") %>% 
    arrange(Order.Date)

# ---------------------------------------------------------------------------------
# Let's analyze EU Consumer datasets
# ---------------------------------------------------------------------------------

# Modeling EU consumer sales data

# create time series for EU consumer sales
  consumer.eu.sales.ts <- ts(consumer.eu$Sales.agg, frequency = 12, start=c(2011,1), end=c(2014,12))
# plot the timeseries
  plot(consumer.eu.sales.ts)
  
# Let's Decompose the time series to get the trend and sesonal data out of it.
  decomposed.consumer.eu.sales.ts <- decompose(consumer.eu.sales.ts)
  plot(decomposed.consumer.eu.sales.ts) 
    # We can clearly see we have the additive series with trend and seasonality
  
# plot the time series
  plot(consumer.eu.sales.ts, xlab="Time",ylab="Sales",lwd=2,col='blue', main = "Sales - EU Consumer Segment")
  
# smooth the time series
  smoothedseries <- stats::filter(consumer.eu.sales.ts, 
                                  filter=rep(1/3, 3), 
                                  method='convolution', sides=2)
  
# lets plot the line over the plot
  lines(smoothedseries, col="red", lwd=2)
    # we can see smoothed series showing upward trend
  
# add missing point due to smoothing the series
  smoothedseries[1] = smoothedseries[2] - (smoothedseries[3] - smoothedseries[2])
  smoothedseries[48] = smoothedseries[47] + (smoothedseries[47] - smoothedseries[46])
  
  lines(smoothedseries, col="red", lwd=2)
  
# lets create a dataframe out of the smoothseries
  smoothed.consumer.eu.sales <- data.frame(cbind(c(1:48), smoothedseries))
# rename the columns
  colnames(smoothed.consumer.eu.sales) <- c("Month", "Sales")

# create a time series from the smooth dataframe
  smoothed.consumer.eu.sales.ts <- ts(smoothed.consumer.eu.sales$Sales, frequency=12, start=c(2011,1), end=c(2014,12))  
  lines(smoothed.consumer.eu.sales.ts, col='red', lwd=2) 
  
# Let's create a test and train dataset, test dataset is last 6 months
  consumer.eu.sales.train <- window(smoothed.consumer.eu.sales.ts, start=c(2011,1), end=c(2014,6))
  consumer.eu.sales.test <- window(smoothed.consumer.eu.sales.ts, start=c(2014,7), end=c(2014,12))
  
# create and fit linear regression model to timeseries
  consumer.eu.sales.tslm <- tslm(consumer.eu.sales.train ~ trend+I(sin(0.5*trend/2))+season)
# lets summarize model
  summary(consumer.eu.sales.tslm)   
    # Adjusted R-squared:  0.8258 
  
# Forecasting time series for next 6 months
  consumer.eu.sales.forecast <- forecast(consumer.eu.sales.tslm, h=6, level=0)
  plot(smoothed.consumer.eu.sales.ts, ylab="Sales", xlab="Time", col="blue", lwd=2, main = "Sales -  EU Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.eu.sales.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.eu.sales.forecast$mean, col="red", lwd=2)
  
# Accuracy measures for a forecast model
  consumer.eu.sales.accuracy <- accuracy(consumer.eu.sales.forecast$mean, consumer.eu.sales.test) 
  Mape_eu_sales_cd<-consumer.eu.sales.accuracy[5] 
  Mape_eu_sales_cd
    # MAPE 7.960089
  
# plot acf 
  acf(consumer.eu.sales.forecast$residuals,lag.max = 12) 
  acf(consumer.eu.sales.forecast$residuals,lag.max = 12, type="partial")
    # using acf we can see that residuals are white noise
  

# Fit best ARIMA model to univariate time series
  consumer.eu.sales.arima <- auto.arima(consumer.eu.sales.train)
  summary(consumer.eu.sales.arima)
    # Series: consumer.eu.sales.train 
    # ARIMA(0,1,0)(1,0,0)[12] 
  
    # Coefficients:
    #           sar1
    #           0.5125
    # s.e.      0.1395
  
    # sigma^2 estimated as 18341286:  log likelihood=-402.35
    # AIC=808.71   AICc=809.02   BIC=812.13
  
    # Training set error measures:
    #                    ME     RMSE      MAE    MPE     MAPE      MASE      ACF1
    # Training set 325.7499 4179.461 3199.181 1.0661 11.61645 0.4477466 0.1347556
  
  
# Test for stationary
  adf.test(consumer.eu.sales.arima$residuals, alternative = "stationary") 
    # p-value = 0.03221 (<0.05)
  kpss.test(consumer.eu.sales.arima$residuals) 
    # p-value = 0.1 (>0.05)
    # using this adf and kpss test we see that the residual is stationary series
  
# Forecasting time series for next 6 months
  consumer.eu.sales.arima.forecast <- forecast(consumer.eu.sales.arima, h=6)
  plot(smoothed.consumer.eu.sales.ts, ylab="Sales", xlab="Time", col="blue",lwd=4,main = "Sales -  EU Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.eu.sales.arima.forecast$residual, lwd=2,col="yellow",lty=3)
  lines(consumer.eu.sales.arima.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.eu.sales.arima.forecast$mean, col="red",lwd=2)
  
# Accuracy measures for a forecast model
  consumer.eu.sales.arima.accuracy <- accuracy(consumer.eu.sales.arima.forecast, consumer.eu.sales.test)
  Mape_eu_sales_arima<- consumer.eu.sales.arima.accuracy[2,5] 
  Mape_eu_sales_arima
    # MAPE 27.11146
  
# ---------------------------------------------------------------------------------

# Modeling EU consumer quantity data

# create time series for EU consumer quanity
  consumer.eu.qty.ts <- ts(consumer.eu$Quantity.agg, frequency = 12, start=c(2011,1), end=c(2014,12))
# plot the timeseries
  plot(consumer.eu.qty.ts)
  
# Let's Decompose the time series to get the trend and sesonal data out of it.
  decomposed.consumer.eu.qty.ts <- decompose(consumer.eu.qty.ts)
  plot(decomposed.consumer.eu.qty.ts) 
    # We can clearly see we have the additive series with trend and seasonality
  
# plot the time series
  plot(consumer.eu.qty.ts, xlab="Time",ylab="Quantity",lwd=2,col='blue', main = "Quanity - EU Consumer Segment")
  
# smooth the time series
  smoothedseries_qty <- stats::filter(consumer.eu.qty.ts, 
                                  filter=rep(1/3, 3), 
                                  method='convolution', sides=2)
  
# lets plot the line over the plot
  lines(smoothedseries_qty, col="red", lwd=2)
    # we can see smoothed series showing upward trend
  
# add missing point due to smoothing the series
  smoothedseries_qty[1] = smoothedseries_qty[2] - (smoothedseries_qty[3] - smoothedseries_qty[2])
  smoothedseries_qty[48] = smoothedseries_qty[47] + (smoothedseries_qty[47] - smoothedseries_qty[46])
  
  lines(smoothedseries_qty, col="red", lwd=2)
  
# lets create a dataframe out of the smoothseries
  smoothed.consumer.eu.qty <- data.frame(cbind(c(1:48), smoothedseries_qty))
# rename the columns
  colnames(smoothed.consumer.eu.qty) <- c("Month", "Quantity")

# create a time series from the smooth dataframe
  smoothed.consumer.eu.qty.ts <- ts(smoothed.consumer.eu.qty$Quantity, frequency=12, start=c(2011,1), end=c(2014,12))  
  lines(smoothed.consumer.eu.qty.ts, col='green', lwd=2) 
  
# Let's create a test and train dataset, test dataset is last 6 months
  consumer.eu.qty.train <- window(smoothed.consumer.eu.qty.ts, start=c(2011,1), end=c(2014,6))
  consumer.eu.qty.test <- window(smoothed.consumer.eu.qty.ts, start=c(2014,7), end=c(2014,12))
  
# create and fit linear regression model to timeseries
  consumer.eu.qty.tslm <- tslm(consumer.eu.qty.train ~ trend+I(sin(0.5*trend/2))+season)
# lets summarize model
  summary(consumer.eu.qty.tslm)   
    # Adjusted R-squared:  0.9119
  
# Forecasting time series for next 6 months
  consumer.eu.qty.forecast <- forecast(consumer.eu.qty.tslm, h=6, level=0)
  plot(smoothed.consumer.eu.qty.ts, ylab="Quantity", xlab="Time", col="blue", lwd=2, main = "Quantity -  EU Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.eu.qty.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.eu.qty.forecast$mean, col="red", lwd=2)
  
# Accuracy measures for a forecast model
  consumer.eu.qty.accuracy <- accuracy(consumer.eu.qty.forecast$mean, consumer.eu.qty.test) 
  Mape_eu_qty_cd <- consumer.eu.qty.accuracy[5] 
  Mape_eu_qty_cd
      #MAPE 13.63608
  
# plot acf 
  acf(consumer.eu.qty.forecast$residuals,lag.max = 12) 
  acf(consumer.eu.qty.forecast$residuals,lag.max = 12, type="partial")
    # using acf we can see that residuals are white noise
  
# Fit best ARIMA model to univariate time series
  consumer.eu.qty.arima <- auto.arima(consumer.eu.qty.train)
  summary(consumer.eu.qty.arima)
    # Series: consumer.eu.qty.train 
    # ARIMA(0,1,0)(0,1,1)[12] 
  
    # Coefficients:
    #          sma1
    #       -0.5654
    # s.e.   0.2982
  
    # sigma^2 estimated as 1538:  log likelihood=-149.22
    # AIC=302.43   AICc=302.89   BIC=305.17
  
    # Training set error measures:
    #                     ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
    # Training set -2.350854 32.02399 19.23091 -1.010755 4.966902 0.2317908 0.03618192
  
  
# Test for stationary
  adf.test(consumer.eu.qty.arima$residuals, alternative = "stationary") 
      # p-value = 0.01 (<0.05)
  kpss.test(consumer.eu.qty.arima$residuals) 
      # p-value = 0.1 (>0.05)
      # using this adf and kpss test we see that the residual is stationary series
  
# Forecasting time series for next 6 months
  consumer.eu.qty.arima.forecast <- forecast(consumer.eu.qty.arima, h=6)
  plot(smoothed.consumer.eu.qty.ts, ylab="Quantity", xlab="Time", col="blue",lwd=4,main = "Quantity -  EU Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.eu.qty.arima.forecast$residual, lwd=2,col="yellow",lty=3)
  lines(consumer.eu.qty.arima.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.eu.qty.arima.forecast$mean, col="red",lwd=2)
  
# Accuracy measures for a forecast model
  consumer.eu.qty.arima.accuracy <- accuracy(consumer.eu.qty.arima.forecast, consumer.eu.qty.test)
  Mape_eu_qty_arima<- consumer.eu.qty.arima.accuracy[2,5]
  Mape_eu_qty_arima
  # MAPE 19.288094


# ---------------------------------------------------------------------------------
# Let's analyze APAC Consumer datasets
# ---------------------------------------------------------------------------------
  
# Modeling APAC consumer sales data
  
# create time series for APAC consumer sales
  consumer.apac.sales.ts <- ts(consumer.apac$Sales.agg, frequency = 12, start=c(2011,1), end=c(2014,12))
# plot the timeseries
  plot(consumer.apac.sales.ts)
  
# Let's Decompose the time series to get the trend and sesonal data out of it.
  decomposed.consumer.apac.sales.ts <- decompose(consumer.apac.sales.ts)
  plot(decomposed.consumer.apac.sales.ts) 
    # This too is an additive series with trend and seasonality
  
# plot the time series
  plot(consumer.apac.sales.ts, xlab="Time",ylab="Sales",lwd=2,col='blue', main ="Sales - APAC Consumer Segment")
  
# smooth the time series
  smoothedseries_apac <- stats::filter(consumer.apac.sales.ts, 
                                  filter=rep(1/3, 3), 
                                  method='convolution', sides=2)
  
# lets plot the line over the plot
  lines(smoothedseries_apac, col="red", lwd=2)
    # we can see smoothed series showing upward trend
  
# add missing point due to smoothing the series
  smoothedseries_apac[1] = smoothedseries_apac[2] - (smoothedseries_apac[3] - smoothedseries_apac[2])
  smoothedseries_apac[48] = smoothedseries_apac[47] + (smoothedseries_apac[47] - smoothedseries_apac[46])
  
  lines(smoothedseries_apac, col="red", lwd=2)
  
# lets create a dataframe out of the smoothseries
  smoothed.consumer.apac.sales <- data.frame(cbind(c(1:48), smoothedseries_apac))
# rename the columns
  colnames(smoothed.consumer.apac.sales) <- c("Month", "Sales")
  
# create a time series from the smooth dataframe
  smoothed.consumer.apac.sales.ts <- ts(smoothed.consumer.apac.sales$Sales, frequency=12, start=c(2011,1), end=c(2014,12))  
  lines(smoothed.consumer.apac.sales.ts, col='green', lwd=2) 
  
# Let's create a test and train dataset, test dataset is last 6 months
  consumer.apac.sales.train <- window(smoothed.consumer.apac.sales.ts, start=c(2011,1), end=c(2014,6))
  consumer.apac.sales.test  <- window(smoothed.consumer.apac.sales.ts, start=c(2014,7), end=c(2014,12))
  
# create and fit linear regression model to timeseries
  consumer.apac.sales.tslm <- tslm(consumer.apac.sales.train ~ trend+I(sin(0.5*trend/2))+season)
# lets summarize model
  summary(consumer.apac.sales.tslm)   
      # Adjusted R-squared:  0.8581  
  
# Forecasting time series for next 6 months
  consumer.apac.sales.forecast <- forecast(consumer.apac.sales.tslm, h=6, level=0)
  plot(smoothed.consumer.apac.sales.ts, ylab="Sales", xlab="Time", col="blue", lwd=2, main = "Sales -  APAC Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.apac.sales.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.apac.sales.forecast$mean, col="red", lwd=2)
  
# Accuracy measures for a forecast model
  consumer.apac.sales.accuracy <- accuracy(consumer.apac.sales.forecast$mean, consumer.apac.sales.test) 
  Mape_apac_sales_cd<- consumer.apac.sales.accuracy[5] 
  Mape_apac_sales_cd
    # MAPE 11.69721
  
# plot acf 
  acf(consumer.apac.sales.forecast$residuals,lag.max = 12) 
  acf(consumer.apac.sales.forecast$residuals,lag.max = 12, type="partial")
    # using acf we can see that residuals are white noise
  
# Fit best ARIMA model to univariate time series
  consumer.apac.sales.arima <- auto.arima(consumer.apac.sales.train)
  summary(consumer.apac.sales.arima)
    # Series: consumer.apac.sales.train 
    # ARIMA(0,1,0)(1,0,0)[12] 
  
    # Coefficients:
    #         sar1
    #       0.5471
    # s.e.  0.1166
  
    # sigma^2 estimated as 23296680:  log likelihood=-407.56
    # AIC=819.12   AICc=819.44   BIC=822.55
  
    # Training set error measures:
    #                    ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
    # Training set 226.4577 4710.341 3669.553 0.1899726 10.56447 0.4463773 0.06279321
  
# Test for stationary
  adf.test(consumer.apac.sales.arima$residuals, alternative = "stationary") 
    # p-value = 0.01 (<0.05)
  kpss.test(consumer.apac.sales.arima$residuals) 
    # p-value = 0.1  (>0.05)
    # using this adf and kpss test we see that the residual is stationary series
  
# Forecasting time series for next 6 months
  consumer.apac.sales.arima.forecast <- forecast(consumer.apac.sales.arima, h=6)
  plot(smoothed.consumer.apac.sales.ts, ylab="Sales", xlab="Time", col="blue",lwd=4, main = "Sales -  APAC Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.apac.sales.arima.forecast$residual, lwd=2,col="yellow",lty=3)
  lines(consumer.apac.sales.arima.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.apac.sales.arima.forecast$mean, col="red",lwd=2)
  
# Accuracy measures for a forecast model
  consumer.apac.sales.arima.accuracy <- accuracy(consumer.apac.sales.arima.forecast, consumer.apac.sales.test)
  Mape_apac_sales_arima<- consumer.apac.sales.arima.accuracy[2,5]
  Mape_apac_sales_arima  
  # MAPE 25.37304

# ---------------------------------------------------------------------------------
# Modeling APAC consumer quantity data
  
# create time series for APAC consumer quanity
  consumer.apac.qty.ts <- ts(consumer.apac$Quantity.agg, frequency = 12, start=c(2011,1), end=c(2014,12))
  # plot the timeseries
  
# Let's Decompose the time series to get the trend and sesonal data out of it.
  decomposed.consumer.apac.qty.ts <- decompose(consumer.apac.qty.ts)
  plot(decomposed.consumer.apac.qty.ts) 
    # Looks to be an additive series with trend and seasonality
  
# Plot the time series
  plot(consumer.apac.qty.ts, xlab="Time",ylab="Quantity",lwd=2,col='blue', main = "Quanity - APAC Consumer Segment")
  
# smooth the time series
  smoothedseries_apac_qty <- stats::filter(consumer.apac.qty.ts, 
                                      filter=rep(1/3, 3), 
                                      method='convolution', sides=2)
  
# lets plot the line over the plot
  lines(smoothedseries_apac_qty, col="red", lwd=2)
    # we can see smoothed series showing upward trend
  
# add missing point due to smoothing the series
  smoothedseries_apac_qty[1] = smoothedseries_apac_qty[2] - (smoothedseries_apac_qty[3] - smoothedseries_apac_qty[2])
  smoothedseries_apac_qty[48] = smoothedseries_apac_qty[47] + (smoothedseries_apac_qty[47] - smoothedseries_apac_qty[46])
  
  lines(smoothedseries_apac_qty, col="red", lwd=2)
  
# lets create a dataframe out of the smoothseries
  smoothed.consumer.apac.qty <- data.frame(cbind(c(1:48), smoothedseries_apac_qty))
# rename the columns
  colnames(smoothed.consumer.apac.qty) <- c("Month", "Quantity")
  
# create a time series from the smooth dataframe
  smoothed.consumer.apac.qty.ts <- ts(smoothed.consumer.apac.qty$Quantity, frequency=12, start=c(2011,1), end=c(2014,12))  
  lines(smoothed.consumer.apac.qty.ts, col='green', lwd=2) 
  
# Let's create a test and train dataset, test dataset is last 6 months
  consumer.apac.qty.train <- window(smoothed.consumer.apac.qty.ts, start=c(2011,1), end=c(2014,6))
  consumer.apac.qty.test <- window(smoothed.consumer.apac.qty.ts, start=c(2014,7), end=c(2014,12))
  
# create and fit linear regression model to timeseries
  consumer.apac.qty.tslm <- tslm(consumer.apac.qty.train ~ trend+I(sin(0.5*trend/2))+season)
# lets summarize model
  summary(consumer.apac.qty.tslm)   
    # Adjusted R-squared:  0.8919
  
# Forecasting time series for next 6 months
  consumer.apac.qty.forecast <- forecast(consumer.apac.qty.tslm, h=6, level=0)
  plot(smoothed.consumer.apac.qty.ts, ylab="Quantity", xlab="Time", col="blue", lwd=2, main = "Quantity forcast -  APAC Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.apac.qty.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.apac.qty.forecast$mean, col="red", lwd=2)
  
# Accuracy measures for a forecast model
  consumer.apac.qty.accuracy <- accuracy(consumer.apac.qty.forecast$mean, consumer.apac.qty.test) 
  Mape_apac_qty_cd<- consumer.apac.qty.accuracy[5] 
  Mape_apac_qty_cd
    # MAPE 16.70772
  
# plot acf 
  acf(consumer.apac.qty.forecast$residuals,lag.max = 12) 
  acf(consumer.apac.qty.forecast$residuals,lag.max = 12, type="partial")
  adf.test(consumer.apac.qty.forecast$residuals, alternative = "stationary") 
  kpss.test(consumer.apac.qty.forecast$residuals) 
   #Seems that the residual is not a stationary series
  
# Fit best ARIMA model to univariate time series
  consumer.apac.qty.arima <- auto.arima(consumer.apac.qty.train)
  summary(consumer.apac.qty.arima)
    # Series: consumer.apac.qty.train 
    # ARIMA(1,0,0)(1,1,0)[12] with drift 
  
    # Coefficients:
    #           ar1     sar1   drift
    #        0.5875  -0.5076  6.9552
    #  s.e.  0.1483   0.1681  1.0765
  
    # sigma^2 estimated as 1759:  log likelihood=-155.07
    # AIC=318.15   AICc=319.75   BIC=323.75
  
    # Training set error measures:
    #                   ME     RMSE      MAE        MPE     MAPE      MASE      ACF1
    #Training set 0.237675 33.62645 21.58811 -0.5828135 5.349074 0.2552791 0.1464213
  
  
# Test for stationary
  adf.test(consumer.apac.qty.arima$residuals, alternative = "stationary") 
    # p-value = 0.08798 (>0.05) - Fails to reject the alternate hypothesis that residual is stationary
  kpss.test(consumer.apac.qty.arima$residuals) 
    # p-value = 0.1 (>0.05)- Fails to reject the alternate hypothesis that residual is not stationary
    #??? using this adf and kpss test we see that the residual is stationary series
  
# Forecasting time series for next 6 months
  consumer.apac.qty.arima.forecast <- forecast(consumer.apac.qty.arima, h=6)
  plot(smoothed.consumer.apac.qty.ts, ylab="Quantity", xlab="Time", col="blue",lwd=4,main = "Quantity forcast -  APAC Consumer Segment")  
  axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
  lines(consumer.apac.qty.arima.forecast$residual, lwd=2,col="yellow",lty=3)
  lines(consumer.apac.qty.arima.forecast$fitted, lwd=2,col="green",lty=3)
  lines(consumer.apac.qty.arima.forecast$mean, col="red",lwd=2)
  
# Accuracy measures for a forecast model
  consumer.apac.qty.arima.accuracy <- accuracy(consumer.apac.qty.arima.forecast, consumer.apac.qty.test)
  Mape_apac_qty_arima<- consumer.apac.qty.arima.accuracy[2,5]
  Mape_apac_qty_arima
  # MAPE 16.783094
  
# ---------------------------------------------------------------------------------
# conclusions
# ---------------------------------------------------------------------------------
  Mape_table <-data.frame()
  Mape_table <-rbind(c(Mape_eu_sales_cd,Mape_eu_sales_arima),c(Mape_eu_qty_cd,Mape_eu_qty_arima))
  Mape_table <-rbind(Mape_table,c(Mape_apac_sales_cd,Mape_apac_sales_arima),c(Mape_apac_qty_cd,Mape_apac_qty_arima))
  row.names(Mape_table)<-c("EU Sales", "EU Quantiy", "APAC Sales", "APAC Quantity")
  colnames(Mape_table)<-c("Classical Decomposition","Auto ARIMA")
  View(Mape_table)
    #               Classical Decomposition Auto ARIMA
    # EU Sales                     7.960089   27.11146
    # EU Quantiy                  13.636080   19.28809
    # APAC Sales                  11.697209   25.37304
    # APAC Quantity               16.707719   16.78309
  
    # it is noticed that classical decomposition is performing better. hence using the models 
    # generated using Classical decomposition for forecastig future values
  
# Forcast future sales, i.e. 2015 Jan to 2015 June, using classical decomposition
  consumer.eu.sales.forecast_new <- forecast(consumer.eu.sales.tslm, h=12, level=0)
  plot(consumer.eu.sales.forecast_new, ylab="Sales", xlab="Time", col="blue", lwd=2, main = "Sales forcast -  EU Consumer Segment")
  axis(1,at=seq(2011,2015,1))
  lines(smoothed.consumer.eu.sales.ts, lwd=1,col="red") 

# Forcast future quantity, i.e. 2015 Jan to 2015 June, using classical decomposition
  consumer.eu.qty.forecast_new <- forecast(consumer.eu.qty.tslm, h=12, level=0)
  plot(consumer.eu.qty.forecast_new, ylab="Quantity", xlab="Time", col="blue", lwd=2, main = "Quantity forcast -  EU Consumer Segment")
  axis(1,at=seq(2011,2015,1))
  lines(smoothed.consumer.eu.qty.ts, lwd=1,col="red") 


# Forcast future sales, i.e. 2015 Jan to 2015 June, using classical decomposition
  consumer.apac.sales.forecast_new <- forecast(consumer.apac.sales.tslm, h=12, level=0)
  plot(consumer.apac.sales.forecast_new, ylab="Sales", xlab="Time", col="blue", lwd=2, main = "Sales forcast -  APAC Consumer Segment")
  axis(1,at=seq(2011,2015,1))
  lines(smoothed.consumer.apac.sales.ts, lwd=1,col="red") 

# Forcast future quantity, i.e. 2015 Jan to 2015 June, using classical decomposition
  consumer.apac.qty.forecast_new <- forecast(consumer.apac.qty.tslm, h=12, level=0)
  plot(consumer.apac.qty.forecast_new, ylab="Quantity", xlab="Time", col="blue", lwd=2, main = "Quantity forcast -  APAC Consumer Segment")
  axis(1,at=seq(2011,2015,1))
  lines(smoothed.consumer.apac.qty.ts, lwd=1,col="red") 

  Forecasted.Values<-data.frame(matrix(NA, nrow = 12, ncol = 0))
  Forecasted.Values<-cbind(Forecasted.Values,consumer.eu.sales.forecast_new$mean, consumer.apac.sales.forecast_new$mean,consumer.eu.qty.forecast_new$mean, consumer.apac.qty.forecast_new$mean)

# need only data from Jan to June of 2015
  Forecasted.Values<-Forecasted.Values[7:12,]
  colnames(Forecasted.Values)<- c("EU Sales", "APAC Sales", "EU Qty","APAC Qty")
  row.names(Forecasted.Values)<-c("Jan 2015", "Feb 2015", "Mar 2015", "Apr 2015", "May 2015", "Jun 2015")

  View(Forecasted.Values)
  
