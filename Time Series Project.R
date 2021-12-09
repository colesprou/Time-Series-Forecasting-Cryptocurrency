library(astsa)
library(fGarch)
library(quantmod)
library(forecast)
library(tidyverse)
library(rugarch)

get_crypto_data <- function(ticker_and_USD,conversion_currency='USD',start_date,end_date) {
  start = as.Date(start_date)
  end = as.Date(end_date)
  df = getSymbols(ticker_and_USD,src='yahoo',from=start,to=end,auto.assign = F)
  colnames(df) = c("open_price","highest_price",'lowest_price','closing_price','volume','adjusted')
  return(df)
  }
btc = get_crypto_data(ticker='BTC-USD',start_date='2020-11-29',end_date='2021-11-29')
btc
head(btc)

## Arima Model:
par(mfrow=c(1,1))
plot(btc$adjusted,main='Closing Price of Bitcoin')
par(mfrow=c(2,1))
acf1(btc$adjusted , max.lag = 50,main='ACF of Bitcoin')# ACF plot
acf1(btc$adjusted , max.lag = 50,main='PACF of Bitcoin',pacf=T)
# log transformed series
par(mfrow=c(2,1))
lbtc = log(btc$adjusted)
plot(lbtc,main='Log BTC Closing Price')
acf1(lbtc,max.lag = 50, main='ACF of Log of Bitcoin')
# log returns 
ldbtc = diff(log(btc$adjusted))
plot(ldbtc,main='Log Returns BTC')
acf1(ldbtc,max.lag=50,main='ACF of Log Returns')
## orders
orders = auto.arima(log.fdiff.btc)$coef
print(orders)
arima.fit = sarima(na.omit(lbtc) , 0, 1 , 0)
sarima.for(na.omit(lbtc) , n.ahead = 20 , p = 0 , d = 1, q =0, plot.all = TRUE)
## ARMA-Garch
par(mfrow=c(2,1))
plot(btc$adjusted)
acf(na.omit(btc$adjusted))
rt = na.omit(diff(log(btc$adjusted)))
rt2 = na.omit(diff(log(btc$adjusted)))^2
par(mfrow=c(2,1))
acf1(rt2,max.lag=20)
acf1(rt2,max.lag=20,pacf= T)

plot.xts(rt,main='Log Returns BTC')

par(mfrow=c(2,1))
acf1(rt,max.lag = 20)
acf1(rt,max.lag=20,pacf=TRUE) 


arch.fit = garchFit(~arma(0,0)+garch(1,1),data = rt, cond.dist = "std")
summary(arch.fit)
v2 = predict(arch.fit)
## done with bitcoin
## Ethereum:
eth = get_crypto_data(ticker='ETH-USD',start_date='2020-11-29',end_date='2021-11-29')
head(eth)

plot(eth$adjusted,main='Closing Price of Ethereum')
par(mfrow=c(2,1))
plot(log(eth$adjusted))
acf1(log(eth$adjusted) , max.lag = 50,main='ACF of Log Ethereum')
acf1(log(eth$adjusted) , max.lag = 50,main='PACF of Ethereum',pacf=T)

rt = na.omit(diff(log(eth$adjusted)))

par(mfrow=c(3,1))

plot.xts(rt,main='Log Returns ETH')
acf1(rt , max.lag = 20,main='ACF of Returns ETH') # ACF plot
acf1(rt , pacf = TRUE,main= 'PACF of Returns ETH') # PACF plot
orders = auto.arima(rt)$coef ## 1,0
print(orders) 

arima.fit.eth = sarima(log(eth$adjusted),1,1,0)
sarima.for(na.omit(eth$adjusted),p=1,d=1,q=0,n.ahead=20,lty=1)

## ARMA GARCH
par(mfrow=c(2,1))
plot(eth$adjusted)
acf(na.omit(eth$adjusted))
rt = na.omit(diff(log(eth$adjusted)))
rt2 = na.omit(log(diff(eth$adjusted)))^2
par(mfrow=c(2,1))
acf1(rt2,max.lag=20)
acf1(rt2,max.lag=20,pacf= T)
plot.xts(rt,main='Log Returns ETH')

par(mfrow=c(2,1))
acf1(rt,max.lag = 20)
acf1(rt,max.lag=20,pacf=TRUE) 


arch.fit = garchFit(~arma(1,0)+garch(1,1),data = rt, cond.dist = "std")
summary(arch.fit)
v = predict(arch.fit,10)
v













#arch.fit = garchFit(~arma(0,0)+garch(1,1),data = log.fdiff.btc, cond.dist = "std")
#summary(arch.fit)
#v = predict(arch.fit,10)
rt2 = log.fdiff.btc^2
acf1(rt2 , max.lag = 20) # ACF plot
acf1(rt2 , pacf = TRUE) # PACF plot


rt = na.omit(diff(log(btc$adjusted)))
par(mfrow=c(3,1))
plot.xts(rt,main='Log Returns BTC')
acf1(rt , max.lag = 20) # ACF plot
acf1(rt , pacf = TRUE) # PACF plot

#components.ts = decompose(mr)
#plot(components.ts)
# Fit to the differenced log transformed series 
orders = auto.arima(rt)$coef ## 1,0
print(orders) #AIC=-1147.62 BIC=-1140.01

arima.fit = sarima(na.omit(log(btc$adjusted)) , 1 , 1 , 0)
# two-weeks ahead steps ahead forecasting
par(mfrow=c(1,1))
sarima.for(na.omit(log(btc$adjusted)) , n.ahead = 20 , p = 1 , d = 1, q =0, plot.all = TRUE)
sarima.for(na.omit(diff(log(btc$adjusted))) , n.ahead = 20 , p = 0 , d = 1, q =0, plot.all = TRUE)

v
plot(predict(arch.fit))


## Ethereum:
eth = get_crypto_data(ticker='ETH-USD',start_date='2020-11-29',end_date='2021-11-29')
head(eth)

plot(eth$adjusted,main='Closing Price of Ethereum')
par(mfrow=c(2,1))
plot(log(eth$adjusted))
acf1(log(eth$adjusted) , max.lag = 50,main='ACF of Log Ethereum')
acf1(eth$adjusted , max.lag = 50,main='PACF of Ethereum',pacf=T)

rt = na.omit(diff(eth$adjusted))

rt = na.omit(diff(log(eth$adjusted)))
par(mfrow=c(3,1))

plot.xts(rt,main='Returns ETH')
acf1(rt , max.lag = 20,main='ACF of Returns ETH') # ACF plot
acf1(rt , pacf = TRUE,main= 'PACF of Returns ETH') # PACF plot
orders = auto.arima(rt)$coef ## 1,0
print(orders) # ARIMA(2,1,1) AIC=-1147.62 BIC=-1140.01

arima.fit.eth = sarima(eth$adjusted,1,1,0)
sarima.for(na.omit(eth$adjusted),p=1,d=1,q=0,n.ahead=20,lty=1)

## ARMA GARCH
par(mfrow=c(2,1))
plot(eth$adjusted)
acf(na.omit(eth$adjusted))
rt = na.omit(diff(eth$adjusted))
#rt2 = na.omit(diff(log(eth$adjusted)))^2
par(mfrow=c(2,1))
acf1(rt2,max.lag=20)
acf1(rt2,max.lag=20,pacf= T)
plot.xts(rt,main='Log Returns ETH')

par(mfrow=c(2,1))
acf1(rt,max.lag = 20)
acf1(rt,max.lag=20,pacf=TRUE) 


arch.fit = garchFit(~arma(1,0)+garch(1,1),data = rt, cond.dist = "std")
summary(arch.fit)
v = predict(arch.fit,10)
v
(4239.981-575.781)/575.781
head(btc)
(57806.57-18177.48)/18177.48*100

