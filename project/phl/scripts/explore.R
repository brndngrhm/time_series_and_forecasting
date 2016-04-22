
#packages ----
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(RCurl)
library(rvest)
library(ggplot2)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)
library(reshape)
library(car)
library(forecast)
library(fGarch)

#data explore ----

#UNIVARIATE EXPLORATION
#format pax, emp and earnings as monthly time series w/frequency 12
pax <- ts(phl$pax, frequency = 12)
avg.pax <- ts(phl$avg.pax, frequency = 12)
emp <- ts(phl$emp, frequency = 12)
earnings <- ts(phl$earnings, frequency = 12)
price <- ts(phl$price, frequency = 12)

#time series plots of each variable, all show evidence of non stationarity (trend, heterskedasticity, seasonality)
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_line() + 
  labs(title = "Passengers\n"))

(avg.pax.plot <- ggplot(phl, aes(x=date, y=avg.pax)) + 
  geom_point() + geom_line() + 
  labs(title = "Avg Daily Passengers\n"))

(fuel.plot <- ggplot(phl, aes(x=date, y=price)) + 
  geom_point() + geom_line() + 
  labs(title = "Fuel\n"))

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + 
  geom_point() + geom_line() +
  labs(title = "Employment\n"))

(earnings.plot <- ggplot(phl, aes(x=date, y=earnings)) + 
  geom_point() + geom_line() + 
  labs(title = "Earnings\n"))

#ACF plots of pax
acf2(pax, max.lag = 80)

#Since  there is both trend and seasonality apparent in each series, need to difference twice for Trend and Seasonality
dl.pax <- diff(log(pax), 1)
dl12.pax <- diff(dl.pax, 12)

#ts plots and ACF plots of new pax
plot(dl12.pax, type = "o")
acf2(dl12.pax, max.lag = 80)


#BIVARIATE EXPLORATION

#scatterplot matrix
scatterplotMatrix(~ avg.pax + price + earnings + emp, data = phl)

#looking at scatterplots of variables vs each other
(pax.scatter <- ggplot(phl, aes(x=date, y=pax)) + geom_point() + geom_line() +  geom_smooth(se = F))
(pax.emp <- ggplot(phl, aes(x=emp, y=pax)) + geom_point() + geom_smooth(se = F))
(pax.earnings <- ggplot(phl, aes(x=earnings, y=pax)) + geom_point() + geom_smooth(se = F))
(pax.price <- ggplot(phl, aes(x=price, y=pax)) + geom_point() + geom_smooth(se = F))

#CCF plots, need to make white noise first
acf2(pax, max.lag = 80)
acf2(avg.pax, max.lag = 80)
acf2(price, max.lag = 80)
acf2(earnings, max.lag = 80)
acf2(emp, max.lag = 80)

dl.avg.pax <- diff(log(avg.pax), 1)
dl12.avg.pax <- diff(dl.avg.pax, 12)
acf2(dl12.avg.pax, max.lag = 80)

dl.price <- diff(log(price), 1)
acf2(dl.price, max.lag = 80)

d.earnings <- diff((earnings))
acf2(d.earnings, max.lag = 80)

d.emp <- diff(emp)
d12.emp <- diff(d.emp, 12)

#cross correllation plots, doesnt show a lot of promise
earnings.ccf <- ccf(dl12.pax, d.earnings) #6 month, 10 months
emp.ccf <- ccf(dl12.pax, d12.emp) #14 months
price.ccf<- ccf(dl12.pax, dl.price) #6 month, 10 months


