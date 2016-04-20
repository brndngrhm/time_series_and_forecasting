#TS Project Script 2.0

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

#data prep ----
  
#read enplanement data, format and combine

x.2007 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2007.csv")
data.2007 <- read.csv(text = x.2007)

x.2008 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2008.csv")
data.2008 <- read.csv(text = x.2008)

x.2009 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2009.csv")
data.2009 <- read.csv(text = x.2009)

x.2010 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2010.csv")
data.2010 <- read.csv(text = x.2010)

x.2011 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2011.csv")
data.2011 <- read.csv(text = x.2011)

x.2012 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2012.csv")
data.2012 <- read.csv(text = x.2012)

x.2013 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2013.csv")
data.2013 <- read.csv(text = x.2013)

x.2014 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2014.csv")
data.2014 <- read.csv(text = x.2014)

x.2015 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2015.csv")
data.2015 <- read.csv(text = x.2015)


#data.2007 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2007.csv")
#data.2008 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2008.csv")
#data.2009 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2009.csv")
#data.2010 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2010.csv")
#data.2011 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2011.csv")
#data.2012 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2012.csv")
#data.2013 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2013.csv")
#data.2014 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2014.csv")
#data.2015 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2015.csv")

data.2007[5] <- NULL
data.2007$year <- "2007"

data.2008[5] <- NULL
data.2008$year <- "2008"

data.2009[5] <- NULL
data.2009$year <- "2009"

data.2010[5] <- NULL
data.2010$year <- "2010"

data.2011[5] <- NULL
data.2011$year <- "2011"

data.2012[5] <- NULL
data.2012$year <- "2012"

data.2013[5] <- NULL
data.2013$year <- "2013"

data.2014[5] <- NULL
data.2014$year <- "2014"

data.2015[5] <- NULL
data.2015$year <- "2015"

pax <- rbind(data.2007, data.2008, data.2009, data.2010, data.2011, data.2012, data.2013, data.2014, data.2015)
names(pax) <- tolower(names(pax))
phl <- pax %>% filter(origin == "PHL") %>% group_by(year, month) %>% summarise("pax" = sum(passengers))
phl$month2 <- month(phl$month, label = TRUE)
phl$month <- NULL
names(phl)[3] <- "month"
phl$date <- paste(phl$month, "1,", phl$year, sep=" ")
phl$date <- mdy(phl$date)
#write.csv(phl, file = "~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.csv")

#emp 
x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_monthly_emp.csv")
emp <- read.csv(text = x.emp)
#emp <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_monthly_emp.csv"))

emp <-  melt(emp, id.vars = c("Year"))
names(emp)[1] <- "year"
names(emp)[2] <- "month"
names(emp)[3] <- "emp"
emp$year <- as.character(emp$year)
emp$emp <- emp$emp * 1000
emp$month2[emp$month == "January"] <- 1
emp$month2[emp$month == "February"] <- 2
emp$month2[emp$month == "March"] <- 3
emp$month2[emp$month == "April"] <- 4
emp$month2[emp$month == "May"] <- 5
emp$month2[emp$month == "June"] <- 6
emp$month2[emp$month == "July"] <- 7
emp$month2[emp$month == "August"] <- 8
emp$month2[emp$month == "September"] <- 9
emp$month2[emp$month == "October"] <- 10
emp$month2[emp$month == "November"] <- 11
emp$month2[emp$month == "December"] <- 12
emp$month <- month(emp$month2, label = TRUE)
emp$month2 <- NULL
emp <- emp %>% group_by(year, month) %>% summarise("emp" = sum(emp))
emp <- emp[-c(105, 106, 107, 108), ]

#earnings
x.earn <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_avg_wk_earn.csv")
earnings <- read.csv(text = x.earn)
#earnings <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_avg_wk_earn.csv"))

earnings <-  melt(earnings, id.vars = c("Year"))
names(earnings)[1] <- "year"
names(earnings)[2] <- "month"
names(earnings)[3] <- "earnings"
earnings$year <- as.character(earnings$year)
earnings$earnings <- earnings$earnings * 4
earnings$month2[earnings$month == "January"] <- 1
earnings$month2[earnings$month == "February"] <- 2
earnings$month2[earnings$month == "March"] <- 3
earnings$month2[earnings$month == "April"] <- 4
earnings$month2[earnings$month == "May"] <- 5
earnings$month2[earnings$month == "June"] <- 6
earnings$month2[earnings$month == "July"] <- 7
earnings$month2[earnings$month == "August"] <- 8
earnings$month2[earnings$month == "September"] <- 9
earnings$month2[earnings$month == "October"] <- 10
earnings$month2[earnings$month == "November"] <- 11
earnings$month2[earnings$month == "December"] <- 12
earnings$month <- month(earnings$month2, label = TRUE)
earnings$month2 <- NULL
earnings <- earnings %>% group_by(year, month) %>% summarise("earnings" = sum(earnings))
earnings <- earnings[-c(105, 106, 107, 108), ]

#join data and save .rda
socio <- left_join(emp, earnings, by = c("year", "month"))
phl <- left_join(phl, socio, by = c("year", "month"))
phl$year <- as.factor(phl$year)
phl <- phl %>% select(date, year, month, pax, emp, earnings)
#save(phl, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda")
write.csv(phl, file = "~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.csv")

#exploring ----

#format pax, emp and earnings as monthly time series w/frequency 12
pax <- ts(phl$pax, frequency = 12)
emp <- ts(phl$emp, frequency = 12)
earnings <- ts(phl$earnings, frequency = 12)

#time series plots of each variable, all show evidence of non stationarity (trend, heterskedasticity, seasonality)
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_line() + geom_smooth(se = F) + 
  labs(title = "Passengers\n"))

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + 
  geom_point() + geom_line()+ geom_smooth(se = F) + 
  labs(title = "Employment\n"))

(earnings.plot <- ggplot(phl, aes(x=date, y=earnings)) + 
  geom_point() + geom_line() + geom_smooth(se = F) + 
  labs(title = "Earnings\n"))

#ACF plots of each variable
acf2(pax, max.lag = 80)
acf2(earnings, max.lag = 80)
acf2(emp, max.lag = 80)

#Since  there is both trend and seasonality apparent in each series, need to difference twice for Trend and Seasonality
dl.pax <- diff(log(pax), 1)
dl.emp <- diff(log(emp), 1)
dl.earnings <- diff(log(earnings), 1)

dl12.pax <- diff(dl.pax, 12)
dl12.emp <- diff(dl.emp, 12)
dl12.earnings <- diff(dl.earnings, 12)

#ts plots and ACF plots of new variables
plot(dl12.pax, type = "o")
plot(dl12.emp, type = "o")
plot(dl12.earnings, type = "o")

acf2(dl12.pax, max.lag = 80)
acf2(dl12.earnings, max.lag = 80)
acf2(dl12.emp, max.lag = 80)

#fitting a SARIMA model for PAX alone ----

#checking seasonal and other part fit using aic matrix, row=AR=P, col=MA=Q, and fitting a SARIMA model
uplim=4
aicmat.pax <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax[i+1,j+1]=sarima(log(pax),0,1,0,i,1,j,12,details=F,tol=0.001)$AIC
    print(aicmat.pax)}}

aicmat.pax2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax2[i+1,j+1]<-sarima(log(pax), i, 1, j, 0, 1, 2, 12, details=F, tol=0.001)$AIC
    print(aicmat.pax2)}}

(model <- sarima(log(pax), 2, 1, 3, 0, 1, 1, 12, detail = FALSE))

#SARIMA forecast
sarima.for(log(pax), 2, 2, 1, 3, 0, 1, 1, 12)

#regression with arma errors ----

#scatterplot matrix
scatterplotMatrix(~ pax + earnings + emp, data = phl)

#looking at scatterplots of variables vs each other
(pax <- ggplot(phl, aes(x=date, y=pax)) + geom_point() + geom_line() +  geom_smooth(se = F))
(pax.emp <- ggplot(phl, aes(x=emp, y=pax)) + geom_point() + geom_smooth(se = F))
(pax.earnings <- ggplot(phl, aes(x=earnings, y=pax)) + geom_point() + geom_smooth(se = F))

#creating data frame to use for regression w/ arma errors
pax <- ts(phl$pax, frequency = 12)
time <- 1:104
time2 <- time^2
emp <- ts(phl$emp, frequency = 12)
emp <- phl$emp - mean(phl$emp)
emp2 <- emp^2
earnings <- phl$earnings/1000
earnings <- earnings - mean(earnings)
earnings2 <- earnings^2
pax2 <- data.frame(cbind(pax, time, time2, emp, emp2, earnings, earnings2))

#1st regression model using all variables, not a lot of significant variables
lm <- lm(log(pax) ~ ., data = pax2)
summary(lm)

#trying variable selection techniques
step(lm, direction = "forward")
step(lm, direction = "backward")
step(lm, direction = "both")

#second regression model based on variable selection techniques
lm2 <-  lm(log(pax) ~ earnings + earnings2, data = pax2)
summary(lm2)

#saving residuals, plotting, shows evidnce of seasonality
resids <- (lm2$residuals)
plot(resids, type="o")
acf2(resids,  max.lag = 85)

#aic matrix for seasonal part
uplim <- 4
aicmat.resids <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids[i+1,j+1]=sarima(resids,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC
    print(aicmat.resids)}}

#Trying to fit the other part. !!stops working halfway throuh for some reason??
uplim <- 4
aicmat.resids2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids2[i+1,j+1]<-sarima(resids,i,1,j,0,1,1,12,details=F,tol=0.001)$AIC
    print(aicmat.resids2)}}

#SARIMA residual model
sarima(resids,1,1,0,0,1,1,12,details = FALSE)

#Regression w arma erorrs (https://www.otexts.org/fpp/9/1)
#september monthly earnings = 958.42*4=3833.68
#october monthly earnings = 958.04*4=3832.16

forecast.earnings <- as.data.frame(c(3833.68+14697102, 3832.16+14685450))
forecast.earnings$month <- c(9,10)

(fit <- Arima(log(pax), xreg=pax2[,6:7], order=c(1,1,0), seasonal = c(0,1,1)))

(forecast <- forecast.Arima(fit, h=1, xreg = forecast.earnings))

reg.arma.pred <-as.data.frame(c(1127622, 1127622))
names(reg.arma.pred)[1] <- "Reg w/ ARMA Pred"
reg.arma.pred$month <- c(9,10)

#classical decompisition ----
pax2 <- log(pax[1:96])
t=seq(2007,2014.917,by=1/12)
t2 <- t^2
plot(t,pax2,type='l') #Seems to be nearly linear with period-12 seasonal component.
season=rep(1:12,8)
out=lm(pax2~t+t2+factor(season))
plot(out$residuals,type='l')
acf2(out$residuals)

dat=out$residuals
uplim=4
aicmat=matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat[i+1,j+1]=arima(dat,order=c(i,0,j))$aic}}
aicmat #First version

dat=out$residuals
uplim=4
aicmat2=matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]=sarima(dat,i,0,j,details=F)$AIC}}
aicmat2 #Second version


#Fitting ARMA(4,4).
sarima(dat,4,0,4,details=F)

#Doing the forecasting (for 2 months ahead)
beta=out$coeff
xmat=matrix(double(12*14),12,14)
xmat[,1]=1
xmat[,2]=seq(2007,2014.917,length=12)
xmat[,3]=seq(2007,2014.917,length=12)^2
xmat[1,4]=1
xmat[2,5]=1
xmat[3,6]=1
xmat[4,7]=1
xmat[5,8]=1
xmat[6,9]=1
xmat[7,10]=1
xmat[8,11]=1
xmat[9,12]=1
xmat[10,13]=1
xmat[11,14]=1
pred=xmat %*% beta
pred2=sarima.for(dat,12,4,0,4)
tot=pred+pred2$pred

#Plotting actual and fitted values.
plot(t,pax2)
lines(t,out$fitted.values)

#Adding things together to make the forecasts.
plot(t,pax2,xlim=c(2007,2016),type='l')
lines(xmat[,2],tot,lty=2)

#Forecasts with and without the ARMA part.
plot(xmat[,2],tot,lty=1,type='l') #With is the solid curve
lines(xmat[,2],pred,lty=2) #Without is the dashed curve

#Plotting just the ARMA predictions.
plot(pred2$pred)

#comparing predicted to actual ----
sarima.pred <- as.data.frame(as.numeric(exp(sarima.for(log(pax),2,1,1,3,1,1,1,12)$pred)))
names(sarima.pred)[1] <- "SARIMA Prediction"
sarima.pred$month <- c(9,10)

reg.arma.pred <-as.data.frame(c(1127622, 1127622))
names(reg.arma.pred)[1] <- "Reg w/ ARMA Pred"
reg.arma.pred$month <- c(9,10)

decomp.pred <- as.data.frame(c(965740.1, 1169363))
names(decomp.pred)[1] <- "Classical Decomposition Prediction"
decomp.pred$month <- c(9,10)

phl.new <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_new.csv")
names(phl.new) <- tolower(names(phl.new))
phl.new$month <- as.numeric(phl.new$month)
phl.new$origin <- as.character(phl.new$origin)
phl.new <- phl.new %>% dplyr::filter(origin == "PHL" & month > 8) %>% group_by(month) %>% summarise(pax = sum(passengers)) %>% ungroup()
names(phl.new)[2] <- "Actual Passengers"
names(phl.new)[1] <- "month"

pred.table <- left_join(sarima.pred, reg.arma.pred, by = "month")
pred.table <- left_join(pred.table, decomp.pred, by = "month")
pred.table <- left_join(pred.table, phl.new, by = "month")
pred.table$month <- as.factor(pred.table$month)

pred.table.melt<- melt(data.frame(pred.table), id = "month", measure.vars = )

(pred.plot <- ggplot(pred.table.melt, aes(x=month, y=value, color = variable)) + 
  geom_point(aes(size=5)) +
  labs(x="\nMonth", y="Passngers\n", title = "Actual and Predicted Passengers\n"))

#ARCH/GARCH ----
acf2(dl12.pax)
model <- sarima(dl12.pax, 1,0,0)
acf2(resid(model$fit)^2) #pulling out and plotting squared residuals
fit1 <- garchFit(~arma(1,1) + garch(1,0), dl12.pax, trace = F) #fitting a GARCH model

#checking GARCH fit
summary(fit1)
sr <- fit1@residuals/fit1@sigma.t
plot(sr,type='l') #seem like white noise
acf2(sr) # a few significant lags
acf2(sr^2) #doesn't seem to show signs of ARCH

#lagged variable regression ----

pax <- ts(phl$pax, frequency = 12)
emp <- ts(phl$emp, frequency = 12)
earnings <- ts(phl$earnings, frequency = 12)

#cross correllation plots
earnings.ccf <- ccf(dl12.pax, dl12.earnings) #6 month, 10 months
emp.ccf <- ccf(dl12.pax, dl12.emp) #14 months
