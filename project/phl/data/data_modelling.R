#modelling script

#packages ----
library(ggplot2)
library(dplyr)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)
library(tidyr)
library(reshape)
library(RCurl)
library(rvest)
library(forecast)

#load data and exploration script ----
#for work
load("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda")

#for home
#load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")

source("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/data_explore.R")

#modelling pax alone using SARIMA ----

#ts plot
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
   geom_point() + geom_line() + 
   labs(x= "", y= "Enplanements\n", title = "PHL Monthly Enplanements: 2007-2015\n") + 
   scale_x_datetime(breaks = date_breaks("1 year"), labels=date_format("%Y")) +
   theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
   theme(plot.title=element_text(size=22)) +
   theme(axis.text.x=element_text(size=16)) +
   theme(axis.text.y=element_text(size=16)) +
   theme(axis.title.y=element_text(size=20, vjust=1.5)) +
   theme(axis.title.x=element_text(size=18, vjust=-.5)) + 
   theme(text=element_text(family="Georgia")) + 
   scale_y_continuous(labels=comma))

#scatterplot to check for trend, slight negative trend present
(scatter.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_smooth())

# differencing 1 round to remove trend and plotting both original and differenced data
diff.pax <- diff(pax, differences = 1)
par(mfrow = c(2, 1))
plot(pax, type="o", main = "Original Pax Data")
plot(diff.pax, type="o", main = "Differenced Pax Data") #looks de-trended

par(mfrow = c(2, 1))
acf(pax, lag = 80)
acf(diff.pax, lag = 80)

acf2(pax, max.lag = 80)
acf2(diff.pax, max.lag = 80)

#eliminating seasonal differences and plotting
diff12.pax <- diff(diff.pax, 12)

par(mfrow = c(3,1))
plot(pax, type="o", main = "Original Pax Data")
plot(diff.pax, type="o", main = "Differenced Pax Data")
plot(diff12.pax, type="o", main = "Seasonally Differenced (12)  Pax Data")

par(mfrow = c(3, 1))
acf(pax, lag = 80)
acf(diff.pax, lag = 80)
acf(diff12.pax, lag = 80)

acf2(pax, max.lag = 80)
acf2(diff.pax, max.lag = 80)
acf2(diff12.pax, max.lag = 80)

#fitting a sarima modelbased on acf & pacf plots, sarima(data, p, d, q, P, D, Q, S, details = FALSE)
(model.1 <- (sarima(pax, 2, 1, 3, 0, 1, 1, 12, detail = FALSE)))

#trying log(pax), fit seems betterand aic is lower
(model.1a <- (sarima(log(pax), 2, 1, 3, 0, 1, 1, 12, detail = FALSE)))

#checking seasonal and other part fit using aic matrix, row=AR=P, col=MA=Q
uplim=4
aicmat.pax <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax[i+1,j+1]=sarima(log(pax),0,1,0,i,1,j,12,details=F,tol=0.001)$AIC
    print(aicmat.pax)}}

(model.1b <- (sarima(log(pax), 0, 1, 0, 0, 1, 1, 12, detail = FALSE)))

aicmat.pax2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax2[i+1,j+1]<-sarima(log(pax), i, 1, j, 0, 1, 1, 12, details=F, tol=0.001)$AIC
    print(aicmat.pax)}}

#fitting 2nd sarima model, sarima(data, p, d, q, P, D, Q, S, details = FALSE)
(model.2 <- (sarima(log(pax), 0, 1, 3, 1, 1, 4, 12, detail = FALSE)))
(model.1a <- (sarima(log(pax), 2, 1, 3, 0, 1, 1, 12, detail = FALSE)))

#forecast
sarima.for(log(pax), 120, 2, 1, 3, 0, 1, 1, 12)


#comparing to TAF
pred <- data.frame(sarima.for(pax, 120, 1, 1, 3, 1, 1, 1, 12)$pred)
names(pred)[1] <- "Prediction"
pred <- data.frame(tapply(pred$Prediction,cut(pred$Prediction,12),FUN=sum))
names(pred)[1] <- "Prediction"
pred$year <- c(2016:2027)

x.taf <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/taf.csv")
TAF <- read.csv(text = x.taf)
names(TAF)[2] <- "TAF"

pred <- left_join(pred, TAF, by = "year")
pred <- pred %>% select(year, Prediction, TAF)

pred <-  melt(pred, id.vars = c("year"))

(pred.plot <- ggplot(pred, aes(x=year, y=value, color = variable)) + 
  geom_point(size=3, alpha = .8) + geom_smooth(method="lm", se = FALSE) + 
  labs(x = "\nYear", y="Forecast Enplanements\n", title = "Forecast Comparison") + 
  theme_hc() + scale_color_tableau() + 
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=16)) +
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)) + 
  theme(text=element_text(family="Georgia")) + 
  scale_y_continuous(labels=comma) + 
  theme(legend.position = "right") + theme(legend.title = element_blank()))

#regression with ARIMA Errors ----

#scatterplot to check for trend, slight negative trend present
(scatter.plot <- ggplot(phl, aes(x=date, y=log(pax))) + 
   geom_point() + geom_smooth())

#scatterplot matrix
pairs(phl[, 3:6])

#regression model 1: t = β0 + β1t + β2xt+ β2xt + ϵt 
lm <- lm(pax ~ (month) + emp + earnings) #should I use date? or year? or month? need to format as time(year) or ts(month)?
summary(lm)
plot(lm)

#reggression model 2
lm2 <-  lm(pax~ ts(month) + earnings, data = phl) #should I use date? or year? or month? need to format as time(year) or ts(month)?
summary(lm2)
plot(lm2)

#saving residuals, plotting, differencing and plotting acf/pacf
resids <- ts(lm2$residuals, frequency=12)
plot(resids, type="o")

diff.resids <- diff(resids)
plot(diff.resids, type="o")

diff12.resids <- diff(diff.resids, 12)
plot(diff12.resids, type="o")

acf2(resids, max.lag = 100)
acf2(diff.resids, max.lag=100)
acf2(diff12.resids, max.lag = 90)

#initial residual model
(sarima.resids <- sarima(resids, 1, 1, 3, 0, 1, 1, 12, details = FALSE))

#checking number of seasonal terms
aicmat.resids <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids[i+1,j+1]<-sarima(diff12.resids, 0, 1, 0, i, 1, j, 12, details=F, tol=0.001)$AIC
    print(aicmat.resids)}}

#checking number of other terms
aicmat.resids2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids2[i+1,j+1]<-sarima(diff12.resids, i, 1, j, 0, 1, 2, 12, details=F, tol=0.001)$AIC
    print(aicmat.resids2)}}

#fitting 2nd sariam model, think 1st one is better
(sarima.resids2 <- sarima(resids, 1, 1, 2, 0, 1, 2, 12, details = FALSE))


fit <- auto.arima(phl$pax, xreg=cbind(phl$emp,phl$earnings))
summary(fit)
plot(fit)



