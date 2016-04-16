#PHL data importing and prep

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



----------------------------
----------------------------
#PART I: DATA PREP
----------------------------
----------------------------

#read enplanement data, format and combine ----

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

#emp ----

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

#earnings ----
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

#join data and save .rda ----
socio <- left_join(emp, earnings, by = c("year", "month"))
phl <- left_join(phl, socio, by = c("year", "month"))
phl$year <- as.factor(phl$year)
phl <- phl %>% select(date, year, month, pax, emp, earnings)
save(phl, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda") 



----------------------------
----------------------------
#PART II: DATA EXPLORATION
----------------------------
----------------------------
  
#some plots ----
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

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + 
  geom_point() + geom_line() + 
  labs(x= "", y= "Employment\n", title = "PHL Monthly Employment: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year"), labels=date_format("%Y")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=16)) +
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)) + 
  theme(text=element_text(family="Georgia")) + 
  scale_y_continuous(labels=comma))

(earn.plot <- ggplot(phl, aes(x=date, y=earnings)) + geom_point() + geom_line() + 
  labs(x= "", y= "Earnings\n", title = "PHL Estimated Monthly Earnings: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year"), labels=date_format("%Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=16)) +
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)) + 
  theme(text=element_text(family="Georgia")) + 
  scale_y_continuous(labels=comma))

#formatting as time series and plotting ----

#time series and acf plots of pax, emp, earnings
pax <- ts(phl$pax, frequency = 12)
emp <- ts(phl$emp, frequency = 12)
earnings <- ts(phl$earnings, frequency = 12)
par(mfrow = c(2, 3))
plot(pax, type = "o", main = "Enplanements")
plot(emp, type = "o", main = "Employment")
plot(earnings, type = "o", main = "Earnings")
acf(pax, main ='')
acf(emp, main ='')
acf(earnings, main ='')

#time series and acf plots of transformed data (log pax, emp, earnings)
log.pax <- log(pax)
log.emp <- log(emp)
earnings2 <- earnings/1000
par(mfrow = c(2, 3))
plot(log.pax, type = "o", main = "Log Enplanements")
plot(log.emp, type = "o", main = "Log Employment")
plot(earnings2, type = "o", main = "Earnings (000's")
acf(log.pax, main ='')
acf(log.emp, main ='')
acf(earnings2, main ='')

#time series and acf plots of differenced data to remove trend
diff.log.pax <- diff(log.pax)
diff.log.emp <- diff(log.emp)
diff.earnings2 <- diff(earnings2)
par(mfrow = c(2, 3))
plot(diff.log.pax, type = "o", main = "Diff Log Enplanements")
plot(diff.log.emp, type = "o", main = "Diff Log Employment")
plot(diff.earnings2, type = "o", main = "Diff Earnings (000's")
acf(log.pax, main ='')
acf(log.emp, main ='')
acf(earnings2, main ='')

#time series and acf plots of differenced data to remove seasonality
diff12.log.pax <- diff(diff.log.pax, 12)
diff12.log.emp <- diff(diff.log.emp, 12)
diff12.earnings2 <- diff(diff.earnings2, 12)
par(mfrow = c(2, 3))
plot(diff12.log.pax, type = "o", main = "Diff(12) Log Enplanements")
plot(diff12.log.emp, type = "o", main = "Diff(12) Log Employment")
plot(diff12.earnings2, type = "o", main = "Diff(12) Earnings (000's")
acf(diff12.log.pax, main ='')
acf(diff12.log.emp, main ='')
acf(diff12.earnings2, main ='')

#acf and pacf plots of data, original, transformed, differnced 1, differenced 12 ----
acf2(pax, max.lag = 100)
acf2(emp, max.lag = 100)
acf2(earnings,max.lag = 100)

acf2(log.pax, max.lag = 100)
acf2(log.emp,max.lag = 100)
acf2(earnings2, max.lag = 100)

acf2(diff.log.pax, max.lag = 90)
acf2(diff.log.emp, max.lag = 90)
acf2(diff.earnings2, max.lag = 90)

acf2(diff12.log.pax, max.lag = 90)
acf2(diff12.log.emp, max.lag = 90)
acf2(diff12.earnings2, max.lag = 90)



----------------------------
----------------------------
#PART III: DATA MODELlING
----------------------------
----------------------------

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
plot(diff12.pax, type="o", main = "Seasonally Differenced (12) Pax Data")

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
pred <- as.data.frame(as.numeric(exp(sarima.for(log(pax), 120, 1, 1, 3, 1, 1, 1, 12)$pred))) # do i need to exponentiate??
names(pred)[1] <- "Prediction"
pred$year <- c(1,1,1,1,1,1,1,1,1,1,1,1,
               2,2,2,2,2,2,2,2,2,2,2,2,
               3,3,3,3,3,3,3,3,3,3,3,3,
               4,4,4,4,4,4,4,4,4,4,4,4,
               5,5,5,5,5,5,5,5,5,5,5,5,
               6,6,6,6,6,6,6,6,6,6,6,6,
               7,7,7,7,7,7,7,7,7,7,7,7,
               8,8,8,8,8,8,8,8,8,8,8,8,
               9,9,9,9,9,9,9,9,9,9,9,9,
               10,10,10,10,10,10,10,10,10,10,10,10)
pred <- pred %>% select(Prediction, year) %>% group_by(year) %>% summarise(Prediction = sum(Prediction))
pred[1] <- NULL
pred$year <- c(2016:2025)

x.taf <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/taf.csv")
TAF <- read.csv(text = x.taf)
names(TAF)[2] <- "TAF"
TAF <- as.data.frame(TAF %>% filter(year <= 2025))

pred <- left_join(pred, TAF, by = "year")
pred <- pred %>% select(year, Prediction, TAF)

pred2 <- melt(pred, id.vars = "year", measure.vars = c("Prediction", "TAF"))

(pred.plot <- ggplot(pred2, aes(x=year, y=Prediction, color = Value) + 
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
                       theme(legend.position = "right") + theme(legend.title = element_blank())))

#regression with ARIMA Errors ----

#format data
pax <- phl$pax
time <- 1:104
time2 <- time^2
emp <- phl$emp - mean(phl$emp)
emp2 <- emp^2
earnings <- phl$earnings - mean(phl$earnings)
earnings2 <- earnings^2
pax2 <- data.frame(cbind(pax, time, time2, emp, emp2, earnings, earnings2))

#plot of pax vs earnings, suggests earnings^2 would be good to use as well
ggplot(pax2,aes(x=earnings,y=pax)) + geom_point() + geom_smooth()

#plot of pax vs earnings, 
ggplot(pax2,aes(x=emp,y=pax)) + geom_point() + geom_smooth()

lm <- lm(log(pax) ~ ., data = pax2)
summary(lm)
plot(lm)

step(lm, direction = "forward")
step(lm, direction = "backward")
step(lm, direction = "both")

lm2 <-  lm(log(pax) ~ earnings + earnings2, data = pax2)
summary(lm2)
plot(lm2)

#saving residuals, plotting, differencing and plotting acf/pacf
resids <- (lm2$residuals)
plot(resids, type="o")
acf2(resids,  max.lag = 85)

diff.resids <- diff(resids)
plot(lm$residuals, type="l")
acf2(diff.resids)

diff12.resids <- diff(diff.resids, 12)
plot(diff12.resids, type="o")
acf2(diff12.resids, max.lag = 85)

#initial residual model

#aic matrix for seasonal part
uplim <- 4
aicmat2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]=sarima(resids,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC
    print(aicmat2)}}

#Trying to fit the other part. !!stops working halfway throuh for some reason??
uplim <- 4
aicmat <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat[i+1,j+1]<-sarima(resids,i,1,j,1,1,1,12,details=F,tol=0.001)$AIC
    print(aicmat)}}

sarima(resids,0,1,1,1,1,1,12,details = FALSE)

#fitting model with arima errors

#Model gives an error when t and t2 are included but is ok without them
#model also gives error with log(pax), not sure of the reason for either of these errors
#log(pax) works with 1 xreg variable at a time, but when when use cbind()

sarima(log(pax), 0,1,1,1,1,1,12, details = FALSE, xreg = cbind(earnings, earnings2))
