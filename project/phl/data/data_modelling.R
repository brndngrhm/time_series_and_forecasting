#modelling script

#packages ----
library(ggplot2)
library(dplyr)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)

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

#fitting a sarima model, sarima(data, p, d, q, P, D, Q, S, details = FALSE)
(model.1 <- (sarima(pax, 1, 1, 3, 1, 1, 1, 12, detail = FALSE)))

#checking using aic matrix8

#forecast
sarima.for(pax, 120, 1, 1, 3, 1, 1, 1, 12)


#comparing to TAF
pred <- data.frame(sarima.for(pax, 120, 1, 1, 3, 1, 1, 1, 12)$pred)
names(pred)[1] <- "pred"
pred <- data.frame(tapply(pred$pred,cut(pred$pred,12),FUN=sum))
names(pred)[1] <- "pred"
pred$year <- c(2016:2027)

x.taf <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/taf.csv")
taf <- read.csv(text = x.taf)

pred <- left_join(pred, taf, by = "year")
pred <- pred %>% select(year, pred, taf)

(pred.plot <- ggplot(pred, aes(year)) + 
  geom_line(aes(y = pred/1000000, colour = "Prediction")) + 
  geom_line(aes(y = taf/1000000, colour = "TAF")) + coord_fixed(ratio=1))

(pred.scatter <- ggplot(pred, aes(x=year, y=pred/1000000)) + geom_point() + geom_smooth(se = FALSE, method = lm)) +coord_fixed(ratio = 1)
(taf.scatter <- ggplot(pred, aes(x=year, y=taf/1000000)) + geom_point() + geom_smooth(se = FALSE, method = lm))

lm1 <- lm(pred ~ year, data = pred)
summary(lm1)

lm2 <- lm(taf ~ year, data = pred)
summary(lm2)

#regression with ARIMA Errors ----

phl$year2 <- phl$year
phl$year2 <- as.character(phl$year2)
phl$year2 <- as.numeric(phl$year2)

lm <- lm(pax ~ time(month) + emp + earnings, data = phl) #should I use date? or year? or month? need to format as time(year) or ts(month)?
summary(lm)
plot(lm)

resids <- ts(lm$residuals, frequency=12)
plot(resids, type="o")

diff.resids <- diff(resids)
plot(diff.resids, type="o")

diff12.resids <- diff(diff.resids, 12)
plot(diff12.resids, type="o")

acf2(resids, max.lag = 100)
acf2(diff.resids, max.lag=100)
acf2(diff12.resids, max.lag = 90)

(sarima.resids <- sarima(resids, 1, 1, 3, 1, 1, 1, 12, details = FALSE))

lm.coeff <- data.frame(coefficients(lm))
sarima.coeff <- data.frame(c(0.6295, -1.3205, 0.5266, -0.1664, 0.2012, -0.9990))
