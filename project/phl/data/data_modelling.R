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

# modelling pax alone ----

#ts plot
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point(aes(color = month, size=4)) + geom_line(aes(alpha = .5, size =2)) + scale_color_discrete()+
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

#plot to check for trend, slight negative trend present
(scatter.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_smooth())

# differencing 1 round to remove trend and plotting both original and differenced data
diff.pax <- diff(pax, differences = 1)
par(mfrow = c(2, 1))
plot(pax, type="o", main = "Original Data")
plot(diff.pax, type="o", main = "Differenced Data")

par(mfrow = c(2, 1))
acf(pax, lag = 80)
acf(diff.pax, lag = 80)

acf2(pax, max.lag = 80)
acf2(diff.pax, max.lag = 80)

#eliminating seasonal differences and plotting
diff12.pax <- diff(diff.pax, 12)

par(mfrow = c(3,1))
plot(pax, type="o", main = "Original Data")
plot(diff.pax, type="o", main = "Differenced Data")
plot(diff12.pax, type="o", main = "Differenced (12) Data")

par(mfrow = c(3, 1))
acf(pax, lag = 80)
acf(diff.pax, lag = 80)
acf(diff12.pax, lag = 80)

acf2(pax, max.lag = 80)
acf2(diff.pax, max.lag = 80)
acf2(diff12.pax, max.lag = 80)

#fitting a sarima model, sarima(data, p, d, q, P, D, Q, S, details = FALSE)
(model.1 <- (sarima(pax, 1, 1, 2, 1, 1, 1, 12, detail = FALSE)))

#checking using aic matrix


