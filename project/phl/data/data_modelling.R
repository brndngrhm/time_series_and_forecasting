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

# modelling pax alone

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

acf2(pax, main ='', max.lag = 100)

log.diff.pax <- diff(log(pax), differences = 1)
acf(diff.pax, lag = 100)
acf2(diff.pax,  max.lag = 100)

log.diff12.pax <- diff(log(pax), differences = 12)
acf(diff12.pax, lag = 95)
acf2(diff12.pax,  max.lag = 95)

(sarima(diff12.pax, 0, 1, 5, 2, 1, 2, 12))
sarima()

sarima()