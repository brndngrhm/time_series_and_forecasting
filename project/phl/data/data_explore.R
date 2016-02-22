#time series exploration

library(ggplot2)
library(dplyr)
library(scales)
library(astsa)

#for work
load("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda")

#for home
#load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")


#some plots
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_line() + 
  labs(x= "", y= "Enplanements\n", title = "PHL Monthly Enplanements: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)))

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + 
  geom_point() + geom_line() + 
  labs(x= "", y= "Employment\n", title = "PHL Monthly Employment: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)))

(earn.plot <- ggplot(phl, aes(x=date, y=earnings)) + geom_point() + geom_line() + 
  labs(x= "", y= "Earnings\n", title = "PHL Estimated Monthly Earnings: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + theme_hc()  +
  theme(plot.title=element_text(size=22)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12)) +
  theme(axis.title.y=element_text(size=20, vjust=1.5)) +
  theme(axis.title.x=element_text(size=18, vjust=-.5)))


#formatting as time series
phl.ts <- phl
phl.ts[1] <- NULL
phl.ts[1] <- NULL
phl.ts[1] <- NULL
phl.ts <- ts(phl.ts, frequency = 12)

#base time series plots
plot(phl.ts, type="o")

#acf plot amtrix
acf(phl.ts)

