#time series exploration

library(ggplot2)
library(dplyr)
library(scales)
library(astsa)

load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")

#some plots
(pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_line() + 
  labs(x= "", y= "Enplanements\n", title = "PHL Monthly Enplanements: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")))

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + 
  geom_point() + geom_line() + 
  labs(x= "", y= "Employment\n", title = "PHL Monthly Employment: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")))

(earn.plot <- ggplot(phl, aes(x=date, y=earnings)) + geom_point() + geom_line() + 
  labs(x= "", y= "Earnings\n", title = "PHL Monthly Earnings: 2007-2015\n") + 
  scale_x_datetime(breaks = date_breaks("1 year")))

#formatting as time series
pax <- ts(phl$pax)
emp <- ts(phl$emp)
earnings <- ts(phl$earnings)

acf2(pax)
acf2(emp)
acf2(earnings)
