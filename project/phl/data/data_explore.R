#time series exploration

library(ggplot2)
library(dplyr)
library(scales)

load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")

#formatting as time series
phl.ts <- ts(phl)
plot(phl.ts, type = "o")

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