#time series exploration

library(ggplot2)
library(dplyr)

load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")

#formatting as time series
phl.ts <- ts(phl)
plot(phl.ts, type = "o")

#some plots

pax.plot <- ggplot(phl, aes(x=date, y=pax)) + 
  geom_point() + geom_line() + 
  scale_x_datetime(breaks = date_breaks("1 year"), minor_breaks = date_breaks("1 month")) + 
  labs(x="/nDate", y="Enplanements/n", title = "PHL Monthly Enplanements: 2007-2015\n")

(emp.plot <- ggplot(phl, aes(x=date, y=emp)) + geom_point() + geom_line() + scale_x_datetime(breaks = date_breaks("1 year")))

(earn.plot <- ggplot(phl, aes(x=date, y=earnings)) + geom_point() + geom_line() + scale_x_datetime(breaks = date_breaks("1 year")))
