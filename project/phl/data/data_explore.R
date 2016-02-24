#time series exploration

#packages ----
library(ggplot2)
library(dplyr)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)

#load data ----
#for work
load("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda")

#for home
#load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")

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

#formatting as time series ----

#time series and acf plots
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

#time series and acf plots of transformed data
log.pax <- ts(log(phl$pax), frequency = 12)
log.emp <- ts(log(phl$emp), frequency = 12)
earnings2 <- ts(phl$earnings/1000, frequency = 12)
par(mfrow = c(2, 3))
plot(log.pax, type = "o", main = "Log Enplanements")
plot(log.emp, type = "o", main = "Log Employment")
plot(earnings2, type = "o", main = "Earnings (000's")
acf(log.pax, main ='')
acf(log.emp, main ='')
acf(earnings2, main ='')

#time series and acf plots of differenced data
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

#acf and pacf plots of differenced data
par(mfrow = c(2, 3))
acf2(diff.log.pax)
acf2(diff.log.emp)
acf2(diff.earnings2)

