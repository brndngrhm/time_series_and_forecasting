#https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/

library(TTR)
library(astsa)

#2----
pax <- read.table("http://www19.homepage.villanova.edu/jesse.frey/Math8444/airline2.txt", 
                  header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
names(pax)[1] <- "pax"
pax <- ts(pax)
plot(pax, type = "p")

#this uses slightly different code to replicate the ex on p.71
cmort <- ts(cmort)
mov.avg5 <- SMA(cmort, n=5)
mov.avg53 <- SMA(cmort, n=53)
plot(cmort, type="p", ylab="mortality")
lines(mov.avg5, col = "red")
lines(mov.avg53, col = "blue")

#Moving Average
mov.avg4 <-SMA(pax, n=4) #uses TTR package
mov.avg12 <-SMA(pax, n=12)
plot(pax, type="p")
lines(mov.avg4, col = "red")
lines(mov.avg12, col = "blue")

#Polynomial and Periodic Regression Smoothers
month <- time(pax) - mean(time(pax))
month2 <- month^2
month3 <- month^3
cos <- cos(2*pi*month)
sin <- sin(2*pi*month)
reg1 <- lm(pax~month + month2 + month3, na.action=NULL)
reg2 <- lm(pax~month + month2 + month3 + cos + sin, na.action=NULL)
plot(pax, type="p", ylab="Pax")
lines(fitted(reg1), col="red")
lines(fitted(reg2), col="blue")

#Kernel Smoothing
plot(pax, type="p", ylab="mortality")
lines(ksmooth(time(pax), pax, "normal", bandwidth=5/12), col= "red")
lines(ksmooth(time(pax), pax, "normal", bandwidth=12), col="blue")

#Lowess and Nearest Neighbor Regression
par(mfrow=c(2,1))
plot(pax, type="p", ylab="pax", main="nearest neighbor")
lines(supsmu(time(pax), pax, span=.5),col="red")
lines(supsmu(time(pax), pax, span=.01),col="blue")

plot(pax, type="p", ylab="pax", main="lowess")
lines(lowess(pax, f=.02), col = "red")
lines(lowess(pax, f=2/3), col = "blue")

#Smoothing Splines
dev.off()
plot(pax, type="p", ylab="pax")
lines(smooth.spline(time(pax), pax), col="red")
lines(smooth.spline(time(pax), pax, spar=1), col="blue")

#all 6 plots
par(mfrow=c(2,3))
#moving avg
plot(pax, type="p", main = "moving average")
lines(mov.avg4, col = "red")
lines(mov.avg12, col = "blue")

#Polynomial and Periodic Regression Smoothers
plot(pax, type="p", ylab="Pax", main = "polynomial")
lines(fitted(reg1), col="red")
lines(fitted(reg2), col="blue")

#kernel
plot(pax, type="p", ylab="mortality", main="kernel smoothing")
lines(ksmooth(time(pax), pax, "normal", bandwidth=5/12), col= "red")
lines(ksmooth(time(pax), pax, "normal", bandwidth=12), col="blue")

#lowess & NN
plot(pax, type="p", ylab="pax", main="nearest neighbor")
lines(supsmu(time(pax), pax, span=.5),col="red")
lines(supsmu(time(pax), pax, span=.01),col="blue")

plot(pax, type="p", ylab="pax", main="nearest neighbor")
lines(supsmu(time(pax), pax, span=.5),col="red")
lines(supsmu(time(pax), pax, span=.01),col="blue")

#smoothing splines
plot(pax, type="p", ylab="pax", main="Smoothing splines")
lines(smooth.spline(time(pax), pax), col="red")
lines(smooth.spline(time(pax), pax, spar=1), col="blue")


#3----
acf(hw3_3$series1, main = "ACF of Series 1")
acf(hw3_3$series2, main = "ACF of Series 2")
acf(hw3_3$series3, main = "ACF of Series 3")
acf(hw3_3$series4, main = "ACF of Series 4")


