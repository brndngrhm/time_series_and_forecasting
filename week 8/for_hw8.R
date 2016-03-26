library(astsa)

load("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.rda")
pax <- phl$pax
pax2 <- pax - mean(pax)
n <- length(pax2)
I <- abs(fft(pax2))^2/104
p <- (4/104)*I[1:(104/2)]
f <- 0:51/104
plot(f,p,type='l',xlab='Frequency',ylab='Scaled Periodogram of Monthly Passengers')

#4.1

x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
plot.ts(x, ylim=c(-16,16), main="sum")

#4.2
dev.off()
P = Mod(2*fft(x)/100)^2
Fr = 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")

#1
library(astsa)
unemp <- unemp

t=seq(1948,1978.917, by=1/12)
plot(t,log(unemp),type='l') #Seems to be nearly linear with period-12 seasonal component.

plot(diff(unemp), type = "o")

#Here we create indicator variables for the seasonal components.

n=length(unemp)
t2=t*t
season=rep(1:12,31)

out <- lm(log(unemp)~t+t2+factor(season))
plot(out$residuals,type='l')
acf2(out$residuals)

#Looking at the AIC.
dat=out$residuals
uplim=4
aicmat=matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat[i+1,j+1]=arima(dat,order=c(i,0,j))$aic}}
aicmat #First version

dat=out$residuals
uplim=4
aicmat2=matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]=sarima(dat,i,0,j,details=F)$AIC}}
aicmat2 #Second version

#Fitting ARMA(3,2).

sarima(dat, 3, 0, 2, 0, 0, 0, details = FALSE)

#Doing the forecasting (for 12 months ahead)

beta=out$coeff
xmat=matrix(double(12*14),12,14)
xmat[,1]=1
xmat[,2]=seq(1948,1979.917,length=12)
xmat[,3]=seq(1948,1979.917,length=12)^2
xmat[1,4]=1
xmat[2,5]=1
xmat[3,6]=1
xmat[4,7]=1
xmat[5,8]=1
xmat[6,9]=1
xmat[7,10]=1
xmat[8,11]=1
xmat[9,12]=1
xmat[10,13]=1
xmat[11,14]=1
pred=xmat %*% beta
pred2=sarima.for(dat, 12, 3, 0, 2)
tot=pred+pred2$pred

#Plotting actual and fitted values.
plot(t,log(unemp))
lines(t,out$fitted.values)

#Adding things together to make the forecasts.
plot(t,log(unemp),xlim=c(1948,1980),ylim=c(5,6.9),type='l')
lines(xmat[,2],tot,lty=2)

#Forecasts with and without the ARMA part.
plot(xmat[,2],tot,lty=1,type='l') #With is the solid curve
lines(xmat[,2],pred,lty=2) #Without is the dashed curve

#Plotting just the ARMA predictions.
plot(pred2$pred)

plot(decompose(log(unemp)))


pred2=sarima.for(log(unemp), 12, 3, 1, 2, 0, 1, 0, 12)

