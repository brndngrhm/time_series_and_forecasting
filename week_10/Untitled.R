#hw10

library(astsa)
library(fGarch)

#5.6
gnpgr <- diff(log(gnp)) #converts to growth rate
plot(gnpgr, type='l') #seems like white noise process
acf2(gnpgr) #AR 1 seems like a good fit
model <- sarima(gnpgr, 1,0,0)
acf2(resid(model$fit)^2) #pulling out and plotting squared residuals
fit1 <- garchFit(~arma(1,0) + garch(1,0), gnpgr, trace = F) #fitting a GARCH model

#5.7
diff.log.oil <- diff(log.oil) #this is the growth rate
plot(diff.log.oil) #now looks like white noise
acf2(diff.log.oil)

#finding smallest aic
uplim<-4
aicmat2<-matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]<-arima(diff.log.oil,order=c(i,0,j))$aic}}
print(aicmat2)

#fitting model
model2 <- sarima(diff.log.oil,4,0,4,0,0,0,details = FALSE) #looks ok
resids <- resid(model2$fit)
acf2(resids^2) #evidence of GARCH behavior

#trying some model w/trial and error
garchFit(~arma(4,4) + garch(1,0), diff.log.oil, trace = F)

fit2 <- garchFit(~arma(4,4) + garch(1,1), diff.log.oil, trace = F) #seems to be the best one

garchFit(~arma(4,4) + garch(1,2), diff.log.oil, trace = F) 
garchFit(~arma(4,4) + garch(2,0), diff.log.oil, trace = F)
garchFit(~arma(4,4) + garch(2,1), diff.log.oil, trace = F) 
garchFit(~arma(4,4) + garch(3,0), diff.log.oil, trace = F)
garchFit(~arma(4,4) + garch(3,1), diff.log.oil, trace = F) 
garchFit(~arma(4,4) + garch(3,2), diff.log.oil, trace = F) 
garchFit(~arma(4,4) + garch(4,0), diff.log.oil, trace = F) 

#checking model fit and plotting standardized residuals
summary(fit2)
sr <- fit2@residuals/fit2@sigma.t
plot(sr,type='l') #seem like white noise
acf2(sr) # a few significant lags
acf2(sr^2) #doesn't seem to show signs of ARCH

#5.11
diff.sales <- diff(sales)
diff.lead <- diff(lead, lag = 3)

lm <- lm(diff.sales[1:147] ~ diff.lead)
resids3 <- lm$residuals
plot(resids3, type = "l")
acf2(resids3) #maybe AR(2) model will work

#modeling residuals
arima(resids3, order=c(2,0,0)) #looks ok

#modelling regression with arma errors
arima(diff.sales[1:147], order=c(2,0,0), xreg = diff.lead)

#checking model residuals
resids4<-model3.2$residuals
acf2(resids4)
