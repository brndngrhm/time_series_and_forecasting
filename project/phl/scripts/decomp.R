#classical decomposition

pax2 <- log(pax[1:96])
t=seq(2007, 2014.917,by=1/12)
t2 <- t^2
plot(t,pax2,type='l') #Seems to be nearly linear with period-12 seasonal component.
season=rep(1:12,8)
out=lm(pax2~t+t2+factor(season))
plot(out$residuals,type='l')
acf2(out$residuals)

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


#Fitting ARMA(4,4).
sarima(dat,4,0,4,details=F)

#Doing the forecasting (for 2 months ahead)
beta=out$coeff
xmat=matrix(double(12*14),12,14)
xmat[,1]=1
xmat[,2]=seq(2007,2014.917,length=12)
xmat[,3]=seq(2007,2014.917,length=12)^2
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
pred2=sarima.for(dat,12,4,0,4)
tot=pred+pred2$pred

#Plotting actual and fitted values.
plot(t,pax2)
lines(t,out$fitted.values)

#Adding things together to make the forecasts.
plot(t,pax2,xlim=c(2007,2016),type='l')
lines(xmat[,2],tot,lty=2)

#Forecasts with and without the ARMA part.
plot(xmat[,2],tot,lty=1,type='l') #With is the solid curve
lines(xmat[,2],pred,lty=2) #Without is the dashed curve

#Plotting just the ARMA predictions.
plot(pred2$pred)
