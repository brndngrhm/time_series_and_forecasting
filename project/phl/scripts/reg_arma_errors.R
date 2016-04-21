#regression with ARMA Errors

#scatterplot matrix
scatterplotMatrix(~ avg.pax + price + earnings + emp, data = phl)

#looking at scatterplots of variables vs each other
(pax.scatter <- ggplot(phl, aes(x=date, y=pax)) + geom_point() + geom_line() +  geom_smooth(se = F))
(pax.emp <- ggplot(phl, aes(x=emp, y=pax)) + geom_point() + geom_smooth(se = F))
(pax.earnings <- ggplot(phl, aes(x=earnings, y=pax)) + geom_point() + geom_smooth(se = F))
(pax.price <- ggplot(phl, aes(x=price, y=pax)) + geom_point() + geom_smooth(se = F))

#creating data frame to use for regression w/ arma errors
pax <- ts(phl$pax, frequency = 12)
avg.pax <- ts(phl$avg.pax, frequency = 12)
price <- phl$price - mean(phl$price)
price2 <- price^2
time <- 1:104
time2 <- time^2
emp <- phl$emp - mean(phl$emp)
emp2 <- emp^2
earnings <- phl$earnings/1000
earnings <- earnings - mean(earnings)
earnings2 <- earnings^2
pax2 <- data.frame(cbind(avg.pax, time, time2, price, price2, emp, emp2, earnings, earnings2))

#1st regression model using all variables, not a lot of significant variables
lm <- lm(log(avg.pax) ~ ., data = pax2)
summary(lm)

#trying variable selection techniques
step(lm, direction = "forward")
step(lm, direction = "backward")
step(lm, direction = "both")

#second regression model based on variable selection techniques
lm2 <-  lm(log(avg.pax[2:104]) ~ time2[1:103] + price[1:103] + earnings[1:103] + earnings2[1:103])
summary(lm2)

#saving residuals, plotting, shows evidnce of seasonality
resids <- (lm2$residuals)
plot(resids, type="o")
acf2(resids,  max.lag = 85)

#aic matrix for seasonal part
uplim <- 4
aicmat.resids <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids[i+1,j+1]=sarima(resids,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC}}

print(aicmat.resids)

#Trying to fit the other part. !!stops working halfway throuh for some reason??
uplim <- 4
aicmat.resids2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids2[i+1,j+1]<-sarima(resids,i,1,j,0,1,2,12,details=F,tol=0.001)$AIC}}

print(aicmat.resids2)

#SARIMA residual model
sarima(resids,0,1,1,0,1,2,12,details = FALSE)

#Regression w arma erorrs (https://www.otexts.org/fpp/9/1)
#september monthly earnings = 958.42*4=3833.68
#october monthly earnings = 958.04*4=3832.16

regressors <- as.data.frame(cbind(price[1:103], earnings[1:103], earnings2[1:103]))

(fit <- Arima(log(avg.pax)[2:104], xreg=regressors, order=c(0,1,1), seasonal = c(0,1,2)))

(forecast <- forecast.Arima(fit, h=1, xreg = forecast.earnings))

reg.arma.pred <-as.data.frame(c(1127622, 1127622))
names(reg.arma.pred)[1] <- "Reg w/ ARMA Pred"
reg.arma.pred$month <- c(9,10)
