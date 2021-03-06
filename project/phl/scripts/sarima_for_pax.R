#SARIMA

#checking seasonal and other part fit using aic matrix, row=AR=P, col=MA=Q, and fitting a SARIMA model
uplim=4
aicmat.pax <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax[i+1,j+1]=sarima(log(pax),0,1,0,i,1,j,12,details=F,tol=0.001)$AIC}}

print(aicmat.pax)

aicmat.pax2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.pax2[i+1,j+1]<-sarima(log(pax), i, 1, j, 0, 1, 2, 12, details=F, tol=0.001)$AIC}}

print(aicmat.pax2)

(model <- sarima(log(pax), 2, 1, 3, 0, 1, 1, 12))

#checking ARCH/GARH behavior, seems ok
acf2(resid(model$fit)^2) #pulling out and plotting squared residuals

#SARIMA forecast
sarima.for(log(pax), 2, 2, 1, 3, 0, 1, 1, 12) #next 2 months forecast are 13.83949 and 13.92725


#getting actual phl pax for new months
x.phl.new <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_new.csv")
phl.new <- read.csv(text = x.phl.new)
names(phl.new) <- tolower(names(phl.new))
phl.new$month <- as.numeric(phl.new$month)
phl.new$origin <- as.character(phl.new$origin)
phl.new <- phl.new %>% dplyr::filter(origin == "PHL" & month > 8) %>% group_by(month) %>% summarise(pax = sum(passengers)) %>% ungroup()

#getting fitted values and plotting with original points and adding forecast:

#sarima fit
sarima.fit <- ts(fitted(arima(log(pax), order=c(2, 1, 3), seasonal = c(0, 1, 1))), frequency = 12)

#decomp fit
decomp.fit <- out$fitted.values

#reg.arma fit
reg.arma.fit <- fitted(fit)

#stuff for plot
time <- seq(1, 104)
time.decomp <- seq(1:96)
time.actual <- seq(1:106)
time.reg.arma <-  seq(1:103)
forecast.time <- seq(104, 106)
actual.pax <- c(14.0242, 13.89314, 13.97053)

#various predicted values from models
sarima.pred <- c(14.01201, 13.83949, 13.92725)
decomp.pred <- c(14.0242, 13.97036, 13.93802)
reg.pred <- c(13.95143, 13.80375, 13.76140)

#plot
plot(time, log(pax), type = "p", xlim=c(80, 108))
abline(v=time[104])

#adding SARIMA fitted line
lines(time, sarima.fit, col="red", type ="l")

#adding decomp fitted line
lines(time.decomp, decomp.fit, col="darkgreen", type="l")

#adding reg.arma fitted line
lines(time.reg.arma, reg.arma.fit, col="mediumpurple3", type="l")

#adding actual Pax
points(105, 13.89314, col="deepskyblue2")
points(106, 13.97053, col="deepskyblue2")
lines(forecast.time, actual.pax, type="l", col="deepskyblue2", lwd=2)

#SARIMA predictions
points(105, 13.83949, col="red", pch=24)
points(106, 13.92725, col="red", pch=24)
lines(forecast.time, sarima.pred, type="l", col="red", lwd=2)

#Decomp predictions
points(105, 13.97036, col="darkgreen", pch=24)
points(106, 13.93802, col="darkgreen", pch=24)
lines(forecast.time, decomp.pred, type="l", col="darkgreen", lwd=2)

#Reg w/ arma errors prediction
points(105, 13.80375, col="mediumpurple3", pch=24)
points(106, 13.76140, col="mediumpurple3", pch=24)
lines(forecast.time, reg.pred, type="l", col="mediumpurple3", lwd=2)

#comparing all fits vs actual

comparison <- data.frame(cbind(actual.pax, sarima.pred, decomp.pred, reg.pred))
comparison <- comparison[2:3,]
comparison$month <- c("September", "October")

comp.melt<- melt((comparison), id = "month")

(pred.plot <- ggplot(comp.melt, aes(x=variable, y=exp(value), color = variable)) + 
  geom_point(aes(size=5)) + facet_grid(.~variable) +
  labs(x="\nMonth", y="Passngers\n", title = "Actual and Predicted Passengers\n"))

#numerical differences

#SARIMA
#November
exp(13.89314) - exp(13.83949)
#october
exp(13.97053) - exp(13.92725)

#DEcomp
#November
exp(13.89314) - exp(13.97036)
#october
exp(13.97053) - exp(13.93802)

#Reg.arma
#November
exp(13.89314) - exp(13.80375)
#october
exp(13.97053) - exp(13.76140)

#various predicted values from models
sarima.pred <- c(14.01201, 13.83949, 13.92725)
decomp.pred <- c(14.0242, 13.97036, 13.93802)
reg.pred <- c(13.95143, 13.80375, 13.76140)

#plot showing actual points
plot(time, log(pax), type = "o", xlim=c(0, 108))
abline(v=time[104])
#adding actual Pax
points(105, 13.89314, col="deepskyblue2")
points(106, 13.97053, col="deepskyblue2")
lines(forecast.time, actual.pax, type="l", col="deepskyblue2", lwd=2)


