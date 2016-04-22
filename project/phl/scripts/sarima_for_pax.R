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

(model <- sarima(log(pax), 2, 1, 3, 0, 1, 1, 12, detail = FALSE))

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


#getting fitted values and plotting with original points and adding forecast looks good

#sarima fit
sarima.fit <- ts(fitted(arima(log(pax), order=c(2, 1, 3), seasonal = c(0, 1, 1))), frequency = 12)

#decomp fit
decomp.fit <- out$fitted.values

#stuff for plot
time <- seq(1, 104)
time.decomp <- seq(1:96)
time.actual <- seq(1:106)
forecast.time <- seq(104, 106)
actual.pax <- c(14.0242, 13.89314, 13.97053)

#various predicted values from models
sarima.pred <- c(14.0242, 13.83949, 13.92725)
decomp.pred <- c(14.0242, 13.92797, 13.83861)

#plot
plot(time, log(pax), type = "p", xlim=c(80, 108))
abline(v=time[104])

#adding SARIMA fitted line
lines(time, sarima.fit, col="red", type ="l")

#adding decomp fitted line
lines(time.decomp, decomp.fit, col="darkgreen", type="l")

#adding actual Pax
points(105, 13.89314, col="deepskyblue2")
points(106, 13.97053, col="deepskyblue2")
lines(forecast.time, actual.pax, type="l", col="deepskyblue2", lwd=2)

#SARIMA predictions
points(105,13.83949, col="green3", pch=24)
points(106, 13.92725, col="green3", pch=24)
lines(forecast.time, sarima.pred, type="l", col="green3", lwd=2)

#Decomp predictions
points(105,13.92797, col="darkgreen", pch=24)
points(106, 13.83861, col="darkgreen", pch=24)
lines(forecast.time, decomp.pred, type="l", col="darkgreen", lwd=2)

