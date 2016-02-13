#hw 5

#acf for a

a <- ARMAacf(ar=c(-0.2, 0.48), ma = 1, lag.max=50)
plot(0:50,a,type='h',xlab='Lag')

#acf for c

c <- ARMAacf(ar=c(-0.6), ma=c(1, 1.2),lag.max=50)
plot(0:50,c,type='h',xlab='Lag')

#acf for d

d <- ARMAacf(ar=c(-1.8, -0.81), ma=c(1),lag.max=50)
plot(0:50,d,type='h',xlab='Lag')

#plot them all
par(mfrow = c(3,1))
plot(0:50,a,type='h',xlab='Lag', main = "ACF of Part (a)")
plot(0:50,c,type='h',xlab='Lag', main = "ACF of Part (c)")
plot(0:50,d,type='h',xlab='Lag', main = "ACF of Part (d)")

dev.off()