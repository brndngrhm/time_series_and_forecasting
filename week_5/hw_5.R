#hw 5

#acf for a----
a <- ARMAacf(ar=c(-0.2, 0.48), ma=1,lag.max=50)
plot(0:50,a,type='h',xlab='Lag')

#acf for c
c <- ARMAacf(ar=c(-0.6), ma=c(1, 1.2),lag.max=50)
plot(0:50,c,type='h',xlab='Lag')

#acf for d
d <- ARMAacf(ar=c(-1.8, -0.81), ma=1,lag.max=50)
plot(0:50,d,type='h',xlab='Lag')

#plot them all as they are on hw sheet
par(mfrow = c(3,1))
plot(0:50,a,type='h',xlab='Lag', main = "ACF of Part (a)")
plot(0:50,c,type='h',xlab='Lag', main = "ACF of Part (c)")
plot(0:50,d,type='h',xlab='Lag', main = "ACF of Part (d)")

dev.off()

#alt acf plots----

#acf for a

a <- ARMAacf(ar=c(1, 0.2, -0.48), ma = 1, lag.max=50)
plot(0:50,a,type='h',xlab='Lag')

#acf for c

c <- ARMAacf(ar=c(1, 0.6), ma=c(1, 1.2),lag.max=50)
plot(0:50,c,type='h',xlab='Lag')

#acf for d

d <- ARMAacf(ar=c(1, 1.8, 0.81), ma=c(1),lag.max=50)
plot(0:50,d,type='h',xlab='Lag')

#plot them all as they are in the fucntions theta(Z) and phi(z)
par(mfrow = c(3,1))
plot(0:50,a,type='h',xlab='Lag', main = "ACF of Part (a)")
plot(0:50,c,type='h',xlab='Lag', main = "ACF of Part (c)")
plot(0:50,d,type='h',xlab='Lag', main = "ACF of Part (d)")

dev.off()

#checking 1----
#a
p <- c(1, 0.2, -.48)
polyroot(p)

#b
p <- c(1, 1.9, .88)
polyroot(p)

p <- c(1, .2, .7)
polyroot(p)

#c
p <- c(1, .6)
polyroot(p)

p <- c(1, 1.2)
polyroot(p)

#d
p <- c(1, 1.8, .81)
polyroot(p)

#e
p <- c(1, 1.6)
polyroot(p)

p <- c(1, -.4, .04)
polyroot(p)




