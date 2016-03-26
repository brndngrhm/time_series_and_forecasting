#hw9

library(astsa)
library(tsa3)

#2

a <- sqrt(.6785)
arma.spec(ma=c(1.75, 0, -.5, -.25), log = "no", main = "Spectral Density Plot")

w <- seq(from = -100, to = 100, by=.001)

plot(w, (1-(.5*cos(6*pi*w))-(cos(4*pi*w))), type = "l", xlim=c(-.6, .6))

w <- seq(from = -100, to = 100, by=.001)

plot(w, (1-(.5*cos(6*pi*w))-(cos(4*pi*w))), type = "l", xlim=c(-.6, .6))

#4
plot(w, (3+(2*cos(4*pi*w))+(4*cos(2*pi*w))), type = "l", xlim=c(0, .5))

arma.spec(ma = c(1,1), var.noise = 1, log="no", n.freq=500)

#5
arma.spec(ar = c(0, 0, .99), log="no")


omega<-0.4
n<-30
x<-1:n
y1<-cos(2*pi*x*omega)
y2<-cos(2*pi*x*(1-omega))

par(mfrow=c(2,1))
plot(x,y1,type='o')
plot(x,y2,type='o')

#What if we use a finer grid of time points?

x2<-seq(1,n,by=0.1)
y11<-cos(2*pi*x2*omega)
y22<-cos(2*pi*x2*(1-omega))