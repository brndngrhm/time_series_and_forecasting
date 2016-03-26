#hw9

library(astsa)
library(tsa3)

#2

a <- sqrt(.6785)
arma.spec(ma=c(1.75, 0, -.5, -.25), log = "no", main = "Spectral Density Plot")

w <- seq(from = -100, to = 100, by=.001)

plot(w, (1-(.5*cos(6*pi*w))-(cos(4*pi*w))), type = "l", xlim=c(-.6, .6), main = "Spectral Density Plot for #2")

#4
plot(w, (3+(2*cos(4*pi*w))+(4*cos(2*pi*w))), type = "l", xlim=c(0, .5))

arma.spec(ma = c(.999,.998), var.noise = 1, log="no", n.freq=500, main = "Spectral Density for #4")

#5
arma.spec(ar = c(0, 0, .99), log="no", main = "Spectral Density for #5")

 