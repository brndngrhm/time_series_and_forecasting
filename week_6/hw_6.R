#hw_6
library(ggplot2)
library(astsa)


#2
#read in data and plot
wins <- read.table("http://www19.homepage.villanova.edu/jesse.frey/Math8444/Phillies2015.txt", 
                   header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

names(wins)[1] <- "year"
names(wins)[2] <- "win"

(plot <- ggplot(wins, aes(x=year, y=win)) + geom_point() + geom_line() + 
  labs(x="Year", y="Win Percentage", title = "Phillies Win Percentage: 1901-2015", size = 14))

#format as time series
wins2 <- wins
wins2[1] <- NULL
wins2 <- ts(wins2, frequency = 1)


#plot acf and pacf
acf(wins2)
pacf(wins2)
