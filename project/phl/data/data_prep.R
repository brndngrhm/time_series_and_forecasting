#PHL

library(dplyr)
library(lubridate)

#read data, format and combine----
data.2007 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2007.csv")
data.2008 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2008.csv")
data.2009 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2009.csv")
data.2010 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2010.csv")
data.2011 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2011.csv")
data.2012 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2012.csv")
data.2013 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2013.csv")
data.2014 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2014.csv")
data.2015 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2015.csv")

data.2007[5] <- NULL
data.2007$year <- "2007"

data.2008[5] <- NULL
data.2008$year <- "2008"

data.2009[5] <- NULL
data.2009$year <- "2009"

data.2010[5] <- NULL
data.2010$year <- "2010"

data.2011[5] <- NULL
data.2011$year <- "2011"

data.2012[5] <- NULL
data.2012$year <- "2012"

data.2013[5] <- NULL
data.2013$year <- "2013"

data.2014[5] <- NULL
data.2014$year <- "2014"

data.2015[5] <- NULL
data.2015$year <- "2015"

pax <- rbind(data.2007, data.2008, data.2009, data.2010, data.2011, data.2012, data.2013, data.2014, data.2015)
names(pax) <- tolower(names(pax))
phl <- pax %>% filter(origin == "PHL") %>% group_by(year, month) %>% summarise("enp" = sum(passengers))
phl$month2 <- month(phl$month, label = TRUE)

write.csv(phl, "phl.csv")

