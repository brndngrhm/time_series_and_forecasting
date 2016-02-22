#PHL

#packages ----
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(RCurl)
library(rvest)

#read enplanement data, format and combine ----

x.2007 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2007.csv")
data.2007 <- read.csv(text = x.2007)

x.2008 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2008.csv")
data.2008 <- read.csv(text = x.2008)

x.2009 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2009.csv")
data.2009 <- read.csv(text = x.2009)

x.2010 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2010.csv")
data.2010 <- read.csv(text = x.2010)

x.2011 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2011.csv")
data.2011 <- read.csv(text = x.2011)

x.2012 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2012.csv")
data.2012 <- read.csv(text = x.2012)

x.2013 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2013.csv")
data.2013 <- read.csv(text = x.2013)

x.2014 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2014.csv")
data.2014 <- read.csv(text = x.2014)

x.2015 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2015.csv")
data.2015 <- read.csv(text = x.2015)


#data.2007 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2007.csv")
#data.2008 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2008.csv")
#data.2009 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2009.csv")
#data.2010 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2010.csv")
#data.2011 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2011.csv")
#data.2012 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2012.csv")
#data.2013 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2013.csv")
#data.2014 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2014.csv")
#data.2015 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2015.csv")

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
phl <- pax %>% filter(origin == "PHL") %>% group_by(year, month) %>% summarise("pax" = sum(passengers))
phl$month2 <- month(phl$month, label = TRUE)
phl$month <- NULL
names(phl)[3] <- "month"
phl$date <- paste(phl$month, "1,", phl$year, sep=" ")
phl$date <- mdy(phl$date)
#write.csv(phl, file = "~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.csv")

#emp ----

x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_monthly_emp.csv")
emp <- read.csv(text = x.emp)
#emp <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_monthly_emp.csv"))

emp <-  melt(emp, id.vars = c("Year"))
names(emp)[1] <- "year"
names(emp)[2] <- "month"
names(emp)[3] <- "emp"
emp$year <- as.character(emp$year)
emp$emp <- emp$emp * 1000
emp$month2[emp$month == "January"] <- 1
emp$month2[emp$month == "February"] <- 2
emp$month2[emp$month == "March"] <- 3
emp$month2[emp$month == "April"] <- 4
emp$month2[emp$month == "May"] <- 5
emp$month2[emp$month == "June"] <- 6
emp$month2[emp$month == "July"] <- 7
emp$month2[emp$month == "August"] <- 8
emp$month2[emp$month == "September"] <- 9
emp$month2[emp$month == "October"] <- 10
emp$month2[emp$month == "November"] <- 11
emp$month2[emp$month == "December"] <- 12
emp$month <- month(emp$month2, label = TRUE)
emp$month2 <- NULL
emp <- emp %>% group_by(year, month) %>% summarise("emp" = sum(emp))
emp <- emp[-c(105, 106, 107, 108), ]

#earnings ----
x.earn <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_avg_wk_earn.csv")
earnings <- read.csv(text = x.earn)
#earnings <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_avg_wk_earn.csv"))

earnings <-  melt(earnings, id.vars = c("Year"))
names(earnings)[1] <- "year"
names(earnings)[2] <- "month"
names(earnings)[3] <- "earnings"
earnings$year <- as.character(earnings$year)
earnings$earnings <- earnings$earnings * 4
earnings$month2[earnings$month == "January"] <- 1
earnings$month2[earnings$month == "February"] <- 2
earnings$month2[earnings$month == "March"] <- 3
earnings$month2[earnings$month == "April"] <- 4
earnings$month2[earnings$month == "May"] <- 5
earnings$month2[earnings$month == "June"] <- 6
earnings$month2[earnings$month == "July"] <- 7
earnings$month2[earnings$month == "August"] <- 8
earnings$month2[earnings$month == "September"] <- 9
earnings$month2[earnings$month == "October"] <- 10
earnings$month2[earnings$month == "November"] <- 11
earnings$month2[earnings$month == "December"] <- 12
earnings$month <- month(earnings$month2, label = TRUE)
earnings$month2 <- NULL
earnings <- earnings %>% group_by(year, month) %>% summarise("earnings" = sum(earnings))
earnings <- earnings[-c(105, 106, 107, 108), ]

#join data and save .rda ----
socio <- left_join(emp, earnings, by = c("year", "month"))
phl <- left_join(phl, socio, by = c("year", "month"))
phl$year <- as.factor(phl$year)
phl <- phl %>% select(date, year, month, pax, emp, earnings)
save(phl, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/phl/data/phl.rda") 

