#Fatal Police Shootings Script

#packages----
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RCurl)
library(rvest)
library(extrafont)
library(ggthemes)
library(grid)

#loads and formats data----

#shooting data
x <- getURL("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
data <- read.csv(text = x)
data$date <- ymd(data$date)
data$age <- as.numeric(data$age)
data$month <- month(data$date, label = TRUE)
data$week <- week(data$date)
data$year <- year(data$date)
data$year <- as.factor(data$year)
data$day <- wday(data$date, label = TRUE, abbr = TRUE)
data$count <- 1
data$age[is.na(data$age)] <- 0

#re-codes race as white, black hispanic, other
data$race2 <- "Other"
data$race2[data$race == "W"] <- "White"
data$race2[data$race == "B"] <- "Black"
data$race2[data$race == "H"] <- "Hispanic"
data$race2[data$race == " "] <- "Unknown"


#creates age buckets
data$age2 <- "Under 18"
data$age2[18 <= data$age & data$age < 30] <- "18 to 29"
data$age2[30 <= data$age & data$age < 45] <- "30 to 44"
data$age2[45 <= data$age] <- "45 and Older"
data$age2[data$age == 0] <- "Unknown"
data$age2 <- as.factor(data$age2)

#loads and checks structure
#View(data)
#str(data)

#state population data
y <- getURL("http://www.census.gov/popest/data/state/totals/2015/tables/NST-EST2015-01.csv", header = FALSE)
states <- read.csv(text = y)
states <- states[-c(1, 2,3, 4, 5, 6, 7, 8, 9, 61, 62, 63, 64, 65, 66, 67), -c(2,3,4,5,6,7,8)] #removes unwanted rows and columns, keeps only 2015 population and state name
names(states)[1] <- "state"
names(states)[2] <- "pop"
states$state <- sub('.', '', states$state) #removes "." at beginning of state names
states$pop <- gsub(',', '', states$pop) #removes commas from population column
states$state <- state.abb[match(states$state,state.name)] #adds state abbreviations
states[9,1] <- "DC" #Adds Washington DC abbreviation
states$state <- as.factor(states$state)
states$pop <- as.numeric(states$pop)

#loads and checks structure
#View(states)
#str(states)

#joins shooting and population data
data <- left_join(data, states, by = "state")
data$state <- as.factor(data$state)
#str(data)

#creates dataframe of list of dates to fill in date gaps
date.ref <- data.frame(date=seq(as.Date("2015-01-01"), as.Date("2016-01-16"), by="days"))
date.ref$date <- ymd(date.ref$date)

#joins data and date.ref dataframes and replaces na with 0's
data <- merge(data,date.ref,by.x='date',by.y='date',all.x=T,all.y=T)
data$count[is.na(data$count)] <- 0

#replaces NA in age 2 with NA, then makes a factor
data$race2[is.na(data$race2)] <- "NA"
data$race2 <- as.factor(data$race2)


#writes .csv file
#write.csv(data, file = "C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/data/shootings.csv")

#ggplot theme----

#Import and list fonts
#loadfonts(device="win")

#Fonts to plug into font.type variable
"Lucida Sans"
"Gil Sans MT"
"Verdana"
"Trebuchet MS"
"Georgia"
"Garamond"

#Global theme options - to easily all plots at once
font.type <- "Lucida Sans"
background.color <- "#f1f1f1"
line.color <- "#d8d8d8"
title.color <- "#3C3C3C"
title.size <- 22
axis.color <- "#535353"
axis.size <- 18

transparency <- .7 #for alpha
line.size <- 1.6 #for geom_line()
point.size <- 3 #for geom_point()

#theme
theme_bg <-theme(panel.background=element_rect(fill=background.color)) + 
  theme(plot.background=element_rect(fill=background.color)) +
  theme(panel.grid.major=element_line(colour=line.color,size=.60)) +
  theme(panel.grid.minor=element_line(colour=line.color,size=.05)) +
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(face="bold",vjust=2, hjust=-.07, colour=title.color,size=title.size)) +
  theme(axis.text.x=element_text(size=axis.size,colour=axis.color)) +
  theme(axis.text.y=element_text(size=axis.size,colour=axis.color)) +
  theme(axis.title.y=element_text(size=axis.size,colour=axis.color,vjust=1.5)) +
  theme(axis.title.x=element_text(size=axis.size,colour=axis.color,vjust=-.5)) +
  theme(text=element_text(family=font.type))

#theme options (to add plots inividually)

#to add bold line at y=0
geom_hline(yintercept=0,size=1.2,colour="#535353")

#to change plot margins
theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

#to get rid of legend
theme(legend.position="none")
guides(fill = FALSE)

#to format legend when it's needed
theme(legend.background = element_rect(fill=background.color)) + 
  theme(legend.key = element_rect(colour = background.color)) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")

#exploration and plots----
total <- sum(data$count) #total shootings
year <- data %>% filter(count > 0) %>% group_by(year) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by year
state <- data %>% filter(count > 0) %>% group_by(state) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #top 10 states
city <- data %>%filter(count > 0) %>% group_by(city) %>% summarise(total = sum(count)) %>% top_n(20) %>% arrange(desc(total)) #top 10 cities
race <- data %>%filter(count > 0) %>% group_by(race2) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by race
age <- data %>% filter(count > 0) %>% group_by(age2) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by race
mental.illness <- data %>%filter(count > 0) %>% group_by(signs_of_mental_illness) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by signs of mental illness
armed <- data %>% filter(count > 0) %>% group_by(armed) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by victim weapon type
threat.level <- data %>% filter(count > 0) %>% group_by(threat_level) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #shootings by threat level
manner.of.death <- data %>% filter(count > 0) %>% group_by(manner_of_death) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total)) #manner of death

#new dataframe of shootings by state per 100,000 
per.capita <- data %>% filter(count > 0) %>% select(state, count, pop) %>% group_by(state, pop) %>% summarise(total = sum(count)) %>% ungroup() %>% arrange(desc(total))
per.capita$pop.thousands <- per.capita$pop/100000
per.capita$per.cap.thousands <- per.capita$total/per.capita$pop.thousands
per.capita <- per.capita %>%  arrange(desc(per.cap.thousands))

(per.cap.plot <- ggplot(per.capita, aes(x=reorder(state, per.cap.thousands), y=per.cap.thousands, fill = race)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + labs(x="", y="Fatal Shootings by Police per 100,000 Residents") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/per.cap.plot.png")


(year.plot <- ggplot(year, aes(x=(reorder(year,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/year.plot.png", height=7, width=8)


(state.plot <- ggplot(state, aes(x=(reorder(state,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/state.plot.png", height=7, width=8)


(city.plot <- ggplot(city, aes(x=(reorder(city,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/city.plot.png", height=7, width=8)


(race.plot <- ggplot(race, aes(x=(reorder(race2,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/race.plot.png", height=7, width=8)


(age.plot <- ggplot(age, aes(x=(reorder(age2,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/age.plot.png", height=7, width=8)


(mental.illness.plot <- ggplot(mental.illness, aes(x=(reorder(signs_of_mental_illness,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/mental.illness.plot.png", height=7, width=8)


(armed.plot <- ggplot(armed, aes(x=(reorder(armed,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/armed.plot.png", height=7, width=8)



(threat.plot <- ggplot(threat.level, aes(x=(reorder(threat_level,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal Shootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/threat.plot.png", height=7, width=8)


(manner.plot <- ggplot(manner.of.death, aes(x=(reorder(manner_of_death,total)), y=total)) + 
  geom_bar(stat = "identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="Fatal hootings by Police") + coord_flip() + theme_bg)

#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/manner.plot.png", height=7, width=8)


#groups by date and makes date plot
month <- data %>% group_by(year, month, count) %>% summarise(total = sum(count))

(month.plot <- ggplot(subset(month, count > 0), aes(x=month, y=total)) + facet_grid(.~year)+ 
 geom_bar(stat="identity", alpha=transparency, fill = "dodgerblue") + 
  labs(x="", y="", title = "Fatal Shootings by Police per Month") + theme_bg + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.35)))


#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/month.plot.png", height=7, width=8)

#daily time series
date <- data %>% group_by(date, count, race2) %>% summarise(total = sum(count))

(timeseries.plot <- ggplot(date, aes(x=date, y=total)) + 
  geom_point(size=.2) + geom_line(size=1) + 
  labs(x="", y="", title = "Timeseries Plot of Fatal Shootings by Police") + theme_bg + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.35)) + 
  scale_x_datetime(breaks = ("1 month")) + geom_line(stat="hline", yintercept="mean", color = "red"))


(timeseries.facet.plot <- ggplot(date, aes(x=date, y=total)) + 
  geom_point(size=.2) + geom_line(size=1) + 
  facet_grid(race2 ~ .) + 
  labs(x="", y="", title = "Timeseries Plot of Fatal Shootings by Police") + theme_bg + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.35)) + 
  scale_x_datetime(breaks = ("1 month")) + geom_line(stat="hline", yintercept="mean", color = "red"))

#weekly time series
week <- data %>% group_by(year, week, count, race2) %>% summarise(total = sum(count))

(week.timeseries.plot <- ggplot(na.omit(week), aes(x=week, y=total, color = year)) + 
  geom_point(size=.2) + geom_line(size=1) + facet_grid(.~year) + 
  labs(x="", y="", title = "Timeseries Plot of Fatal Shootings by Police") + theme_bg + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.35)) + 
  geom_line(stat="hline", yintercept="mean", color = "red"))

(week.timeseries.facet.plot <- ggplot(week, aes(x=week, y=total)) + 
  geom_point(size=.2) + geom_line(size=1) + 
  facet_grid(race2 ~ .) + 
  labs(x="", y="", title = "Timeseries Plot of Fatal Shootings by Police") + theme_bg + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.35)) + 
  geom_line(stat="hline", yintercept="mean", color = "red"))


#ggsave("C:/Users/GRA/Desktop/Misc/R Working Directory/School/time_series_and_forecasting/project/plots/timeseries.plot.png", height=7, width=8)
