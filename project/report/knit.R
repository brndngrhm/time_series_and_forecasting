source("pa_shootings.R")
library("rmarkdown")
render('shootings.Rmd')
rstudio::viewer("C:/Users/GRA/Desktop/Misc/R Working Directory/Other/NFL/nfl_team_stats/shootings.html")