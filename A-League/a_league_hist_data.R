library(tidyverse)
library(textclean)
library(data.table)
library(flexdashboard)
library(rvest)
library(googlesheets)
library(readxl)
library(raster)
library(tidyverse)
library(data.table)
library(tools)
library(httr)
library(RSelenium)
library(jsonlite)
library(RSelenium)
library(httr)
library(rvest)
library(ttutils)
library(jsonlite)
library(remotes)
library(emayili)
library(curl)
library(openssl)
library(sys)
library(mailR)
library(knitr)
library(png) 
library(mailR)
library(DT)
library(webshot)
library(stringdist)
detach(package:plyr)
detach(package:dplyr)
library(plyr)
library(dplyr)
library(curl)
library(openssl)
library(sys)
library(mailR)
library(printr)
library(png)
library(mailR)
library(DT)
library(webshot)
library(ggpubr)
library(png)
library(grid)
library(gridExtra)
library(tidyquant)
library(pracma)
library(magrittr)
library(haven)
library(stringi)
library(worldfootballR)
getwd()
############ League Analysis ################

setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/A-League")
source("fotmob_functions.R")

########## Variables ########

l <- fotmob_get_league_ids(cached = FALSE)
View(l)
leagues <- c(80)
league_name <- "A-League"
ccode <- "AUS"


fixtures <- fotmob_get_league_matches(ccode, league_name, cached = FALSE)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]
f_teams <- read_xlsx("team_df2 3.xlsx")

team_df <- data.frame(team_1 = c("Melbourne City FC","Western United FC" ,"Brisbane Roar FC" ,"Western Sydney Wanderers FC"),
                      team_2 = c("Melbourne City","Western United" ,"Brisbane Roar" ,"Western Sydney FC"))
write.csv(team_df,paste0("team_codes_",ccode,"_",league_name,".csv"))

team_df2 <-  data.frame(team_1 = c( "Melbourne City FC" , "Newcastle Jets FC","Western Sydney Wanderers" ,"Western United FC"  ),
                        team_2 = c( "Melbourne City" ,  "Newcastle Jets" ,"Western Sydney FC","Western United"     ))

write.csv(team_df2,paste0("team_barstool_",ccode,"_",league_name,".csv"))

e1_stat <- get_player_data_all(leagues, team_df)
e1_stats <- e1_stat[[1]]
e1_stats_s <- apply(e1_stats,2,as.character)
write.csv(e1_stats_s,paste0("data_",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))

e_teams_e <- e1_stat[[2]]
write.csv(e_teams_e,paste0("teams_",ccode,"_",league_name,".csv"))

