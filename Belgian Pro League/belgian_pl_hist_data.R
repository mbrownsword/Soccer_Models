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

setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/Belgian Pro League")
source("fotmob_functions.R")

l <- fotmob_get_league_ids(cached = FALSE)

leagues <- c(92)
league_name <- "First Division A"
ccode <- "BEL"


fixtures <- fotmob_get_league_matches(ccode,league_name, cached = FALSE)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]
f_teams <- read_xlsx("team_df2 3.xlsx")

team_df <- data.frame(team_1 = c( "Gent"   ,"Sporting Charleroi" , "Kortrijk"  ,  "Zulte Waregem", "St.Truiden" , "Union St.Gilloise","Royal Antwerp"   ,"Oostende", "Westerlo" ),
                      team_2 = c("KAA Gent","Sporting de Charleroi" ,"KV Kortrijk" ,  "SV Zulte Waregem" ,"St. Truidense" ,"Union Saint Gilloise","Antwerp","KV Oostende", "KVC Westerlo" ))
write.csv(team_df,paste0("team_codes",ccode,"_",league_name,"_",".csv"))

team_df2 <-  data.frame(team_1 = c("Standard de Liège"   ,"Sint-Truiden" ,"AS Eupen" ,"Westerlo","Seraing United"  ,
                                   "Royal Antwerp FC","AA Gent" , "KRC Genk","Zulte Waregem" , "Union Saint-Gilloise" ,"Kortrijk" ),
                        team_2 = c("Standard Liege" , "St. Truidense"  ,"Eupen" ,"KVC Westerlo"  ,  "RFC Seraing"   ,  
                                   "Antwerp", "KAA Gent" , "Genk" ,"SV Zulte Waregem" ,"Union Saint Gilloise"  ,"KV Kortrijk"       ))
write.csv(team_df2,paste0("team_barstool",ccode,"_",league_name,"_",".csv"))

e1_stat <- get_player_data_all(leagues, team_df)
e1_stats <- e1_stat[[1]]
e1_stats_s <- apply(e1_stats,2,as.character)
write.csv(e1_stats_s,paste0("data_",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))

e_teams_e <- e1_stat[[2]]
write.csv(e_teams_e,paste0("teams_",ccode,"_",league_name,"_",".csv"))
