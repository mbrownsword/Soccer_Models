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
library(worldfootballR)
l <- fotmob_get_league_ids(cached = FALSE)
wspc <- "C:/Users/CORSAIR GAMING/Documents/"
############ Tomorrow's games ################

dates <- as.Date(c(as.Date("2023-02-15")))

for(a in 1:length(dates)){
  setwd(paste0(wspc,"Fotmob Soccer Modelling/All Data"))
  e <- fotmob_get_matches_by_date(dates[a])
  f <- e
  p_list <- vector("list",length(f$ccode))
  for(b in 1:length(f$ccode)){
    p <- fotmob_get_match_players(f$match_id[b])
    if(length(p)==0){
      next()
    }else{
      if(all(is.na(p$rating_num))>0){
        next()
      }else{
        
      }
      p$league <- f$name[b]
      p$ccode <- f$ccode[b]
      p$league_id <- f$id[b]
      team1 <- unique(p$team_name)[1]
      team2 <- unique(p$team_name)[2]
      p$formation[p$team_name==team1] <- paste0(unlist(data.frame(table(p$position_row[p$team_name==team1]))$Freq[-1]),collapse = "-")
      p$formation[p$team_name==team2] <- paste0(unlist(data.frame(table(p$position_row[p$team_name==team2]))$Freq[-1]),collapse = "-")
      p$opp_formation[p$team_name==team1] <- paste0(unlist(data.frame(table(p$position_row[p$team_name==team2]))$Freq[-1]),collapse = "-")
      p$opp_formation[p$team_name==team2] <- paste0(unlist(data.frame(table(p$position_row[p$team_name==team1]))$Freq[-1]),collapse = "-")
      
      for(aa in 1:length(p$match_id)){
        setwd(paste0(wspc,"Fotmob Soccer Modelling/All Player Data"))
        all_files <- list.files()
        if(p$page_url[aa] %in% all_files){
          
        }else{
          dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Player Data/",gsub("/","-",p$page_url[aa])))
        }
        setwd(paste0(wspc,"Fotmob Soccer Modelling/All Player Data/",gsub("/","-",p$page_url[aa])))
        game_data_s <- transpose(data.frame(apply(p[aa,],2,as.character)))
        colnames(game_data_s) <-  rownames(data.frame(apply(p[aa,],2,as.character)))
        write.csv(game_data_s,paste0(gsub("-","",dates[a])," ",gsub("/","-",p$page_url[aa]),".csv"),row.names = F)
      }
      
      p_list[[b]] <- p
      rm(all_files)
    } 
    print(paste0("Done with game: ",f$home_name[b]," v ",f$away_name[b]," on ",f$match_status_utc_time[b]))
  }
  day_data <- rbindlist(p_list, fill = T)
  dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Data/",gsub("-","",dates[a])))
  setwd(paste0(wspc,"Fotmob Soccer Modelling/All Data/",gsub("-","",dates[a])))
  s_list_s <- apply(day_data,2,as.character)
  write.csv(s_list_s,"player_data.csv",row.names = F)
  rm(day_data)
  rm(p_list)
  rm(s_list_s)
}
