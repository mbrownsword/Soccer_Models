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
library(stringi)
l <- fotmob_get_league_ids(cached = FALSE)
wspc <- "C:/Users/CORSAIR GAMING/Documents/"
setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))

get_team_codes <- function(wspc){
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
  files <- list.files()
  files <- files[-which(grepl("\\.",files))]
  teams_codes <- vector("list",length(files))
  for(b in 1:length(files)){
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling","/",files[b])))
    files_2 <- list.files()
    if(any(grepl("team_codes",files_2))){
      teams_codes[[b]] <- read.csv(files_2[which(grepl("team_codes",files_2))])
    }else{
      next()
    }
  }
  all_team_codes <- rbindlist(teams_codes)[,c(2,3)]
  
  team_df <- all_team_codes
  if(length(which(duplicated(team_df)))==0){
    
  }else{
    team_df <- team_df[-which(duplicated(team_df)),]
  }
  team_df
}

get_team_codes2 <- function(wspc){
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
  files <- list.files()
  files <- files[-which(grepl("\\.",files))]
  teams_codes <- vector("list",length(files))
  for(b in 1:length(files)){
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling","/",files[b])))
    files_2 <- list.files()
    if(any(grepl("team_barstool",files_2))){
      teams_codes[[b]] <- read.csv(files_2[which(grepl("team_barstool",files_2))])
    }else{
      next()
    }
  }
  all_team_codes <- rbindlist(teams_codes)[,c(2,3)]
  
  team_df <- all_team_codes
  if(length(which(duplicated(team_df)))==0){
    
  }else{
    team_df <- team_df[-which(duplicated(team_df)),]
  }
  
  team_df
}

get_team_codes3 <- function(wspc){
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
  files <- list.files()
  files <- files[-which(grepl("\\.",files))]
  teams_codes <- vector("list",length(files))
  for(b in 1:length(files)){
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling","/",files[b])))
    files_2 <- list.files()
    if(any(grepl("player_prop",files_2))){
      teams_codes[[b]] <- read.csv(files_2[which(grepl("player_prop",files_2))])
    }else{
      next()
    }
  }
  all_team_codes <- rbindlist(teams_codes)[,c(2,3)]
  
  team_df <- all_team_codes
  if(length(which(duplicated(team_df)))==0){
    
  }else{
    team_df <- team_df[-which(duplicated(team_df)),]
  }
  team_df
}

#team_df <- get_team_codes(wspc)





get_back_data <- function(){
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Data")))
  
  days <- list.files()
  elo <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))
  
  e_teams <- read.table(paste0("https://www.eloratings.net/en.teams.tsv?=1616678992547"), sep = '\t', header = FALSE, fill = T)
  e_teams$V1[e_teams$V2=="Namibia"] <- "NA"
  team_fix <- ""
  ww <- 1
  for(e in 1:length(days)){
    next_content <- F
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Data/",days[e])))
    day_data <- read.csv(list.files())
    umids <- unique(day_data$match_id)
    if(length(umids)==0){
      next()
    }
    for(i in 1:length(team_df$team_1)){
      day_data$team_name[day_data$team_name==team_df$team_1[i]] <- team_df$team_2[i]
    }
    game_data_umids <- vector("list",1)
    for(f in 1:length(umids)){
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Data/",days[e])))
      game_data <- day_data[which(day_data$match_id==umids[f]),]
      
      
      game_data$stats_minutes_played <- as.numeric(game_data$stats_minutes_played)
      game_data$stats_total_shots <- as.numeric(game_data$stats_total_shots)
      for(b in 1:length(game_data$match_id)){
        game_data$stats_tackles_won[b] <- as.numeric(strsplit(strsplit(game_data$stats_tackles_won[b],"/")[[1]][2]," ")[[1]][1])
        game_data$stats_accurate_passes[b] <- as.numeric(strsplit(strsplit(game_data$stats_accurate_passes[b],"/")[[1]][2]," ")[[1]][1])
        game_data$stats_shot_accuracy[b] <- as.numeric(strsplit(game_data$stats_shot_accuracy[b],"/")[[1]][1])
      }
      game_data$stats_shot_accuracy <- as.numeric(game_data$stats_shot_accuracy)
      game_data$stats_tackles_won <- as.numeric(game_data$stats_tackles_won)
      game_data$stats_goals <- as.numeric(game_data$stats_goals)
      game_data$stats_goals_conceded <- as.numeric(game_data$stats_goals_conceded)
      game_data$stats_assists <- as.numeric(game_data$stats_assists)
      game_data$stats_accurate_passes <- as.numeric(game_data$stats_accurate_passes)
      
      if(length(game_data$stats_expected_goals_x_g)==0){
        
      }else{
        game_data$stats_expected_goals_x_g <- as.numeric(game_data$stats_expected_goals_x_g)
        game_data$stats_expected_goals_on_target_x_got <- as.numeric(game_data$stats_expected_goals_on_target_x_got)
        game_data$stats_expected_assists_x_a <- as.numeric(game_data$stats_expected_assists_x_a)
        game_data$stats_x_got_faced <- as.numeric(game_data$stats_x_got_faced)
        
      }
      
     
      for(i in 1:length(team_df$team_1)){
        game_data$team_name[game_data$team_name==team_df$team_1[i]] <- team_df$team_2[i]
      }
      
      
      
      
      game_data$page_url_2 <- game_data$page_url
      for(i in 1:length(game_data$page_url)){
        
        game_data$page_url[i] <- paste0("/",strsplit(game_data$page_url_2[i],"/")[[1]][2],"/",strsplit(game_data$page_url_2[i],"/")[[1]][3],"/")
        
      }
      
      game_data$referee <- ""
      game_data$manager <- 0
      game_data$team_rating <- 0
      game_data$opp_rating <- 0
      team1 <- unique(game_data$team_name)[1]
      team2 <- unique(game_data$team_name)[2]
      
      team1 <- gsub("/","-",team1)
      team2 <- gsub("/","-",team2)
      
      tryCatch({ 
        game_json <- read_json(paste0("https://www.fotmob.com/api/matchDetails?matchId=",
                                    umids[f],"&ccode3=USA&timezone=America%2FLos_Angeles&refresh=true&includeBuzzTab=false&acceptLanguage=en-US"))
      },
      
      error=function(e) {
        message('No game data for this game')
        next_content <- T
      })
      
      if(next_content==T){
        rm(next_content)
        next()
      }
      
      if(length(game_json$content$lineup$coaches$coachesArr[[1]][[1]]$id)==0){
        manager1_id <- 0
        manager1_url <- "no-coach-data"
      }else{
        manager1_id <- game_json$content$lineup$coaches$coachesArr[[1]][[1]]$id
        manager1_url <- game_json$content$lineup$coaches$coachesArr[[1]][[1]]$pageUrl
      }
      
      if(length(game_json$content$lineup$coaches$coachesArr[[2]][[1]]$id)==0){
        manager2_id <- 0
        manager2_url <- "no-coach-data"
      }else{
        manager2_id <- game_json$content$lineup$coaches$coachesArr[[2]][[1]]$id
        manager2_url <- game_json$content$lineup$coaches$coachesArr[[2]][[1]]$pageUrl
      }
      
      
      referee <- game_json$content$matchFacts$infoBox$Referee$text
      game_data$referee <- referee
      game_data$manager[game_data$team_name==team1] <- manager1_id
      game_data$manager[game_data$team_name==team2] <- manager2_id
      
      
      matchdate <- as.Date((force_tz(as.POSIXct(game_json$general$matchTimeUTCDate, format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800))
      
      
      if(unique(game_data$ccode)=="INT"&unique(game_data$league)!="Champions League"&unique(game_data$league)!="Europa League"&unique(game_data$league)!="Champions League Final Stage"&
         unique(game_data$league)!="Europa Conference League"&unique(game_data$league)!="AFC Champions League"&unique(game_data$league)!="Club Friendlies"&
         unique(game_data$league)!="CONCACAF Champions League"&unique(game_data$league)!="Copa Libertadores Qualification"&unique(game_data$league)!="Copa Libertadores"&
         unique(game_data$league)!="FIFA Club World Cup"&unique(game_data$league)!="Europa League Final Stage"&unique(game_data$league)!="Copa Libertadores Grp. E"&
         unique(game_data$league)!="Copa Libertadores Grp. A"&unique(game_data$league)!="Copa Libertadores Grp. C"&unique(game_data$league)!="Copa Libertadores Grp. F"&
         unique(game_data$league)!="Copa Libertadores Grp. B"&unique(game_data$league)!="Copa Libertadores Grp. D"&unique(game_data$league)!="Copa Libertadores Grp. G"&
         unique(game_data$league)!="Copa Libertadores Grp. H"&unique(game_data$league)!="AFC Champions League Grp. E"&unique(game_data$league)!= "AFC Champions League Final Stage"&
         unique(game_data$league)!="AFC Champions League Grp. A"&unique(game_data$league)!="AFC Champions League Grp. C"&unique(game_data$league)!="AFC Champions League Grp. F"&
         unique(game_data$league)!="AFC Champions League Grp. B"&unique(game_data$league)!="AFC Champions League Grp. D"&unique(game_data$league)!="AFC Champions League Grp. G"&
         unique(game_data$league)!="AFC Champions League Grp. H"&unique(game_data$league)!="AFC Champions League Grp. I"&unique(game_data$league)!="AFC Champions League Grp. J"&
         unique(game_data$league)!="AFC Cup Grp. A"&unique(game_data$league)!="AFC Cup Grp. C"&unique(game_data$league)!="AFC Cup Grp. F"&
         unique(game_data$league)!="AFC Cup Grp. B"&unique(game_data$league)!="AFC Cup Grp. D"&unique(game_data$league)!="AFC Cup Grp. G"&
         unique(game_data$league)!="AFC Cup Grp. H"&unique(game_data$league)!="AFC Cup Grp. I"&unique(game_data$league)!="AFC Cup Grp. J"&unique(game_data$league)!="UEFA Super Cup"&
         unique(game_data$league)!="Champions League Qualification"&unique(game_data$league)!="Europa League Qualification"&unique(game_data$league)!="AFC Cup Grp. E"&
         unique(game_data$league)!="Europa Conference League Qualification"&unique(game_data$league)!="Champions League Grp. E"&unique(game_data$league)!="CAF Super Cup"&
         unique(game_data$league)!="Champions League Grp. A"&unique(game_data$league)!="Champions League Grp. C"&unique(game_data$league)!="Champions League Grp. F"&
         unique(game_data$league)!="Champions League Grp. B"&unique(game_data$league)!="Champions League Grp. D"&unique(game_data$league)!="Champions League Grp. G"&
         unique(game_data$league)!="Champions League Grp. H"&unique(game_data$league)!="Europa League Grp. E"&unique(game_data$league)!="Copa Sudamericana"&
         unique(game_data$league)!="Copa Sudamericana Grp. A"&unique(game_data$league)!="Copa Sudamericana Grp. C"&unique(game_data$league)!="Copa Sudamericana Grp. E"&
         unique(game_data$league)!="Copa Sudamericana Grp. B"&unique(game_data$league)!="Copa Sudamericana Grp. D"&unique(game_data$league)!="Copa Sudamericana Grp. F"&
         unique(game_data$league)!="Copa Sudamericana Grp. G"&unique(game_data$league)!="Copa Sudamericana Grp. H"&unique(game_data$league)!="Copa Sudamericana Grp. I"&
         unique(game_data$league)!="Copa Sudamericana Final Stage"&unique(game_data$league)!="Summer Olympics Grp. A"&unique(game_data$league)!="Summer Olympics Grp. B"&
         unique(game_data$league)!="Summer Olympics Grp. C"&unique(game_data$league)!="Summer Olympics Grp. E"&
         unique(game_data$league)!="Summer Olympics Grp. D"&unique(game_data$league)!="Summer Olympics Grp. F"&unique(game_data$league)!="Summer Olympics Final Stage"&
         unique(game_data$league)!="Europa League Grp. A"&unique(game_data$league)!="Europa League Grp. C"&unique(game_data$league)!="Europa League Grp. F"&
         unique(game_data$league)!="Europa League Grp. B"&unique(game_data$league)!="Europa League Grp. D"&unique(game_data$league)!="Europa League Grp. G"&
         unique(game_data$league)!="Europa League Grp. H"&unique(game_data$league)!="Europa League Grp. I"&unique(game_data$league)!="Europa League Grp. J"&
         unique(game_data$league)!="Europa League Grp. K"&unique(game_data$league)!="Europa League Grp. L"&unique(game_data$league)!="Copa Libertadores Final Stage"&
         unique(game_data$league)!="Europa Conference League Grp. E"&unique(game_data$league)!="Europa Conference League Grp. I"&unique(game_data$league)!="Europa Conference League Grp. J"&
         unique(game_data$league)!="Europa Conference League Grp. A"&unique(game_data$league)!="Europa Conference League Grp. C"&unique(game_data$league)!="Europa Conference League Grp. F"&
         unique(game_data$league)!="Europa Conference League Grp. B"&unique(game_data$league)!="Europa Conference League Grp. D"&unique(game_data$league)!="Europa Conference League Grp. G"&
         unique(game_data$league)!="Europa Conference League Grp. H"&unique(game_data$league)!="Europa Conference League Grp. K"&unique(game_data$league)!="Europa Conference League Grp. L"&
         unique(game_data$league)!="CAF Champions League Grp. E"&unique(game_data$league)!="Copa Sudamericana Qualification"&unique(game_data$league)!="Recopa Sudamericana"&
         unique(game_data$league)!="CAF Champions League Grp. A"&unique(game_data$league)!="CAF Champions League Grp. C"&unique(game_data$league)!="CAF Champions League Grp. F"&
         unique(game_data$league)!="CAF Champions League Grp. B"&unique(game_data$league)!="CAF Champions League Grp. D"&unique(game_data$league)!="CAF Champions League Grp. G"&
         unique(game_data$league)!="CAF Champions League Grp. H"&unique(game_data$league)!="CAF Champions League Grp. I"&unique(game_data$league)!="CAF Champions League Grp. J"&
         unique(game_data$league)!="CAF Champions League Grp. K"&unique(game_data$league)!="CAF Champions League Grp. L"&unique(game_data$league)!="CAF Champions League Final Stage"&
         unique(game_data$league)!="CAF Confederation Cup Grp. A"&unique(game_data$league)!="CAF Confederation Cup Grp. C"&unique(game_data$league)!="CAF Confederation Cup Grp. F"&
         unique(game_data$league)!="CAF Confederation Cup Grp. B"&unique(game_data$league)!="CAF Confederation Cup Grp. D"&unique(game_data$league)!="CAF Confederation Cup Grp. G"&
         unique(game_data$league)!="CAF Confederation Cup Grp. H"&unique(game_data$league)!="CAF Confederation Cup Grp. I"&unique(game_data$league)!="CAF Confederation Cup Grp. J"&
         unique(game_data$league)!="CAF Confederation Cup Grp. K"&unique(game_data$league)!="CAF Confederation Cup Grp. L"&unique(game_data$league)!="CAF Confederation Cup Final Stage"){
        
        y <- year(matchdate)
        elo_int <- read.table(paste0("https://www.eloratings.net/",y,"_results.tsv"), sep = '\t', header = FALSE)
        elo_int$V4 <- ifelse(is.na(elo_int$V4),"NA",elo_int$V4)
        elo_int$V5 <- ifelse(is.na(elo_int$V5),"NA",elo_int$V5)
        elo_int$date <- as.Date(paste0(elo_int$V1,"/", elo_int$V2,"/", elo_int$V3), "%Y/%m/%d")
        
        
        game_data$team_name[game_data$team_name=="UAE"] <- "United Arab Emirates"
        game_data$team_name[game_data$team_name=="Chinese Taipei"] <- "Taiwan"
        game_data$team_name[game_data$team_name=="USA"] <- "United States"
        game_data$team_name[game_data$team_name=="DR Congo"] <- "Democratic Republic of Congo"
        game_data$team_name[game_data$team_name=="St. Kitts and Nevis"] <-  "Saint Kitts and Nevis"
        game_data$team_name[game_data$team_name=="Czech Republic"] <- "Czechia"
        game_data$team_name[game_data$team_name=="Turkiye"] <- "Turkey"
        game_data$team_name[game_data$team_name=="Curacao"] <- e_teams$V2[100]
        game_data$team_name[game_data$team_name=="Sao Tome and Principe"] <- e_teams$V2[313]
          
          team1 <- unique(game_data$team_name)[1]
          team2 <- unique(game_data$team_name)[2]
        
          games <- elo_int[c(which(elo_int$V4==e_teams$V1[e_teams$V2==team2]),
                             which(elo_int$V5==e_teams$V1[e_teams$V2==team2]),
                             which(elo_int$V4==e_teams$V1[e_teams$V2==team1]),
                             which(elo_int$V5==e_teams$V1[e_teams$V2==team1])),]
          if(length(which(duplicated(games)))==0){
            
          }else{
            games <- games[-which(duplicated(games)),]
          }
          p_game <- games[games$date<=matchdate,]
          p_game <- p_game[max(p_game$date)==p_game$date,]
          if(p_game$V5!=e_teams$V1[e_teams$V2==team2] | p_game$V4!=e_teams$V1[e_teams$V2==team1]){
            break()
          }
          if(p_game$V4==e_teams$V1[e_teams$V2==team2]){
            game_data$team_rating[game_data$team_name==team2] <- p_game$V11-p_game$V10
            game_data$team_rating[game_data$team_name==team1] <- p_game$V12+p_game$V10
            game_data$opp_rating[game_data$team_name==team2] <- p_game$V12+p_game$V10
            game_data$opp_rating[game_data$team_name==team1] <- p_game$V11-p_game$V10
          }else{
            game_data$team_rating[game_data$team_name==team2] <- p_game$V12+p_game$V10
            game_data$team_rating[game_data$team_name==team1] <- p_game$V11-p_game$V10
            game_data$opp_rating[game_data$team_name==team2] <- p_game$V11-p_game$V10
            game_data$opp_rating[game_data$team_name==team1] <- p_game$V12+p_game$V10
          }
        
      }else{
        game_elo1 <- elo[c(which(elo$team1==team1),
                           which(elo$team2==team1),
                           which(elo$team1==team2),
                           which(elo$team2==team2)),]
        if(length(game_elo1$team1)==0){
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data")))
          
          team_files <- list.files()
          
          if(team1 %in% team_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team1))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team1))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",team1,".csv"),row.names = F)
          
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data")))
          
          team_files <- list.files()
          
          if(team2 %in% team_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team2))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team2))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",team2,".csv"),row.names = F)
          
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data")))
          
          manager_files <- list.files()
          
          if(gsub("/","-",manager1_url) %in% manager_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager1_url)))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager1_url)))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",gsub("/","-",manager1_url),".csv"),row.names = F)
          
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data")))
          
          manager_files <- list.files()
          
          if(gsub("/","-",manager2_url) %in% manager_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager2_url)))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager2_url)))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",gsub("/","-",manager2_url),".csv"),row.names = F)
          
          
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data")))
          
          formation_matchup <- paste0(unique(game_data$formation[game_data$team_name==team1])," v. ",
                                      unique(game_data$formation[game_data$team_name==team2]))
          
          formation_files <- list.files()
          
          if(formation_matchup %in% formation_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data/",formation_matchup))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data/",formation_matchup))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",formation_matchup,".csv"),row.names = F)
          
          
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
          
          league <- paste0(unique(game_data$ccode)," ",
                           unique(game_data$league))
          
          league_files <- list.files()
          
          if(league %in% league_files){
            
          }else{
            dir.create(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",league))
          }
          
          setwd(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",league))
          
          game_data_s <- data.frame(apply(game_data,2,as.character))
          write.csv(game_data_s,paste0(gsub("-","",days[e])," ",league," ",team1," v ",team2,".csv"),row.names = F)
          game_data_umids[[f]] <- game_data
          next()
        }
        
        if(length(which(duplicated(game_elo1)))==0){
          
        }else{
          game_elo1 <- game_elo1[-which(duplicated(game_elo1)),]
        }
        
        game_elo1 <- game_elo1[game_elo1$date<=matchdate,]
        
        sp_game_elo <- game_elo1[which(game_elo1$team1==team1&game_elo1$team2==team2&game_elo1$date==matchdate),]
        
        if(length(sp_game_elo$season)==0){
          team_1_game_elo <- game_elo1[c(which(game_elo1$team1==team1),
                                         which(game_elo1$team2==team1)),]
          team_1_game_elo <- team_1_game_elo[which(team_1_game_elo$date==max(team_1_game_elo$date)),]
          if(length(team_1_game_elo$season)==0){
            print(paste0("No data for ", team1))
            team_fix[ww] <- team1
            ww <- ww+1
            next()
          }
          if(team_1_game_elo$team1==team1){
            team1_rating <- team_1_game_elo$spi1
          }else{
            team1_rating <- team_1_game_elo$spi2
          }
          
          
          team_2_game_elo <- game_elo1[c(which(game_elo1$team1==team2),
                                         which(game_elo1$team2==team2)),]
          team_2_game_elo <- team_2_game_elo[which(team_2_game_elo$date==max(team_2_game_elo$date)),]
          if(length(team_2_game_elo$season)==0){
            print(paste0("No data for ", team2))
            team_fix[ww] <- team2
            ww <- ww+1
            next()
          }
          if(team_2_game_elo$team1==team2){
            team2_rating <- team_2_game_elo$spi1
          }else{
            team2_rating <- team_2_game_elo$spi2
          }
          game_data$team_rating[game_data$team_name==team1] <- team1_rating
          game_data$team_rating[game_data$team_name==team2] <- team2_rating
          game_data$opp_rating[game_data$team_name==team1] <- team2_rating
          game_data$opp_rating[game_data$team_name==team2] <- team1_rating
        }else{
          
          game_data$team_rating[game_data$team_name==team1] <- sp_game_elo$spi1
          game_data$team_rating[game_data$team_name==team2] <- sp_game_elo$spi2
          game_data$opp_rating[game_data$team_name==team1] <- sp_game_elo$spi2
          game_data$opp_rating[game_data$team_name==team2] <- sp_game_elo$spi1
        }
      }
      
      
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data")))
      
      team_files <- list.files()
      
      if(team1 %in% team_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team1))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team1))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",team1,".csv"),row.names = F)
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data")))
      
      team_files <- list.files()
      
      if(team2 %in% team_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team2))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",team2))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",team2,".csv"),row.names = F)
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data")))
      
      manager_files <- list.files()
      
      if(gsub("/","-",manager1_url) %in% manager_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager1_url)))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager1_url)))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",gsub("/","-",manager1_url),".csv"),row.names = F)
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data")))
      
      manager_files <- list.files()
      
      if(gsub("/","-",manager2_url) %in% manager_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager2_url)))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",gsub("/","-",manager2_url)))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",gsub("/","-",manager2_url),".csv"),row.names = F)
      
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data")))
      
      formation_matchup <- paste0(unique(game_data$formation[game_data$team_name==team1])," v. ",
                                  unique(game_data$formation[game_data$team_name==team2]))
      
      formation_files <- list.files()
      
      if(formation_matchup %in% formation_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data/",formation_matchup))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data/",formation_matchup))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",formation_matchup," ",team1," v ",team2,".csv"),row.names = F)
      
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
      
      league <- paste0(unique(game_data$ccode)," ",
                                  unique(game_data$league))
      
      league_files <- list.files()
      
      if(league %in% league_files){
        
      }else{
        dir.create(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",league))
      }
      
      setwd(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",league))
      
      game_data_s <- data.frame(apply(game_data,2,as.character))
      write.csv(game_data_s,paste0(gsub("-","",days[e])," ",league," ",team1," v ",team2,".csv"),row.names = F)
      
      
      game_data_umids[[f]] <- game_data
      print(paste0("Done with ",team1," v ",team2," on ",matchdate))
    }
    print(paste0("Done with ",days[e]))
  }
  
}


get_fixture_ratings <- function(id, date, team_df){
  
  fixtures <- fotmob_get_matches_by_date(date)
  fixtures$home_name <- fixtures$home_long_name
  fixtures$away_name <- fixtures$away_long_name
  fixtures <- fixtures[fixtures$primary_id==id,]
  #fixtures <- fotmob_get_league_matches(league_id = id, cached = FALSE)
  #fixtures$fixed_date <- as.Date(force_tz(as.POSIXct(fixtures$status$utcTime, format = "%Y-%m-%dT%H:%M:%OSZ"), "UTC")-28800)
  #fixtures <- fixtures[which( fixtures$fixed_date==date),]
  home <- data.frame(matrix("",length(fixtures$ccode),3))
  away <-  data.frame(matrix("",length(fixtures$ccode),3))
  status <- data.frame(matrix("",length(fixtures$ccode),2))
  colnames(home) <- c("name","id","shortName")
  colnames(away) <- c("name","id","shortName")
  colnames(status) <- c("utcTime","finished")
  for(a in 1:length(fixtures$ccode)){
    home$name[a] <- fixtures$home_name[a]
    away$name[a] <- fixtures$away_name[a]
    home$shortName[a] <- fixtures$home_long_name[a]
    away$shortName[a] <- fixtures$away_long_name[a]
    home$id[a] <- fixtures$home_id[a]
    away$id[a] <- fixtures$away_id[a]
    status$utcTime[a] <- fixtures$match_status_utc_time[a]
    status$finished[a] <- fixtures$match_status_finished[a]
  }
  fixtures$home <- home
  fixtures$away <- away
  fixtures$status <- status
  fixtures$round <- fixtures$ccode
  fixtures$id <- fixtures$match_id
  
  fixtures$home$name[stri_trans_general(fixtures$home$name, "latin-ascii")=="Besiktas"] <- "Besiktas"
  fixtures$home$name[stri_trans_general(fixtures$home$name, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  fixtures$home$name[stri_trans_general(fixtures$home$name, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  
  fixtures$away$name[stri_trans_general(fixtures$away$name, "latin-ascii")=="Besiktas"] <- "Besiktas"
  fixtures$away$name[stri_trans_general(fixtures$away$name, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  fixtures$away$name[stri_trans_general(fixtures$away$name, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  
  fixtures$home$shortName[stri_trans_general(fixtures$home$shortName, "latin-ascii")=="Besiktas"] <- "Besiktas"
  fixtures$home$shortName[stri_trans_general(fixtures$home$shortName, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  fixtures$home$shortName[stri_trans_general(fixtures$home$shortName, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  
  fixtures$away$shortName[stri_trans_general(fixtures$away$shortName, "latin-ascii")=="Besiktas"] <- "Besiktas"
  fixtures$away$shortName[stri_trans_general(fixtures$away$shortName, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  fixtures$away$shortName[stri_trans_general(fixtures$away$shortName, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  
  fixtures_f <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))
  #fixtures_f <- fixtures_f[fixtures_f$date>as.Date("2022-07-01"),]
  
  
  fixtures$spi1 <- 0
  fixtures$spi2 <- 0
  for(i in 1:length(fixtures$round)){
    
    if(is.element(fixtures$home$name[i],team_df$team_1)){
      fixtures$home$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$home$name[i])]
    }
    
    if(is.element(fixtures$away$name[i],team_df$team_1)){
      fixtures$away$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$away$name[i])]
    }
    
    if(length(fixtures_f$spi1[which(fixtures_f$team1==fixtures$away$name[i])])==0){
      next()
    }
    if(length(fixtures_f$spi1[which(fixtures_f$team1==fixtures$home$name[i])])==0){
      next()
    }
    
    fixtures_f2 <- fixtures_f[c(which(fixtures_f$team1==fixtures$home$name[i]),which(fixtures_f$team2==fixtures$home$name[i])),]
    fixtures_f2 <- fixtures_f2[fixtures_f2$date>=(date-1),]
    fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
    if(length(which(grepl("Women",fixtures_f2$league)))==0){
      
    }else{
      fixtures_f2 <- fixtures_f2[-which(grepl("Women",fixtures_f2$league)),]
    }
    
    fixtures_f2 <- fixtures_f2[1,]
    
    
    
    
    fixtures$spi1[i] <- ifelse(length(fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])==0,
                               fixtures_f2$spi2[which(fixtures$home$name[i]==fixtures_f2$team2)],
                               fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])
    
    
    
    fixtures_f2 <- fixtures_f[c(which(fixtures_f$team2==fixtures$away$name[i]),
                                which(fixtures_f$team1==fixtures$away$name[i])),]
    fixtures_f2 <- fixtures_f2[fixtures_f2$date>=(date-1),]
    fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
    if(length(which(grepl("Women",fixtures_f2$league)))==0){
      
    }else{
      fixtures_f2 <- fixtures_f2[-which(grepl("Women",fixtures_f2$league)),]
    }
    fixtures_f2 <- fixtures_f2[1,]
    
    
    
    
    
    fixtures$spi2[i] <- ifelse(length(fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])==0,
                               fixtures_f2$spi1[which(fixtures$away$name[i]==fixtures_f2$team1)],
                               fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])
    
  }
  
  fixtures <- fixtures[fixtures$spi1!=0,]
  fixtures$home$shortName <- fixtures$home$name
  fixtures$away$shortName <- fixtures$away$name
  fixtures
}


team_squad <- function(id){
  
  team_url <- paste0("https://www.fotmob.com/api/teams?id=",id,"&ccode3=USA_NV")
  team_json <- read_json(team_url)
  team_stack <- vector("list",5)
  for(a in 1:5){
    team_all <- team_json$squad
    
    do <- as.data.frame(do.call(rbind, lapply(team_all[[a]][[2]], as.vector)))
    if(length(do)==0){
      next()
    }
    do <- cbind(my.var=rownames(do), do)
    do$my.var <- team_all[[a]][1]
    
    team_stack[[a]] <- do[,c(1:5)]
  }
  team <- rbindlist(team_stack, fill = T)
}

team_player_stats <- function(team1_squad){
  team1_squad$name <- unlist(team1_squad$name)
  all_team_data <- vector("list",length(team1_squad$id))
  for(c in 1:length(team1_squad$id)){
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Player Data")))
    all_player_files <- list.files()
    if(unlist(team1_squad$my.var)[c]=="coach"){
      next()
    }
    
    if(any(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",iconv(team1_squad$name[c],"latin1","ASCII",sub="")))), all_player_files))){
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Player Data/",all_player_files[which(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",iconv(team1_squad$name[c],"latin1","ASCII",sub="")))), all_player_files))])))
      player_files <- list.files()
      player_data <- vector("list",length(player_files))
      for(d in 1:length(player_files)){
        player_data[[d]] <- read.csv(player_files[d])
        player_data[[d]]$year <- substr(player_files[d], 1, 4) 
      }
    }else{
      if(any(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",iconv(strsplit(team1_squad$name[c]," ")[[1]][1],"latin1","ASCII",sub="")))), all_player_files))){
        setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Player Data/",all_player_files[which(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",iconv(strsplit(team1_squad$name[c]," ")[[1]][1],"latin1","ASCII",sub="")))), all_player_files))])))
        player_files <- list.files()
        player_data <- vector("list",length(player_files))
        for(d in 1:length(player_files)){
          player_data[[d]] <- read.csv(player_files[d])
          player_data[[d]]$year <- substr(player_files[d], 1, 4) 
        }
      }else{
        if(any(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",substr(iconv(strsplit(team1_squad$name[c]," ")[[1]][1],"latin1","ASCII",sub = ""),1,2)))), all_player_files))){
          setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Player Data/",all_player_files[which(grepl(paste0(team1_squad$id[c],"-",tolower(gsub(" ","-",substr(iconv(strsplit(team1_squad$name[c]," ")[[1]][1],"latin1","ASCII",sub = ""),1,2)))), all_player_files))])))
          player_files <- list.files()
          player_data <- vector("list",length(player_files))
          for(d in 1:length(player_files)){
            player_data[[d]] <- read.csv(player_files[d])
            player_data[[d]]$year <- substr(player_files[d], 1, 4) 
          }
        }else{
          print(paste0("No player data possible for ",team1_squad$name[c]))
        }
      }
    }
    all_team_data[[c]] <- rbindlist(player_data, fill = T)
  }
  
  team_player_data <- rbindlist(all_team_data, fill = T)
  
  if(length(which(duplicated(team_player_data[,c(1:10)])))==0){
    
  }else{
    team_player_data <- team_player_data[-which(duplicated(team_player_data[,c(1:10)])),]
  }
  
  team_player_data$stats_minutes_played <- as.numeric(team_player_data$stats_minutes_played)
  team_player_data$stats_total_shots <- as.numeric(team_player_data$stats_total_shots)
  for(b in 1:length(team_player_data$match_id)){
    team_player_data$stats_tackles_won[b] <- as.numeric(strsplit(strsplit(team_player_data$stats_tackles_won[b],"/")[[1]][2]," ")[[1]][1])
    team_player_data$stats_accurate_passes[b] <- as.numeric(strsplit(strsplit(team_player_data$stats_accurate_passes[b],"/")[[1]][2]," ")[[1]][1])
    team_player_data$stats_shot_accuracy[b] <- as.numeric(strsplit(team_player_data$stats_shot_accuracy[b],"/")[[1]][1])
  }
  team_player_data$stats_shot_accuracy <- as.numeric(team_player_data$stats_shot_accuracy)
  team_player_data$stats_tackles_won <- as.numeric(team_player_data$stats_tackles_won)
  team_player_data$stats_goals <- as.numeric(team_player_data$stats_goals)
  team_player_data$stats_goals_conceded <- as.numeric(team_player_data$stats_goals_conceded)
  team_player_data$stats_assists <- as.numeric(team_player_data$stats_assists)
  team_player_data$stats_accurate_passes <- as.numeric(team_player_data$stats_accurate_passes)
  
  team_player_data$stats_expected_goals_x_g <- as.numeric(team_player_data$stats_expected_goals_x_g)
  team_player_data$stats_expected_goals_on_target_x_got <- as.numeric(team_player_data$stats_expected_goals_on_target_x_got)
  team_player_data$stats_expected_assists_x_a <- as.numeric(team_player_data$stats_expected_assists_x_a)
  team_player_data$stats_x_got_faced <- as.numeric(team_player_data$stats_x_got_faced)
  
  for(i in 1:length(team_df$team_1)){
    team_player_data$team_name[team_player_data$team_name==team_df$team_1[i]] <- team_df$team_2[i]
  }
  
  
  
  
  team_player_data$page_url_2 <- team_player_data$page_url
  for(i in 1:length(team_player_data$page_url)){
    
    team_player_data$page_url[i] <- paste0("/",strsplit(team_player_data$page_url_2[i],"/")[[1]][2],"/",strsplit(team_player_data$page_url_2[i],"/")[[1]][3],"/")
    
  }
  
  stat_afc <- team_player_data %>%
    group_by(page_url, position_row) %>%
    summarise(page_url_2 = names(which.max(table(page_url_2))),
              minutes = sum(stats_minutes_played, na.rm = T),
              games = length(stats_minutes_played[!is.na(stats_minutes_played)]),
              shots = sum(stats_total_shots[!is.na(stats_minutes_played)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy[!is.na(stats_minutes_played)], na.rm = T),
              tackles = sum(stats_tackles_won[!is.na(stats_minutes_played)], na.rm = T),
              goals = sum(stats_goals[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              assists = sum(stats_assists[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
              minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_goals_against = sum(stats_x_got_faced[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              goals_against = sum(stats_goals_conceded[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T)
              #avg_elo_dif = mean(elo_dif)
              )
  
  stat_afc$sog_90min <- 90*stat_afc$shotsongoal/stat_afc$minutes
  stat_afc$shots_90min <- 90*stat_afc$shots/stat_afc$minutes
  stat_afc$tkl_90min <- 90*stat_afc$tackles/stat_afc$minutes
  stat_afc$pass_90min <- 90*stat_afc$passes/stat_afc$minutes
  stat_afc$xg_90min <- 90*stat_afc$expected_goals/stat_afc$minutes_xg
  stat_afc$xgot_90min <- 90*stat_afc$expected_goals_ot/stat_afc$minutes_xg
  stat_afc$xa_90min <- 90*stat_afc$expected_ast/stat_afc$minutes_xg
  stat_afc$goal_90min <- 90*stat_afc$goals/stat_afc$minutes_xg
  stat_afc$ast_90min <- 90*stat_afc$assists/stat_afc$minutes_xg
  
  stat_afc$xgot_rate_90_min <- stat_afc$xgot_90min/stat_afc$xg_90min 
  stat_afc$xg_rate_90_min <- stat_afc$goal_90min/stat_afc$xg_90min 
  stat_afc$xa_rate_90_min <- stat_afc$ast_90min/stat_afc$xa_90min 
  
  year_afc <- team_player_data %>%
    group_by(page_url, position_row, year) %>%
    summarise(page_url_2 = names(which.max(table(page_url_2))),
              minutes = sum(stats_minutes_played, na.rm = T),
              games = length(stats_minutes_played[!is.na(stats_minutes_played)]),
              shots = sum(stats_total_shots[!is.na(stats_minutes_played)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy[!is.na(stats_minutes_played)], na.rm = T),
              tackles = sum(stats_tackles_won[!is.na(stats_minutes_played)], na.rm = T),
              goals = sum(stats_goals[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              assists = sum(stats_assists[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
              minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              expected_goals_against = sum(stats_x_got_faced[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              goals_against = sum(stats_goals_conceded[!is.na(stats_minutes_played)&!is.na(shotmap)], na.rm = T),
              avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T)
              #avg_elo_dif = mean(elo_dif)
    )
  
  year_afc$sog_90min <- 90*year_afc$shotsongoal/year_afc$minutes
  year_afc$shots_90min <- 90*year_afc$shots/year_afc$minutes
  year_afc$tkl_90min <- 90*year_afc$tackles/year_afc$minutes
  year_afc$pass_90min <- 90*year_afc$passes/year_afc$minutes
  year_afc$xg_90min <- 90*year_afc$expected_goals/year_afc$minutes_xg
  year_afc$xgot_90min <- 90*year_afc$expected_goals_ot/year_afc$minutes_xg
  year_afc$xa_90min <- 90*year_afc$expected_ast/year_afc$minutes_xg
  year_afc$goal_90min <- 90*year_afc$goals/year_afc$minutes_xg
  year_afc$ast_90min <- 90*year_afc$assists/year_afc$minutes_xg
  
  year_afc$xgot_rate_90_min <- year_afc$xgot_90min/year_afc$xg_90min 
  year_afc$xg_rate_90_min <- year_afc$goal_90min/year_afc$xg_90min 
  year_afc$xa_rate_90_min <- year_afc$ast_90min/year_afc$xa_90min 
  
  list(stat_afc,year_afc)
}

predicted_lineup <- function(team1_player_data, m_df, fixtures, team1, team2,team, team1_year_data){
  
  
  fixtures_2 <- fixtures
  u_f <- unlist(strsplit(m_df$formation,"-"))
  
  if(length(u_f)==3){
    l_df <- data.frame(pos = c(0,1,3,5),
                       pos_row = c(1,u_f))
  }else{
    if(length(u_f)==5){
      l_df <- data.frame(pos = c(0,1,2,3,4,5),
                         pos_row = c(1,u_f))
    }else{
      if(m_df$twos>m_df$fours){
        l_df <- data.frame(pos = c(0,1,2,3,5),
                           pos_row = c(1,u_f))
      }else{
        l_df <- data.frame(pos = c(0,1,3,4,5),
                           pos_row = c(1,u_f))
      }
    }
  }
  
  
  t <- read_json(paste0("https://www.fotmob.com/api/matchDetails?matchId=",
                        fixtures_2$id,
                        "&ccode3=USA&timezone=America%2FLos_Angeles&refresh=true&includeBuzzTab=false&acceptLanguage=en-US"))
  if(team==team1){
    naplayers <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[1]]
    naplayers <- lapply(naplayers, function(x) { x["name"] <- NULL; x })
    naplayers <- lapply(naplayers, function(x) { x["naInfo"] <- NULL; x })
    na_players <- data.frame(rbind(rbindlist(naplayers, fill = T), fill = T))
  }else{
    naplayers <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[2]]
    naplayers <- lapply(naplayers, function(x) { x["name"] <- NULL; x })
    naplayer <- lapply(naplayers, function(x) { x["naInfo"] <- NULL; x })
    na_players <- data.frame(rbind(rbindlist(naplayers, fill = T), fill = T))
  }
  
  
  
  
 
  
  
  
  
  starters <- vector("list",length(l_df$pos))
  team1_data <- team1_year_data[-which(team1_year_data$page_url_2 %in% na_players$pageUrl),]
  for(a in (order(l_df$pos_row))){
    if(a==1){
      players <- team1_data$page_url_2[(team1_data$position_row==l_df$pos[a])&team1_data$expected_goals_against>0]
      minutes <- team1_data$minutes_xg[(team1_data$position_row==l_df$pos[a])&team1_data$expected_goals_against>0]
      rating <- team1_data$avg_rating[(team1_data$position_row==l_df$pos[a])&team1_data$expected_goals_against>0] 
      starters[[a]] <- team1_player_data[which(team1_player_data$page_url_2==players[which(minutes==max(minutes))]&team1_player_data$position_row==l_df$pos[a]),]
    }else{
      players <- team1_data$page_url_2[(team1_data$position_row==l_df$pos[a])]
      minutes <- team1_data$minutes_xg[(team1_data$position_row==l_df$pos[a])]/90
      rating <- team1_data$avg_rating[(team1_data$position_row==l_df$pos[a])]*10
      players <- players[minutes>=0.5]
      rating <- rating[minutes>=0.5]
      minutes <- minutes[minutes>=0.5]
      minutes[minutes>=30] <- 30
      minutes <- minutes*rating
      players <- players[order(-minutes)]
      players <- players[1:l_df$pos_row[a]]
      for(b in 1:length(players)){
        if(b==1){
          starters[[a]] <- team1_player_data[team1_player_data$page_url_2==players[b]&team1_player_data$position_row==l_df$pos[a],]
        }else{
          starters[[a]] <- rbind(team1_player_data[team1_player_data$page_url_2==players[b]&team1_player_data$position_row==l_df$pos[a],],starters[[a]])
        }
      }
    }
    team1_data <- team1_data[-which(unlist(team1_data$page_url_2  %in% starters[[a]]$page_url_2)),]
  }
  team1_lineup <- data.frame(rbindlist(starters))
  team1_subs <- team1_player_data[which(team1_player_data$position_row==0&team1_player_data$expected_goals_against==0),]
  
  lineups_1 <- list(team1_lineup, team1_subs)
  
}

team_lineup <- function(id, date, fixtures, team1_player_data, team2_player_data){
  
  fixtures_2 <- fixtures
 
  lineup_list <- vector(("list"),4)
  elo <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))
  elo <- elo[elo$date>as.Date("2022-07-01"),]
  
  for(b in 1:length(fixtures_2$round)){
    fotmob_get_match_info(fixtures_2$id[b])
    
    t <- read_json(paste0("https://www.fotmob.com/api/matchDetails?matchId=",
                          fixtures_2$id[b],
                          "&ccode3=USA&timezone=America%2FLos_Angeles&refresh=true&includeBuzzTab=false&acceptLanguage=en-US"))
    
    
    
    if(is.element(t$header$teams[[1]]$name,team_df$team_1)){
      t$header$teams[[1]]$name <- team_df$team_2[which(team_df$team_1==t$header$teams[[1]]$name)]
    }
    
    if(is.element(t$header$teams[[2]]$name,team_df$team_1)){
      t$header$teams[[2]]$name <- team_df$team_2[which(team_df$team_1==t$header$teams[[2]]$name)]
    }
    
    
    for(i in 1:length(team_df$team_1)){
      fixtures_2$home$name[fixtures_2$home$name==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$away$name[fixtures_2$away$name==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$home$shortName[fixtures_2$home$shortName==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$away$shortName[fixtures_2$away$shortName==team_df$team_1[i]] <- team_df$team_2[i]
    }
    
    fixtures_2$home$name[stri_trans_general(fixtures_2$home$name, "latin-ascii")=="Besiktas"] <- "Besiktas"
    fixtures_2$home$name[stri_trans_general(fixtures_2$home$name, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
    fixtures_2$home$name[stri_trans_general(fixtures_2$home$name, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
    
    fixtures_2$away$name[stri_trans_general(fixtures_2$away$name, "latin-ascii")=="Besiktas"] <- "Besiktas"
    fixtures_2$away$name[stri_trans_general(fixtures_2$away$name, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
    fixtures_2$away$name[stri_trans_general(fixtures_2$away$name, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
    
    fixtures_2$home$shortName[stri_trans_general(fixtures_2$home$shortName, "latin-ascii")=="Besiktas"] <- "Besiktas"
    fixtures_2$home$shortName[stri_trans_general(fixtures_2$home$shortName, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
    fixtures_2$home$shortName[stri_trans_general(fixtures_2$home$shortName, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
    
    fixtures_2$away$shortName[stri_trans_general(fixtures_2$away$shortName, "latin-ascii")=="Besiktas"] <- "Besiktas"
    fixtures_2$away$shortName[stri_trans_general(fixtures_2$away$shortName, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
    fixtures_2$away$shortName[stri_trans_general(fixtures_2$away$shortName, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
    
    if(stri_trans_general(t$header$teams[[1]]$name, "latin-ascii")== "Besiktas"){
      t$header$teams[[1]]$name <- "Besiktas"
    }
    
    if(stri_trans_general(t$header$teams[[2]]$name, "latin-ascii")== "Besiktas"){
      t$header$teams[[2]]$name <- "Besiktas"
    }
    
    if(stri_trans_general(t$header$teams[[1]]$name, "latin-ascii")=="Istanbul Basaksehir"){
      t$header$teams[[1]]$name <- "Istanbul Basaksehir"
    }
    
    if(stri_trans_general(t$header$teams[[2]]$name, "latin-ascii")== "Istanbul Basaksehir"){
      t$header$teams[[2]]$name <- "Istanbul Basaksehir"
    }
    
    if(stri_trans_general(t$header$teams[[1]]$name, "latin-ascii")== "Kasimpasa"){
      t$header$teams[[1]]$name <- "Kasimpasa"
    }
    
    if(stri_trans_general(t$header$teams[[2]]$name, "latin-ascii")== "Kasimpasa"){
      t$header$teams[[2]]$name <- "Kasimpasa"
    }
    
    if(is.element(t$header$teams[[2]]$name,elo$team1)==FALSE){
      
      next()
    }
    
    if(is.element(t$header$teams[[1]]$name,elo$team1)==FALSE){
      
      next()
    }
    
    if(length(t$content$lineup)==1){
      if(t$content$lineup==FALSE){
        
      }
    }else{
      if(length(t$content$lineup$lineup[[1]]$players)==0){
        
        if(t$content$lineup$simpleLineup==FALSE){
          
          
          
        }else{
          
          
          
          
        }
        
      }else{
        
        lineup <- vector(("list"),length(t$content$lineup$lineup[[1]]$players))
        for(a in 1:length(t$content$lineup$lineup[[1]]$players)){
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["name"] <- NULL; x })
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["rating"] <- NULL; x })
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["fantasyScore"] <- NULL; x })
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["stats"] <- NULL; x })
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["teamData"] <- NULL; x })
          t$content$lineup$lineup[[1]]$players[[a]] <- lapply(t$content$lineup$lineup[[1]]$players[[a]], function(x) { x["events"] <- NULL; x })
          lineup[[a]] <- rbindlist(t$content$lineup$lineup[[1]]$players[[a]], fill = T)
        }
        
        lineup_df <- rbindlist(lineup, fill = T)
        
        lineup_df$team <- fixtures_2$home$name[b]
        
        lineup_2 <- vector(("list"),length(t$content$lineup$lineup[[2]]$players))
        for(a in 1:length(t$content$lineup$lineup[[2]]$players)){
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["name"] <- NULL; x })
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["rating"] <- NULL; x })
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["fantasyScore"] <- NULL; x })
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["stats"] <- NULL; x })
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["teamData"] <- NULL; x })
          t$content$lineup$lineup[[2]]$players[[a]] <- lapply(t$content$lineup$lineup[[2]]$players[[a]], function(x) { x["events"] <- NULL; x })
          lineup_2[[a]] <- rbindlist(t$content$lineup$lineup[[2]]$players[[a]], fill = T)
        }
        
        lineup_df_2 <- rbindlist(lineup_2, fill = T)
        lineup_df_2$team <- fixtures_2$away$name[b]
        
        lineup_list[[b]] <- bind_rows(lineup_df,lineup_df_2)
        lineup_list[[b]]$match_id <- t$content$matchFacts$matchId
        
        if(length(t$content$lineup$bench$benchArr[[1]])==0){
          naplayers_1 <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[1]]
          naplayers_2 <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[2]]
          
          naplayers_1 <- lapply(naplayers_1, function(x) { x["name"] <- NULL; x })
          naplayers_1 <- lapply(naplayers_1, function(x) { x["naInfo"] <- NULL; x })
          naplayers_2 <- lapply(naplayers_2, function(x) { x["name"] <- NULL; x })
          naplayers_2 <- lapply(naplayers_2, function(x) { x["naInfo"] <- NULL; x })
          
          
          na_players_1 <- data.frame(rbind(rbindlist(naplayers_1, fill = T), fill = T))
          na_players_2 <- data.frame(rbind(rbindlist(naplayers_2, fill = T), fill = T))
          
          if(length(na_players_1$id)==0){
            team1_leftovers <- team1_player_data[-which(team1_player_data$page_url_2 %in% lineup_df$pageUrl),]
            team1_leftovers <- team1_leftovers[team1_leftovers$expected_goals_against==0,]
            team1_subs <- team1_leftovers[team1_leftovers$position_row==0,]
          }else{
            team1_leftovers <- team1_player_data[-which(team1_player_data$page_url_2 %in% lineup_df$pageUrl),]
            team1_leftovers <- team1_leftovers[-which(team1_leftovers$page_url_2 %in% na_players_1$pageUrl),]
            team1_leftovers <- team1_leftovers[team1_leftovers$expected_goals_against==0,]
            team1_subs <- team1_leftovers[team1_leftovers$position_row==0,]
          }
          
          if(length(na_players_2$id)==0){
            team2_leftovers <- team2_player_data[-which(team2_player_data$page_url_2 %in% lineup_df_2$pageUrl),]
            team2_leftovers <- team2_leftovers[team2_leftovers$expected_goals_against==0,]
            team2_subs <- team2_leftovers[team2_leftovers$position_row==0,]
          }else{
            team2_leftovers <- team2_player_data[-which(team2_player_data$page_url_2 %in% lineup_df_2$pageUrl),]
            team2_leftovers <- team2_leftovers[-which(team2_leftovers$page_url_2 %in% na_players_2$pageUrl),]
            team2_leftovers <- team2_leftovers[team2_leftovers$expected_goals_against==0,]
            team2_subs <- team2_leftovers[team2_leftovers$position_row==0,]
          }
          team1_subs$pageUrl <- team1_subs$page_url
          team1_subs$pageUrl2 <- team1_subs$page_url_2
          team2_subs$pageUrl <- team2_subs$page_url
          team2_subs$pageUrl2 <- team2_subs$page_url_2
        }else{
          
          
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["name"] <- NULL; x })
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["rating"] <- NULL; x })
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["fantasyScore"] <- NULL; x })
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["stats"] <- NULL; x })
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["teamData"] <- NULL; x })
            t$content$lineup$bench$benchArr[[1]] <- lapply(t$content$lineup$bench$benchArr[[1]], function(x) { x["events"] <- NULL; x })
            team1_subs <- rbindlist(t$content$lineup$bench$benchArr[[1]], fill = T)
            
            team1_subs$pageUrl2 <- team1_subs$pageUrl
            for(i in 1:length(team1_subs$pageUrl)){
              
              team1_subs$pageUrl[i] <- paste0("/",strsplit(team1_subs$pageUrl2[i],"/")[[1]][2],"/",strsplit(team1_subs$pageUrl2[i],"/")[[1]][3],"/")
              
            }
            
            team1_subs <- merge(team1_subs, team1_player_data, by.x = c("pageUrl2",
                                                                                                                        "positionRow"), by.y = c("page_url_2","position_row"),
                                  all.x = TRUE)
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["name"] <- NULL; x })
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["rating"] <- NULL; x })
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["fantasyScore"] <- NULL; x })
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["stats"] <- NULL; x })
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["teamData"] <- NULL; x })
            t$content$lineup$bench$benchArr[[2]] <- lapply(t$content$lineup$bench$benchArr[[2]], function(x) { x["events"] <- NULL; x })
            team2_subs <- rbindlist(t$content$lineup$bench$benchArr[[2]], fill = T)
            team2_subs$pageUrl2 <- team2_subs$pageUrl
            for(i in 1:length(team2_subs$pageUrl)){
              
              team2_subs$pageUrl[i] <- paste0("/",strsplit(team2_subs$pageUrl2[i],"/")[[1]][2],"/",strsplit(team2_subs$pageUrl2[i],"/")[[1]][3],"/")
              
            }
            team2_subs <- merge(team2_subs, team2_player_data, by.x = c("pageUrl2",
                                                                        "positionRow"), by.y = c("page_url_2","position_row"),
                                all.x = TRUE)
        }
        
        
        
        
        if(t$content$lineup$usingEnetpulseLineup==TRUE | t$content$lineup$usingOptaLineup==TRUE | t$content$lineup$simpleLineup==TRUE){
          lineup_list[[b]]$confirmed <- "confirmed"
        }else{
          lineup_list[[b]]$confirmed <- "expected"
        }
        
        
        
        
        if(length(which(duplicated(lineup_list[[b]][,4])))==0){
          
        }else{
          lineup_list[[b]] <- lineup_list[[b]][-which(duplicated(lineup_list[[b]][,4])),]
        }
        
        lineup_list[[b]]$pageUrl2 <- lineup_list[[b]]$pageUrl
        for(i in 1:length(lineup_list[[b]]$pageUrl)){
          
          lineup_list[[b]]$pageUrl[i] <- paste0("/",strsplit(lineup_list[[b]]$pageUrl2[i],"/")[[1]][2],"/",strsplit(lineup_list[[b]]$pageUrl2[i],"/")[[1]][3],"/")
          
        }
        
      }
    }
  }
  
  lineup_df <- rbindlist(lineup_list, fill = T)
  
  list(lineup_df,team1_subs,team2_subs)
  
}


league_analysis <- function(leagues){
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
  all_leagues <- vector("list",1)
  if(leagues=="MEX Liga MX"){
    all_leagues[[1]] <- c("MEX Liga MX Apertura","MEX Liga MX Clausura")
  }else{
    all_leagues[[1]] <- leagues
  }
  
  league_list <- vector("list",1)
  league_lines <- vector("list",1)
  for(a in 1:length(all_leagues)){
    
   if(length(all_leagues[[a]])>1){
     a_l_g <- vector("list",length(all_leagues[[a]]))
     for(xx in 1:length(all_leagues[[a]])){
       setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",all_leagues[[a]][xx])))
       
       league_game_files <- list.files()
       league_game_list <- vector("list",1)
       for(b in 1:length(league_game_files)){
         mfg <- read.csv(league_game_files[b])
         league_game_list[[b]] <- mfg
       }
       a_l_g[[xx]] <- rbindlist(league_game_list, fill = T)
     }
    all_league_games <- rbindlist(a_l_g, fill = T)
   }else{
     setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data/",all_leagues[[a]])))
     
     league_game_files <- list.files()
     league_game_list <- vector("list",1)
     for(b in 1:length(league_game_files)){
       mfg <- read.csv(league_game_files[b])
       league_game_list[[b]] <- mfg
     }
     all_league_games <- rbindlist(league_game_list, fill = T)
   }
    
    
    
    
    if(length(which(duplicated(all_league_games[,c(1:10)])))==0){
      
    }else{
      all_league_games <- all_league_games[-which(duplicated(all_league_games[,c(1:10)])),]
    }
    
    all_league_games$is_home_team[all_league_games$is_home_team==" TRUE"] <- TRUE
    
    if(length(which(all_league_games$team_rating>200))==0){
      
    }else{
      all_league_games <- all_league_games[which(all_league_games$team_rating<200),]
    }
    
    if(length(which(all_league_games$team_rating==0))==0){
      
    }else{
      all_league_games <- all_league_games[which(all_league_games$team_rating!=0),]
    }
    
    
    league_afc <- all_league_games %>%
      group_by(match_id) %>%
      summarise(minutes = sum(stats_minutes_played, na.rm = T),
                games = length(stats_minutes_played),
                shots = sum(stats_total_shots, na.rm = T),
                shotsongoal = sum(stats_shot_accuracy, na.rm = T),
                tackles = sum(stats_tackles_won, na.rm = T),
                goals = sum(stats_goals, na.rm = T),
                assists = sum(stats_assists, na.rm = T),
                passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
                minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
                expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)], na.rm = T),
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)], na.rm = T),
                expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)], na.rm = T),
                expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)], na.rm = T),
                goals_against = sum(stats_goals_conceded[!is.na(shotmap)], na.rm = T),
                avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T),
                avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T)
                #avg_elo_dif = mean(elo_dif)
      )
    
    
    tkl_line <- lm(league_afc$tackles[!is.na(league_afc$tackles)]~abs(league_afc$avg_quality[!is.na(league_afc$tackles)]))
    pass_line <- lm(league_afc$passes[!is.na(league_afc$passes)]~league_afc$avg_quality[!is.na(league_afc$passes)])
    shots_line <- lm(league_afc$shots[!is.na(league_afc$shots)]~league_afc$avg_quality[!is.na(league_afc$shots)])
    sog_line <- lm(league_afc$shotsongoal[!is.na(league_afc$shotsongoal)]~league_afc$avg_quality[!is.na(league_afc$shotsongoal)])
    xg_line <- lm(league_afc$expected_goals[!is.na(league_afc$expected_goals)]~league_afc$avg_quality[!is.na(league_afc$expected_goals)])
    xgot_line <- lm(league_afc$expected_goals_ot[!is.na(league_afc$expected_goals_ot)]~league_afc$avg_quality[!is.na(league_afc$expected_goals_ot)])
    xa_line <- lm(league_afc$expected_ast[!is.na(league_afc$expected_ast)]~league_afc$avg_quality[!is.na(league_afc$expected_ast)])
    
    stat_game <- all_league_games %>%
      group_by(match_id, team_name, is_home_team) %>%
      summarise(minutes = sum(stats_minutes_played[!is.na(stats_minutes_played)], na.rm = T),
                games = length(stats_minutes_played[!is.na(stats_minutes_played)]),
                shots = sum(stats_total_shots[!is.na(stats_minutes_played)], na.rm = T),
                shotsongoal = sum(stats_shot_accuracy[!is.na(stats_minutes_played)], na.rm = T),
                tackles = sum(stats_tackles_won[!is.na(stats_minutes_played)], na.rm = T),
                goals = sum(stats_goals[!is.na(stats_minutes_played)], na.rm = T),
                assists = sum(stats_assists[!is.na(stats_minutes_played)], na.rm = T),
                passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
                minutes_xg = sum(stats_minutes_played[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                goals_against = sum(stats_goals_conceded[!is.na(shotmap)&!is.na(stats_minutes_played)], na.rm = T),
                avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T),
                avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T)
                #avg_elo_dif = mean(elo_dif)
      )
    
    stat_game$passes_against <- 0
    stat_game$tackles_against <- 0
    stat_game$shots_against <- 0
    stat_game$sogs_against <- 0
    stat_game$xg_against <- 0
    stat_game$xgot_against <- 0
    stat_game$xa_against <- 0
    stat_game$goals_against <- 0
    
    for(i in 1:length(stat_game$team_name)){
      
      stat_game$passes_against[i] <- stat_game$passes[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$tackles_against[i] <- stat_game$tackles[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$shots_against[i] <- stat_game$shots[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$sogs_against[i] <- stat_game$shotsongoal[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$xg_against[i] <- stat_game$expected_goals[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$xgot_against[i] <- stat_game$expected_goals_ot[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$xa_against[i] <- stat_game$expected_ast[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      stat_game$goals_against[i] <- stat_game$goals[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
      
    }
    
    stat_game_xg <- stat_game[stat_game$expected_goals>0,]
    
    tkl_line_home <- lm(stat_game$tackles[!is.na(stat_game$tackles)&stat_game$is_home_team==TRUE]~abs(stat_game$avg_elo_dif[!is.na(stat_game$tackles)&stat_game$is_home_team==TRUE]))
    pass_line_home <- lm(stat_game$passes[!is.na(stat_game$passes)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$passes)&stat_game$is_home_team==TRUE])
    shots_line_home <- lm(stat_game$shots[!is.na(stat_game$shots)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$shots)&stat_game$is_home_team==TRUE])
    sog_line_home <- lm(stat_game$shotsongoal[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==TRUE])
    
    tkl_line_against_home <- lm(stat_game$tackles_against[!is.na(stat_game$tackles)&stat_game$is_home_team==TRUE]~abs(stat_game$avg_elo_dif[!is.na(stat_game$tackles)&stat_game$is_home_team==TRUE]))
    pass_line_against_home <- lm(stat_game$passes_against[!is.na(stat_game$passes)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$passes)&stat_game$is_home_team==TRUE])
    shots_line_against_home <- lm(stat_game$shots_against[!is.na(stat_game$shots)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$shots)&stat_game$is_home_team==TRUE])
    sog_line_against_home <- lm(stat_game$sogs_against[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==TRUE])
    
    tkl_line_away <- lm(stat_game$tackles[!is.na(stat_game$tackles)&stat_game$is_home_team==FALSE]~abs(stat_game$avg_elo_dif[!is.na(stat_game$tackles)&stat_game$is_home_team==FALSE]))
    pass_line_away <- lm(stat_game$passes[!is.na(stat_game$passes)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$passes)&stat_game$is_home_team==FALSE])
    shots_line_away <- lm(stat_game$shots[!is.na(stat_game$shots)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$shots)&stat_game$is_home_team==FALSE])
    sog_line_away <- lm(stat_game$shotsongoal[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==FALSE])
    
    tkl_line_against_away <- lm(stat_game$tackles_against[!is.na(stat_game$tackles)&stat_game$is_home_team==FALSE]~abs(stat_game$avg_elo_dif[!is.na(stat_game$tackles)&stat_game$is_home_team==FALSE]))
    pass_line_against_away <- lm(stat_game$passes_against[!is.na(stat_game$passes)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$passes)&stat_game$is_home_team==FALSE])
    shots_line_against_away <- lm(stat_game$shots_against[!is.na(stat_game$shots)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$shots)&stat_game$is_home_team==FALSE])
    sog_line_against_away <- lm(stat_game$sogs_against[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$shotsongoal)&stat_game$is_home_team==FALSE])
    
    xg_line_home <- lm(stat_game_xg$expected_goals[!is.na(stat_game_xg$expected_goals)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_goals)&stat_game_xg$is_home_team==TRUE])
    xgot_line_home <- lm(stat_game_xg$expected_goals_ot[!is.na(stat_game_xg$expected_goals_ot)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_goals_ot)&stat_game_xg$is_home_team==TRUE])
    xa_line_home <- lm(stat_game_xg$expected_ast[!is.na(stat_game_xg$expected_ast)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_ast)&stat_game_xg$is_home_team==TRUE])
    
    xg_line_away <- lm(stat_game_xg$expected_goals[!is.na(stat_game_xg$expected_goals)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_goals)&stat_game_xg$is_home_team==FALSE])
    xgot_line_away <- lm(stat_game_xg$expected_goals_ot[!is.na(stat_game_xg$expected_goals_ot)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_goals_ot)&stat_game_xg$is_home_team==FALSE])
    xa_line_away <- lm(stat_game_xg$expected_ast[!is.na(stat_game_xg$expected_ast)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$expected_ast)&stat_game_xg$is_home_team==FALSE])
    
    xg_against_line_home <- lm(stat_game_xg$xg_against[!is.na(stat_game_xg$xg_against)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xg_against)&stat_game_xg$is_home_team==TRUE])
    xgot_against_line_home <- lm(stat_game_xg$xgot_against[!is.na(stat_game_xg$xgot_against)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xgot_against)&stat_game_xg$is_home_team==TRUE])
    xa_against_line_home <- lm(stat_game_xg$xa_against[!is.na(stat_game_xg$xa_against)&stat_game_xg$is_home_team==TRUE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xa_against)&stat_game_xg$is_home_team==TRUE])
    
    xg_against_line_away <- lm(stat_game_xg$xg_against[!is.na(stat_game_xg$xg_against)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xg_against)&stat_game_xg$is_home_team==FALSE])
    xgot_against_line_away <- lm(stat_game_xg$xgot_against[!is.na(stat_game_xg$xgot_against)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xgot_against)&stat_game_xg$is_home_team==FALSE])
    xa_against_line_away <- lm(stat_game_xg$xa_against[!is.na(stat_game_xg$xa_against)&stat_game_xg$is_home_team==FALSE]~stat_game_xg$avg_elo_dif[!is.na(stat_game_xg$xa_against)&stat_game_xg$is_home_team==FALSE])
    
    
    stat_game$expected_tkls[stat_game$is_home_team==TRUE] <- (tkl_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+tkl_line_home$coefficients[[1]]
    stat_game$tkls_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$tackles[stat_game$is_home_team==TRUE]-stat_game$expected_tkls[stat_game$is_home_team==TRUE]
    
    stat_game$expected_passes[stat_game$is_home_team==TRUE] <- (pass_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+pass_line_home$coefficients[[1]]
    stat_game$pass_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$passes[stat_game$is_home_team==TRUE]-stat_game$expected_passes[stat_game$is_home_team==TRUE]
    
    stat_game$expected_shots[stat_game$is_home_team==TRUE] <- (shots_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+shots_line_home$coefficients[[1]]
    stat_game$shots_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$shots[stat_game$is_home_team==TRUE]-stat_game$expected_shots[stat_game$is_home_team==TRUE]
    
    stat_game$expected_sogs[stat_game$is_home_team==TRUE] <- (sog_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+sog_line_home$coefficients[[1]]
    stat_game$sogs_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$shotsongoal[stat_game$is_home_team==TRUE]-stat_game$expected_sogs[stat_game$is_home_team==TRUE]
    
    stat_game$expected_tkls_against[stat_game$is_home_team==TRUE] <- (tkl_line_against_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+tkl_line_against_home$coefficients[[1]]
    stat_game$tkls_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$tackles_against[stat_game$is_home_team==TRUE]-stat_game$expected_tkls_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_passes_against[stat_game$is_home_team==TRUE] <- (pass_line_against_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+pass_line_against_home$coefficients[[1]]
    stat_game$pass_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$passes_against[stat_game$is_home_team==TRUE]-stat_game$expected_passes_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_shots_against[stat_game$is_home_team==TRUE] <- (shots_line_against_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+shots_line_against_home$coefficients[[1]]
    stat_game$shots_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$shots_against[stat_game$is_home_team==TRUE]-stat_game$expected_shots_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_sogs_against[stat_game$is_home_team==TRUE] <- (sog_line_against_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+sog_line_against_home$coefficients[[1]]
    stat_game$sogs_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$sogs_against[stat_game$is_home_team==TRUE]-stat_game$expected_sogs_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_tkls[stat_game$is_home_team==FALSE] <- (tkl_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+tkl_line_away$coefficients[[1]]
    stat_game$tkls_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$tackles[stat_game$is_home_team==FALSE]-stat_game$expected_tkls[stat_game$is_home_team==FALSE]
    
    stat_game$expected_passes[stat_game$is_home_team==FALSE] <- (pass_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+pass_line_away$coefficients[[1]]
    stat_game$pass_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$passes[stat_game$is_home_team==FALSE]-stat_game$expected_passes[stat_game$is_home_team==FALSE]
    
    stat_game$expected_shots[stat_game$is_home_team==FALSE] <- (shots_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+shots_line_away$coefficients[[1]]
    stat_game$shots_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$shots[stat_game$is_home_team==FALSE]-stat_game$expected_shots[stat_game$is_home_team==FALSE]
    
    stat_game$expected_sogs[stat_game$is_home_team==FALSE] <- (sog_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+sog_line_away$coefficients[[1]]
    stat_game$sogs_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$shotsongoal[stat_game$is_home_team==FALSE]-stat_game$expected_sogs[stat_game$is_home_team==FALSE]
    
    stat_game$expected_tkls_against[stat_game$is_home_team==FALSE] <- (tkl_line_against_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+tkl_line_against_away$coefficients[[1]]
    stat_game$tkls_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$tackles_against[stat_game$is_home_team==FALSE]-stat_game$expected_tkls_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_passes_against[stat_game$is_home_team==FALSE] <- (pass_line_against_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+pass_line_against_away$coefficients[[1]]
    stat_game$pass_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$passes_against[stat_game$is_home_team==FALSE]-stat_game$expected_passes_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_shots_against[stat_game$is_home_team==FALSE] <- (shots_line_against_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+shots_line_against_away$coefficients[[1]]
    stat_game$shots_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$shots_against[stat_game$is_home_team==FALSE]-stat_game$expected_shots_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_sogs_against[stat_game$is_home_team==FALSE] <- (sog_line_against_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+sog_line_against_away$coefficients[[1]]
    stat_game$sogs_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$sogs_against[stat_game$is_home_team==FALSE]-stat_game$expected_sogs_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xg[stat_game$is_home_team==TRUE] <- (xg_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xg_line_home$coefficients[[1]]
    stat_game$xg_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$expected_goals[stat_game$is_home_team==TRUE]-stat_game$expected_xg[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xg[stat_game$is_home_team==FALSE] <- (xg_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xg_line_away$coefficients[[1]]
    stat_game$xg_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$expected_goals[stat_game$is_home_team==FALSE]-stat_game$expected_xg[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xgot[stat_game$is_home_team==TRUE] <- (xgot_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xgot_line_home$coefficients[[1]]
    stat_game$xgot_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$expected_goals_ot[stat_game$is_home_team==TRUE]-stat_game$expected_xgot[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xgot[stat_game$is_home_team==FALSE] <- (xgot_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xgot_line_away$coefficients[[1]]
    stat_game$xgot_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$expected_goals_ot[stat_game$is_home_team==FALSE]-stat_game$expected_xgot[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xa[stat_game$is_home_team==TRUE] <- (xa_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xa_line_home$coefficients[[1]]
    stat_game$xa_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$expected_ast[stat_game$is_home_team==TRUE]-stat_game$expected_xa[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xa[stat_game$is_home_team==FALSE] <- (xa_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xa_line_away$coefficients[[1]]
    stat_game$xa_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$expected_ast[stat_game$is_home_team==FALSE]-stat_game$expected_xa[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xg_against[stat_game$is_home_team==TRUE] <- (xg_against_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xg_against_line_home$coefficients[[1]]
    stat_game$xg_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$xg_against[stat_game$is_home_team==TRUE]-stat_game$expected_xg_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xg_against[stat_game$is_home_team==FALSE] <- (xg_against_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xg_against_line_away$coefficients[[1]]
    stat_game$xg_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$xg_against[stat_game$is_home_team==FALSE]-stat_game$expected_xg_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xgot_against[stat_game$is_home_team==TRUE] <- (xgot_against_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xgot_against_line_home$coefficients[[1]]
    stat_game$xgot_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$xgot_against[stat_game$is_home_team==TRUE]-stat_game$expected_xgot_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xgot_against[stat_game$is_home_team==FALSE] <- (xgot_against_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xgot_against_line_away$coefficients[[1]]
    stat_game$xgot_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$xgot_against[stat_game$is_home_team==FALSE]-stat_game$expected_xgot_against[stat_game$is_home_team==FALSE]
    
    stat_game$expected_xa_against[stat_game$is_home_team==TRUE] <- (xa_against_line_home$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==TRUE])+xa_against_line_home$coefficients[[1]]
    stat_game$xa_against_minus_xp[stat_game$is_home_team==TRUE] <- stat_game$xa_against[stat_game$is_home_team==TRUE]-stat_game$expected_xa_against[stat_game$is_home_team==TRUE]
    
    stat_game$expected_xa_against[stat_game$is_home_team==FALSE] <- (xa_against_line_away$coefficients[[2]]*stat_game$avg_elo_dif[stat_game$is_home_team==FALSE])+xa_against_line_away$coefficients[[1]]
    stat_game$xa_against_minus_xp[stat_game$is_home_team==FALSE] <- stat_game$xa_against[stat_game$is_home_team==FALSE]-stat_game$expected_xa_against[stat_game$is_home_team==FALSE]
    
    h_sd <- sd(stat_game$expected_goals_ot[stat_game$is_home_team==TRUE])
    a_sd <- sd(stat_game$expected_goals_ot[stat_game$is_home_team==FALSE])
    
    league_lines[[a]] <- list(stat_game,
                              pass_line_home,
                              tkl_line_home,
                              shots_line_home,
                              sog_line_home,
                              pass_line_against_home,
                              tkl_line_against_home,
                              shots_line_against_home,
                              sog_line_against_home,
                              pass_line_away,
                              tkl_line_away,
                              shots_line_away,
                              sog_line_away,
                              pass_line_against_away,
                              tkl_line_against_away,
                              shots_line_against_away,
                              sog_line_against_away,
                              xg_line_home,
                              xg_line_away,
                              xgot_line_home,
                              xgot_line_away,
                              xa_line_home,
                              xa_line_away,
                              xg_against_line_home,
                              xg_against_line_away,
                              xgot_against_line_home,
                              xgot_against_line_away,
                              xa_against_line_home,
                              xa_against_line_away,
                              pass_line,
                              tkl_line,
                              shots_line,
                              sog_line,
                              xg_line,
                              xa_line,
                              xgot_line,
                              h_sd,
                              a_sd)
    
   
    print(paste0("Done with ",unique(all_league_games$ccode)," ",unique(all_league_games$league)))
    
  }
  
  league_lines
}

#league_factors <- league_analysis("ENG Premier League")

formation_analysis <- function(formation_matchup){
  
  
  tackles_line_1_home <- NA
  tackles_line_2_home <- NA
  tackles_line_3_home <- NA
  tackles_line_4_home <- NA
  tackles_line_5_home <- NA
  passes_line_1_home <- NA
  passes_line_2_home <- NA
  passes_line_3_home <- NA
  passes_line_4_home <- NA
  passes_line_5_home <- NA
  shots_line_1_home <- NA
  shots_line_2_home <- NA
  shots_line_3_home <- NA
  shots_line_4_home <- NA
  shots_line_5_home <- NA
  shotsongoal_line_1_home <- NA
  shotsongoal_line_2_home <- NA
  shotsongoal_line_3_home <- NA
  shotsongoal_line_4_home <- NA
  shotsongoal_line_5_home <- NA
  tackles_against_line_1_home <- NA
  tackles_against_line_2_home <- NA
  tackles_against_line_3_home <- NA
  tackles_against_line_4_home <- NA
  tackles_against_line_5_home <- NA
  passes_against_line_1_home <- NA
  passes_against_line_2_home <- NA
  passes_against_line_3_home <- NA
  passes_against_line_4_home <- NA
  passes_against_line_5_home <- NA
  shots_against_line_1_home <- NA
  shots_against_line_2_home <- NA
  shots_against_line_3_home <- NA
  shots_against_line_4_home <- NA
  shots_against_line_5_home <- NA
  sogs_against_line_1_home <- NA
  sogs_against_line_2_home <- NA
  sogs_against_line_3_home <- NA
  sogs_against_line_4_home <- NA
  sogs_against_line_5_home <- NA
  tackles_line_1_away <- NA
  tackles_line_2_away <- NA
  tackles_line_3_away <- NA
  tackles_line_4_away <- NA
  tackles_line_5_away <- NA
  passes_line_1_away <- NA
  passes_line_2_away <- NA
  passes_line_3_away <- NA
  passes_line_4_away <- NA
  passes_line_5_away <- NA
  shots_line_1_away <- NA
  shots_line_2_away <- NA
  shots_line_3_away <- NA
  shots_line_4_away <- NA
  shots_line_5_away <- NA
  shotsongoal_line_1_away <- NA
  shotsongoal_line_2_away <- NA
  shotsongoal_line_3_away <- NA
  shotsongoal_line_4_away <- NA
  shotsongoal_line_5_away <- NA
  tackles_against_line_1_away <- NA
  tackles_against_line_2_away <- NA
  tackles_against_line_3_away <- NA
  tackles_against_line_4_away <- NA
  tackles_against_line_5_away <- NA
  passes_against_line_1_away <- NA
  passes_against_line_2_away <- NA
  passes_against_line_3_away <- NA
  passes_against_line_4_away <- NA
  passes_against_line_5_away <- NA
  shots_against_line_1_away <- NA
  shots_against_line_2_away <- NA
  shots_against_line_3_away <- NA
  shots_against_line_4_away <- NA
  shots_against_line_5_away <- NA
  sogs_against_line_1_away <- NA
  sogs_against_line_2_away <- NA
  sogs_against_line_3_away <- NA
  sogs_against_line_4_away <- NA
  sogs_against_line_5_away <- NA
  xg_line_1_home <- NA
  xg_line_2_home <- NA
  xg_line_3_home <- NA
  xg_line_4_home <- NA
  xg_line_5_home <- NA
  xgot_line_1_home <- NA
  xgot_line_2_home <- NA
  xgot_line_3_home <- NA
  xgot_line_4_home <- NA
  xgot_line_5_home <- NA
  xa_line_1_home <- NA
  xa_line_2_home <- NA
  xa_line_3_home <- NA
  xa_line_4_home <- NA
  xa_line_5_home <- NA
  xg_against_line_1_home <- NA
  xg_against_line_2_home <- NA
  xg_against_line_3_home <- NA
  xg_against_line_4_home <- NA
  xg_against_line_5_home <- NA
  xgot_against_line_1_home <- NA
  xgot_against_line_2_home <- NA
  xgot_against_line_3_home <- NA
  xgot_against_line_4_home <- NA
  xgot_against_line_5_home <- NA
  xa_against_line_1_home <- NA
  xa_against_line_2_home <- NA
  xa_against_line_3_home <- NA
  xa_against_line_4_home <- NA
  xa_against_line_5_home <- NA
  xg_line_1_away <- NA
  xg_line_2_away <- NA
  xg_line_3_away <- NA
  xg_line_4_away <- NA
  xg_line_5_away <- NA
  xgot_line_1_away <- NA
  xgot_line_2_away <- NA
  xgot_line_3_away <- NA
  xgot_line_4_away <- NA
  xgot_line_5_away <- NA
  xa_line_1_away <- NA
  xa_line_2_away <- NA
  xa_line_3_away <- NA
  xa_line_4_away <- NA
  xa_line_5_away <- NA
  xg_against_line_1_away <- NA
  xg_against_line_2_away <- NA
  xg_against_line_3_away <- NA
  xg_against_line_4_away <- NA
  xg_against_line_5_away <- NA
  xgot_against_line_1_away <- NA
  xgot_against_line_2_away <- NA
  xgot_against_line_3_away <- NA
  xgot_against_line_4_away <- NA
  xgot_against_line_5_away <- NA
  xa_against_line_1_away <- NA
  xa_against_line_2_away <- NA
  xa_against_line_3_away <- NA
  xa_against_line_4_away <- NA
  xa_against_line_5_away <- NA
  
  
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data")))
  
  all_formations <- formation_matchup
  formation_list <- vector("list",1)
  for(a in 1:length(all_formations)){
    
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Formation Data/",all_formations[a])))
    
    formation_game_files <- list.files()
    formation_game_list <- vector("list",1)
    for(b in 1:length(formation_game_files)){
      mfg <- read.csv(formation_game_files[b])
      formation_game_list[[b]] <- mfg
    }
    all_formation_games <- rbindlist(formation_game_list, fill = T)
    
    
    if(length(which(duplicated(all_formation_games[,c(1:10)])))==0){
      
    }else{
      all_formation_games <- all_formation_games[-which(duplicated(all_formation_games[,c(1:10)])),]
    }
    
    all_formation_games$is_home_team[all_formation_games$is_home_team==" TRUE"] <- TRUE
    
    if(length(which(all_formation_games$team_rating>200))==0){
      
    }else{
      all_formation_games <- all_formation_games[which(all_formation_games$team_rating<200),]
    }
    
    if(length(which(all_formation_games$team_rating==0))==0){
      
    }else{
      all_formation_games <- all_formation_games[which(all_formation_games$team_rating!=0),]
    }
    
    stat_afc <- all_formation_games %>%
      group_by(position_row, formation, is_home_team) %>%
      summarise(minutes = sum(stats_minutes_played[role!="Keeper"], na.rm = T),
                games = length(stats_minutes_played[role!="Keeper"]),
                shots = sum(stats_total_shots[role!="Keeper"], na.rm = T),
                shotsongoal = sum(stats_shot_accuracy[role!="Keeper"], na.rm = T),
                tackles = sum(stats_tackles_won[role!="Keeper"], na.rm = T),
                goals = sum(stats_goals[role!="Keeper"], na.rm = T),
                assists = sum(stats_assists[role!="Keeper"], na.rm = T),
                passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)&role!="Keeper"], na.rm = T),
                minutes_xg = sum(stats_minutes_played[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                goals_against = sum(stats_goals_conceded[!is.na(shotmap)&role!="Keeper"], na.rm = T),
                avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0&role!="Keeper"], na.rm = T),
                avg_elo = mean(team_rating[team_rating!=0&role!="Keeper"], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0&role!="Keeper"], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0&role!="Keeper"], na.rm = T) - mean(opp_rating[opp_rating!=0&role!="Keeper"], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0&role!="Keeper"], na.rm = T) + mean(opp_rating[opp_rating!=0&role!="Keeper"], na.rm = T)
                #avg_elo_dif = mean(elo_dif)
      )
    
    stat_afc$sog_90min <- 90*stat_afc$shotsongoal/stat_afc$minutes
    stat_afc$shots_90min <- 90*stat_afc$shots/stat_afc$minutes
    stat_afc$tkl_90min <- 90*stat_afc$tackles/stat_afc$minutes
    stat_afc$pass_90min <- 90*stat_afc$passes/stat_afc$minutes
    stat_afc$xg_90min <- 90*stat_afc$expected_goals/stat_afc$minutes_xg
    stat_afc$xgot_90min <- 90*stat_afc$expected_goals_ot/stat_afc$minutes_xg
    stat_afc$xa_90min <- 90*stat_afc$expected_ast/stat_afc$minutes_xg
    stat_afc$goal_90min <- 90*stat_afc$goals/stat_afc$minutes_xg
    stat_afc$ast_90min <- 90*stat_afc$assists/stat_afc$minutes_xg
    
    stat_afc$xgot_rate_90_min <- stat_afc$xgot_90min/stat_afc$xg_90min 
    stat_afc$xg_rate_90_min <- stat_afc$goal_90min/stat_afc$xg_90min 
    stat_afc$xa_rate_90_min <- stat_afc$ast_90min/stat_afc$xa_90min
    
    stat_game_position <- all_formation_games[!is.na(all_formation_games$team_rating)&!is.na(all_formation_games$opp_rating)] %>%
      group_by(team_name, match_id, position_row, is_home_team, formation) %>%
      summarise(players = length(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"]),
                games = sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"], na.rm = T),
                stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)&role!="Keeper"], na.rm = T),
                shots = sum(stats_total_shots[stats_minutes_played!=0&role!="Keeper"], na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"], na.rm = T)*90,
                stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)&role!="Keeper"], na.rm = T),
                shotsongoal = sum(stats_shot_accuracy, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"], na.rm = T)*90,
                stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)&role!="Keeper"], na.rm = T),
                tackles = sum(stats_tackles_won, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"], na.rm = T)*90,
                stat_goals = sum(stats_minutes_played[!is.na(stats_goals)&role!="Keeper"], na.rm = T),
                goals = sum(stats_goals[role!="Keeper"], na.rm = T),
                stat_assists = sum(stats_minutes_played[!is.na(stats_assists)&role!="Keeper"], na.rm = T),
                assists = sum(stats_assists[role!="Keeper"], na.rm = T),
                stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)&role!="Keeper"], na.rm = T),
                passes = sum(stats_accurate_passes[role!="Keeper"], na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"], na.rm = T)*90,
                expected_goals = sum(stats_expected_goals_x_g[role!="Keeper"&!is.na(shotmap)], na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"&!is.na(shotmap)], na.rm = T)*90,
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[role!="Keeper"&!is.na(shotmap)], na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"&!is.na(shotmap)], na.rm = T)*90,
                expected_ast = sum(stats_expected_assists_x_a[role!="Keeper"&!is.na(shotmap)], na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0&role!="Keeper"&!is.na(shotmap)], na.rm = T)*90,
                avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T))
    
    stat_game_position$shotsongoal[is.nan(stat_game_position$shotsongoal)] <- 0
    stat_game_position$tackles[is.nan(stat_game_position$tackles)] <- 0
    stat_game_position$shots[is.nan(stat_game_position$shots)] <- 0
    stat_game_position$passes[is.nan(stat_game_position$passes)] <- 0
    
    stat_game_position$shotsongoal[is.infinite(stat_game_position$shotsongoal)] <- 0
    stat_game_position$tackles[is.infinite(stat_game_position$tackles)] <- 0
    stat_game_position$shots[is.infinite(stat_game_position$shots)] <- 0
    stat_game_position$passes[is.infinite(stat_game_position$passes)] <- 0
    
    stat_game_position$expected_ast[is.infinite(stat_game_position$expected_ast)] <- NaN
    stat_game_position$expected_goals[is.infinite(stat_game_position$expected_goals)] <- NaN
    stat_game_position$expected_goals_ot[is.infinite(stat_game_position$expected_goals_ot)] <- NaN
    
    
    stat_game_position_home <- stat_game_position[which(stat_game_position$is_home_team==TRUE),]
    
    if(length(stat_afc$minutes[stat_afc$position_row==1&stat_afc$is_home_team==TRUE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==1&stat_afc$is_home_team==TRUE]==0){
        
      }else{
        shots_line_1_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots)])
        shotsongoal_line_1_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shotsongoal)])
        passes_line_1_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes)])
        tackles_line_1_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles)]))
        
        
        if(stat_afc$minutes_xg[stat_afc$position_row==1&stat_afc$is_home_team==TRUE]==0){
          
        }else{
          
          xg_line_1_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals)])
          xgot_line_1_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals_ot)])
          xa_line_1_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==2&stat_afc$is_home_team==TRUE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==2&stat_afc$is_home_team==TRUE]==0){
        
      }else{
        shots_line_2_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots)])
        shotsongoal_line_2_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shotsongoal)])
        passes_line_2_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes)])
        tackles_line_2_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==2&stat_afc$is_home_team==TRUE]==0){
          
        }else{
          
          xg_line_2_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals)])
          xgot_line_2_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals_ot)])
          xa_line_2_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==3&stat_afc$is_home_team==TRUE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==3&stat_afc$is_home_team==TRUE]==0){
        
      }else{
        shots_line_3_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots)])
        shotsongoal_line_3_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shotsongoal)])
        passes_line_3_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes)])
        tackles_line_3_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==3&stat_afc$is_home_team==TRUE]==0){
          
        }else{
          
          xg_line_3_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals)])
          xgot_line_3_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals_ot)])
          xa_line_3_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==4&stat_afc$is_home_team==TRUE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==4&stat_afc$is_home_team==TRUE]==0){
        
      }else{
        shots_line_4_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots)])
        shotsongoal_line_4_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shotsongoal)])
        passes_line_4_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes)])
        tackles_line_4_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==4&stat_afc$is_home_team==TRUE]==0){
          
        }else{
          
          xg_line_4_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals)])
          xgot_line_4_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals_ot)])
          xa_line_4_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==5&stat_afc$is_home_team==TRUE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==5&stat_afc$is_home_team==TRUE]==0){
        
      }else{
        shots_line_5_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots)])
        shotsongoal_line_5_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shotsongoal)])
        passes_line_5_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes)])
        tackles_line_5_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==5&stat_afc$is_home_team==TRUE]==0){
          
        }else{
          
          xg_line_5_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals)])
          xgot_line_5_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals_ot)])
          xa_line_5_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_ast)])
          
        }
      }
    }
    
    stat_game_position_away <- stat_game_position[which(stat_game_position$is_home_team==FALSE),]
    if(length(stat_afc$minutes[stat_afc$position_row==1&stat_afc$is_home_team==FALSE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==1&stat_afc$is_home_team==FALSE]==0){
        
      }else{
        shots_line_1_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots)])
        shotsongoal_line_1_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shotsongoal)])
        passes_line_1_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes)])
        tackles_line_1_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==1&stat_afc$is_home_team==FALSE]==0){
          
        }else{
          
          xg_line_1_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals)])
          xgot_line_1_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals_ot)])
          xa_line_1_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==2&stat_afc$is_home_team==FALSE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==2&stat_afc$is_home_team==FALSE]==0){
        
      }else{
        shots_line_2_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots)])
        shotsongoal_line_2_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shotsongoal)])
        passes_line_2_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes)])
        tackles_line_2_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==2&stat_afc$is_home_team==FALSE]==0){
          
        }else{
          
          xg_line_2_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals)])
          xgot_line_2_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals_ot)])
          xa_line_2_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==3&stat_afc$is_home_team==FALSE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==3&stat_afc$is_home_team==FALSE]==0){
        
      }else{
        shots_line_3_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots)])
        shotsongoal_line_3_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shotsongoal)])
        passes_line_3_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes)])
        tackles_line_3_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==3&stat_afc$is_home_team==FALSE]==0){
          
        }else{
          
          xg_line_3_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals)])
          xgot_line_3_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals_ot)])
          xa_line_3_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==4&stat_afc$is_home_team==FALSE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==4&stat_afc$is_home_team==FALSE]==0){
        
      }else{
        shots_line_4_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots)])
        shotsongoal_line_4_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shotsongoal)])
        passes_line_4_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes)])
        tackles_line_4_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==4&stat_afc$is_home_team==FALSE]==0){
          
        }else{
          
          xg_line_4_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals)])
          xgot_line_4_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals_ot)])
          xa_line_4_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_ast)])
          
        }
      }
    }
    
    if(length(stat_afc$minutes[stat_afc$position_row==5&stat_afc$is_home_team==FALSE])==0){
      
    }else{
      if(stat_afc$minutes[stat_afc$position_row==5&stat_afc$is_home_team==FALSE]==0){
        
      }else{
        shots_line_5_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots)])
        shotsongoal_line_5_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shotsongoal)])
        passes_line_5_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes)])
        tackles_line_5_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles)]))
        if(stat_afc$minutes_xg[stat_afc$position_row==5&stat_afc$is_home_team==FALSE]==0){
          
        }else{
          
          xg_line_5_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals)])
          xgot_line_5_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals_ot)])
          xa_line_5_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_ast)])
          
        }
      }
    }
    
    
     
    
    
    
  }
  
  list(tackles_line_1_home,
       tackles_line_2_home,
       tackles_line_3_home,
       tackles_line_4_home,
       tackles_line_5_home,
       passes_line_1_home,
       passes_line_2_home,
       passes_line_3_home,
       passes_line_4_home,
       passes_line_5_home,
       shots_line_1_home,
       shots_line_2_home,
       shots_line_3_home,
       shots_line_4_home,
       shots_line_5_home,
       shotsongoal_line_1_home,
       shotsongoal_line_2_home,
       shotsongoal_line_3_home,
       shotsongoal_line_4_home,
       shotsongoal_line_5_home,
       tackles_line_1_away,
       tackles_line_2_away,
       tackles_line_3_away,
       tackles_line_4_away,
       tackles_line_5_away,
       passes_line_1_away,
       passes_line_2_away,
       passes_line_3_away,
       passes_line_4_away,
       passes_line_5_away,
       shots_line_1_away,
       shots_line_2_away,
       shots_line_3_away,
       shots_line_4_away,
       shots_line_5_away,
       shotsongoal_line_1_away,
       shotsongoal_line_2_away,
       shotsongoal_line_3_away,
       shotsongoal_line_4_away,
       shotsongoal_line_5_away,
       xg_line_1_home,
       xg_line_2_home,
       xg_line_3_home,
       xg_line_4_home,
       xg_line_5_home,
       xgot_line_1_home,
       xgot_line_2_home,
       xgot_line_3_home,
       xgot_line_4_home,
       xgot_line_5_home,
       xa_line_1_home,
       xa_line_2_home,
       xa_line_3_home,
       xa_line_4_home,
       xa_line_5_home,
       xg_line_1_away,
       xg_line_2_away,
       xg_line_3_away,
       xg_line_4_away,
       xg_line_5_away,
       xgot_line_1_away,
       xgot_line_2_away,
       xgot_line_3_away,
       xgot_line_4_away,
       xgot_line_5_away,
       xa_line_1_away,
       xa_line_2_away,
       xa_line_3_away,
       xa_line_4_away,
       xa_line_5_away
       
  )
}

manager_analysis <- function(manager_ids){
  
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data")))
  all_managers <- list.files()
  all_m_ids <- gsub("[^[:digit:], ]", "", all_managers)
  manager_as <- 0
  for(i in 1:length(manager_ids)){
    manager_as[i] <- which(all_m_ids %in%  manager_ids[i])
  }
  
  
  manager_list <- vector("list",1)
  for(a in 1:length(manager_as)){
    
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Manager Data/",all_managers[manager_as[a]])))
    
    manager_game_files <- list.files()
    manager_game_list <- vector("list",1)
    for(b in 1:length(manager_game_files)){
      mfg <- read.csv(manager_game_files[b])
      manager_game_list[[b]] <- mfg
    }
    all_manager_games <- rbindlist(manager_game_list, fill = T)
    
    if(length(which(duplicated(all_manager_games[,c(1:10)])))==0){
      
    }else{
      all_manager_games <- all_manager_games[-which(duplicated(all_manager_games[,c(1:10)])),]
    }
    
    all_manager_games$is_home_team[all_manager_games$is_home_team==" TRUE"] <- TRUE
    
    if(length(which(all_manager_games$team_rating>200))==0){
      
    }else{
      all_manager_games <- all_manager_games[which(all_manager_games$team_rating<200),]
    }
    
    m_df <- data.frame(table(all_manager_games$manager))
    
    
    manager_id <- as.numeric(as.character(m_df$Var1[which(m_df$Freq==max(m_df$Freq))]))
    manager_json <- read_json(paste0("https://www.fotmob.com/api/playerData?id=",manager_id[1]))
    
    if(length(manager_json)==0){
      manager_name <- "No name data available"
    }else{
      manager_name <- manager_json$name
    }
    
    
    m_stats <- all_manager_games[as.numeric(as.character(all_manager_games$manager))==manager_id[1],]
    opp_stats <- all_manager_games[as.numeric(as.character(all_manager_games$manager))!=manager_id[1],]
    
   manager_afc <- m_stats %>%
      group_by(match_id) %>%
      summarise(minutes = sum(stats_minutes_played, na.rm = T),
                games = length(stats_minutes_played),
                shots = sum(stats_total_shots, na.rm = T),
                shotsongoal = sum(stats_shot_accuracy, na.rm = T),
                tackles = sum(stats_tackles_won, na.rm = T),
                goals = sum(stats_goals, na.rm = T),
                assists = sum(stats_assists, na.rm = T),
                passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
                minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
                expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)], na.rm = T),
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)], na.rm = T),
                expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)], na.rm = T),
                expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)], na.rm = T),
                goals_against = sum(stats_goals_conceded[!is.na(shotmap)], na.rm = T),
                avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T),
                avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T),
                opt_formation =  names(which.max(table(formation))),
                no_1s = length(team_rating[position_row==1]),
                no_2s = length(team_rating[position_row==2]),
                no_3s = length(team_rating[position_row==3]),
                no_4s = length(team_rating[position_row==4]),
                no_5s = length(team_rating[position_row==5])
                #avg_elo_dif = mean(elo_dif)
      )
   opp_afc <- opp_stats %>%
     group_by(match_id) %>%
     summarise(minutes = sum(stats_minutes_played, na.rm = T),
               games = length(stats_minutes_played),
               shots = sum(stats_total_shots, na.rm = T),
               shotsongoal = sum(stats_shot_accuracy, na.rm = T),
               tackles = sum(stats_tackles_won, na.rm = T),
               goals = sum(stats_goals, na.rm = T),
               assists = sum(stats_assists, na.rm = T),
               passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
               minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
               expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)], na.rm = T),
               expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)], na.rm = T),
               expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)], na.rm = T),
               expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)], na.rm = T),
               goals_against = sum(stats_goals_conceded[!is.na(shotmap)], na.rm = T),
               avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T),
               avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
               avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
               avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
               avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T)
               #avg_elo_dif = mean(elo_dif)
     )
    
    m_minutes <- sum(as.numeric(as.character(manager_afc$minutes)), na.rm = T)
    m_minutes_xg <- sum(as.numeric(as.character(manager_afc$minutes_xg)), na.rm = T)
    m_avg_passes <- mean(as.numeric(as.character(manager_afc$passes)), na.rm = T)
    m_avg_shots <- mean(as.numeric(as.character(manager_afc$shots)), na.rm = T)
    m_avg_sogs <- mean(as.numeric(as.character(manager_afc$shotsongoal)), na.rm = T)
    m_avg_xa <- mean(as.numeric(as.character(manager_afc$expected_ast[manager_afc$minutes_xg!=0])), na.rm = T)
    m_avg_xg <- mean(as.numeric(as.character(manager_afc$expected_goals[manager_afc$minutes_xg!=0])), na.rm = T)
    m_avg_xgot <- mean(as.numeric(as.character(manager_afc$expected_goals_ot[manager_afc$minutes_xg!=0])), na.rm = T)
    
    m_avg_elo_dif <- mean(as.numeric(as.character(manager_afc$avg_elo_dif)), na.rm = T)
    
    m_avg_rating <- mean(as.numeric(as.character(manager_afc$avg_elo)), na.rm = T)
    m_avg_opp_rating <- mean(as.numeric(as.character(manager_afc$avg_opp_elo)), na.rm = T)
     
    
    o_minutes <- sum(as.numeric(as.character(opp_afc$minutes)), na.rm = T)
    o_minutes_xg <- sum(as.numeric(as.character(opp_afc$minutes_xg)), na.rm = T) 
    o_avg_passes <- mean(as.numeric(as.character(opp_afc$passes)), na.rm = T)
    o_avg_shots <- mean(as.numeric(as.character(opp_afc$shots)), na.rm = T)
    o_avg_sogs <- mean(as.numeric(as.character(opp_afc$shotsongoal)), na.rm = T)
    o_avg_xa <- mean(as.numeric(as.character(opp_afc$expected_ast[manager_afc$minutes_xg!=0])), na.rm = T)
    o_avg_xg <- mean(as.numeric(as.character(opp_afc$expected_goals[manager_afc$minutes_xg!=0])), na.rm = T)
    o_avg_xgot <- mean(as.numeric(as.character(opp_afc$expected_goals_ot[manager_afc$minutes_xg!=0])), na.rm = T)
    
    o_avg_elo_dif <- mean(as.numeric(as.character(opp_afc$avg_elo_dif)), na.rm = T)
    
    o_avg_rating <- mean(as.numeric(as.character(opp_afc$avg_elo)), na.rm = T)
    o_avg_opp_rating <- mean(as.numeric(as.character(opp_afc$avg_opp_elo)), na.rm = T)
    
    manager_list[[a]] <- data.frame(manager_name,manager_id = manager_id[1], m_minutes,
                              m_minutes_xg,
                              m_avg_passes ,
                              m_avg_shots,
                              m_avg_sogs,
                              m_avg_xa,
                              m_avg_xg ,
                              m_avg_xgot ,
                              m_avg_elo_dif ,
                              m_avg_rating ,
                              m_avg_opp_rating ,
                              o_minutes ,
                              o_minutes_xg ,
                              o_avg_passes ,
                              o_avg_shots ,
                              o_avg_sogs ,
                              o_avg_xa ,
                              o_avg_xg ,
                              o_avg_xgot ,
                              o_avg_elo_dif ,
                              o_avg_rating ,
                              o_avg_opp_rating,
                              formation = as.character(data.frame(table(manager_afc$opt_formation))$Var1[data.frame(table(manager_afc$opt_formation))$Freq==max(data.frame(table(manager_afc$opt_formation))$Freq)[1]]),
                              ones = mean(manager_afc$no_1s),
                              twos = mean(manager_afc$no_2s),
                              threes =mean(manager_afc$no_3s),
                              fours =mean(manager_afc$no_4s),
                              fives = mean(manager_afc$no_5s))
    
    
    
  }
  manager_df <- rbindlist(manager_list)
}


manager_expected <- function( league_lines, manager_df){
  
  stat_game <- league_lines[[1]][[1]]
  pass_line_home <- league_lines[[1]][[2]]
  tkl_line_home <- league_lines[[1]][[3]]
  shots_line_home <- league_lines[[1]][[4]]
  sog_line_home <- league_lines[[1]][[5]]
  pass_line_against_home <- league_lines[[1]][[6]]
  tkl_line_against_home <- league_lines[[1]][[7]]
  shots_line_against_home <- league_lines[[1]][[8]]
  sog_line_against_home <- league_lines[[1]][[9]]
  pass_line_away <- league_lines[[1]][[10]]
  tkl_line_away <- league_lines[[1]][[11]]
  shots_line_away <- league_lines[[1]][[12]]
  sog_line_away <- league_lines[[1]][[13]]
  pass_line_against_away <- league_lines[[1]][[14]]
  tkl_line_against_away <- league_lines[[1]][[15]]
  shots_line_against_away <- league_lines[[1]][[16]]
  sog_line_against_away <- league_lines[[1]][[17]]
  xg_line_home<- league_lines[[1]][[18]]
  xg_line_away<- league_lines[[1]][[19]]
  xgot_line_home<- league_lines[[1]][[20]]
  xgot_line_away<- league_lines[[1]][[21]]
  xa_line_home<- league_lines[[1]][[22]]
  xa_line_away<- league_lines[[1]][[23]]
  xg_against_line_home<- league_lines[[1]][[24]]
  xg_against_line_away<- league_lines[[1]][[25]]
  xgot_against_line_home<- league_lines[[1]][[26]]
  xgot_against_line_away<- league_lines[[1]][[27]]
  xa_against_line_home<- league_lines[[1]][[28]]
  xa_against_line_away<- league_lines[[1]][[29]]
  pass_line <- league_lines[[1]][[30]]
  tkl_line<- league_lines[[1]][[31]]
  shots_line<- league_lines[[1]][[32]]
  sog_line<- league_lines[[1]][[33]]
  xg_line<- league_lines[[1]][[34]]
  xa_line<- league_lines[[1]][[35]]
  xgot_line<- league_lines[[1]][[36]]
  
  m_stats <- vector("list",1)
  for(g in 1:length(manager_df$manager_name)){
    
    
    elo_dif_1 <- manager_df$m_avg_elo_dif[g]
    elo_dif_2 <- manager_df$o_avg_elo_dif[g]
    quality <- manager_df$m_avg_rating[g]+manager_df$o_avg_rating[g]
    
    
    #Passes Overall 
    
    o_passes <- (pass_line$coefficients[[2]]*quality)+pass_line$coefficients[[1]]
    
    #Passes 
    team1_passes <- (pass_line_home$coefficients[[2]]*elo_dif_1)+pass_line_home$coefficients[[1]]
    team2_passes_against <- (pass_line_against_away$coefficients[[2]]*elo_dif_2)+pass_line_against_away$coefficients[[1]]
    
    team1_passes <- mean(c(team1_passes,team2_passes_against), na.rm = T)
    
    team2_passes <-(pass_line_away$coefficients[[2]]*elo_dif_2)+pass_line_away$coefficients[[1]]
    team1_passes_against <- (pass_line_against_home$coefficients[[2]]*elo_dif_1)+pass_line_against_home$coefficients[[1]]
    
    team2_passes <- mean(c(team2_passes,team1_passes_against), na.rm = T)
    
    passes_1 <- (team1_passes/sum(team1_passes,team2_passes))*o_passes
    passes_2 <- (team2_passes/sum(team1_passes,team2_passes))*o_passes
    
    m_passes <- mean(team1_passes,team2_passes)
    
    #Tackles
    
    team1_tackles <- (tkl_line_home$coefficients[[2]]*(elo_dif_1))+tkl_line_home$coefficients[[1]]
    team2_tackles_against <- (tkl_line_against_away$coefficients[[2]]*(elo_dif_2))+tkl_line_against_away$coefficients[[1]]
    
    team1_tackles <- mean(c(team1_tackles,team2_tackles_against), na.rm = T)
    
    team2_tackles <- (tkl_line_away$coefficients[[2]]*(elo_dif_2))+tkl_line_away$coefficients[[1]]
    team1_tackles_against <- (tkl_line_against_home$coefficients[[2]]*(elo_dif_1))+tkl_line_against_home$coefficients[[1]]
    
    team2_tackles <- mean(c(team2_tackles,team1_tackles_against), na.rm = T)
    
    m_tackles <- mean(team1_tackles,team2_tackles)
    
    team1_shots <- (shots_line_home$coefficients[[2]]*elo_dif_1)+shots_line_home$coefficients[[1]]
    team2_shots_against <- (shots_line_against_away$coefficients[[2]]*(elo_dif_2))+shots_line_against_away$coefficients[[1]]
    
    team1_shots <- mean(c(team1_shots,team2_shots_against), na.rm = T)
    
    team2_shots <-(shots_line_away$coefficients[[2]]*elo_dif_2)+shots_line_away$coefficients[[1]]
    team1_shots_against <- (shots_line_against_home$coefficients[[2]]*(elo_dif_1))+shots_line_against_home$coefficients[[1]]
    
    team2_shots <- mean(c(team2_shots,team1_shots_against), na.rm = T)
    
    
    m_shots <- mean(team1_shots,team2_shots)
    
    
    
    team1_sogs <- (sog_line_home$coefficients_home[[2]]*elo_dif_1)+sog_line_home$coefficients_home[[1]]
    team2_sogs_against <- (sog_line_against_away$coefficients[[2]]*(elo_dif_2))+sog_line_against_away$coefficients[[1]]
    
    team1_sogs <- mean(c(team1_sogs,team2_sogs_against), na.rm = T)
    
    
    team2_sogs <-(sog_line_away$coefficients[[2]]*elo_dif_2)+sog_line_away$coefficients[[1]]
    team1_sogs_against <- (sog_line_against_home$coefficients[[2]]*(elo_dif_1))+sog_line_against_home$coefficients[[1]]
    
    team2_sogs <- mean(c(team2_sogs,team1_sogs_against), na.rm = T)
    
    m_sogs <- mean(team1_sogs,team2_sogs)
    
    
    #Tackles
    
    team1_xg <- (xg_line_home$coefficients[[2]]*(elo_dif_1))+xg_line_home$coefficients[[1]]
    team2_xg_against <- (xg_against_line_away$coefficients[[2]]*(elo_dif_2))+xg_against_line_away$coefficients[[1]]
    
    team1_xg <- mean(c(team1_xg,team2_xg_against), na.rm = T)
    
    
    team2_xg <- (xg_line_away$coefficients[[2]]*(elo_dif_2))+xg_line_away$coefficients[[1]]
    team1_xg_against <- (xg_against_line_home$coefficients[[2]]*(elo_dif_1))+xg_against_line_home$coefficients[[1]]
    
    team2_xg <- mean(c(team2_xg,team1_xg_against), na.rm = T)
    
    m_xg <- mean(team1_xg,team2_xg)
    
    
    
    team1_xgot <- (xgot_line_home$coefficients[[2]]*(elo_dif_1))+xgot_line_home$coefficients[[1]]
    team2_xgot_against <- (xgot_against_line_away$coefficients[[2]]*(elo_dif_2))+xgot_against_line_away$coefficients[[1]]
    
    team1_xgot <- mean(c(team1_xgot,team2_xgot_against), na.rm = T)
    
    
    team2_xgot <- (xgot_line_away$coefficients[[2]]*(elo_dif_2))+xgot_line_away$coefficients[[1]]
    team1_xgot_against <- (xgot_against_line_home$coefficients[[2]]*(elo_dif_1))+xgot_against_line_home$coefficients[[1]]
    
    team2_xgot <- mean(c(team2_xgot,team1_xgot_against), na.rm = T)
    
    m_xgot <- mean(team1_xgot,team2_xgot)
    
    team1_xa <- (xa_line_home$coefficients[[2]]*(elo_dif_1))+xa_line_home$coefficients[[1]]
    team2_xa_against <- (xa_against_line_away$coefficients[[2]]*(elo_dif_2))+xa_against_line_away$coefficients[[1]]
    
    team1_xa <- mean(c(team1_xa,team2_xa_against), na.rm = T)
    
    team2_xa <- (xa_line_away$coefficients[[2]]*(elo_dif_2))+xa_line_away$coefficients[[1]]
    team1_xa_against <- (xa_against_line_home$coefficients[[2]]*(elo_dif_1))+xa_against_line_home$coefficients[[1]]
    
    team2_xa <- mean(c(team2_xa,team1_xa_against), na.rm = T)
    
    m_xa <- mean(team1_xa,team2_xa)
    
    #Passes Overall 
    
    o_passes <- (pass_line$coefficients[[2]]*quality)+pass_line$coefficients[[1]]
    
    #Passes 
    team1_passes <- (pass_line_home$coefficients[[2]]*elo_dif_2)+pass_line_home$coefficients[[1]]
    team2_passes_against <- (pass_line_against_away$coefficients[[2]]*(elo_dif_1))+pass_line_against_away$coefficients[[1]]
    
    team1_passes <- mean(c(team1_passes,team2_passes_against), na.rm = T)
    
    team2_passes <-(pass_line_away$coefficients[[2]]*elo_dif_1)+pass_line_away$coefficients[[1]]
    team1_passes_against <- (pass_line_against_home$coefficients[[2]]*(elo_dif_2))+pass_line_against_home$coefficients[[1]]
    
    team2_passes <- mean(c(team2_passes,team1_passes_against), na.rm = T)
    
    passes_1 <- (team1_passes/sum(team1_passes,team2_passes))*o_passes
    passes_2 <- (team2_passes/sum(team1_passes,team2_passes))*o_passes
    
    om_passes <- mean(team1_passes,team2_passes)
    
    #Tackles
    
    team1_tackles <- (tkl_line_home$coefficients[[2]]*elo_dif_2)+tkl_line_home$coefficients[[1]]
    team2_tackles_against <- (tkl_line_against_away$coefficients[[2]]*(elo_dif_1))+tkl_line_against_away$coefficients[[1]]
    
    team1_tackles <- mean(c(team1_tackles,team2_tackles_against), na.rm = T)
    
    team2_tackles <- (tkl_line_away$coefficients[[2]]*(elo_dif_1))+tkl_line_away$coefficients[[1]]
    team1_tackles_against <- (tkl_line_against_home$coefficients[[2]]*(elo_dif_2))+tkl_line_against_home$coefficients[[1]]
    
    team2_tackles <- mean(c(team2_tackles,team1_tackles_against), na.rm = T)
    
    om_tackles <- mean(team1_tackles,team2_tackles)
    
    team1_shots <- (shots_line_home$coefficients[[2]]*elo_dif_2)+shots_line_home$coefficients[[1]]
    team2_shots_against <- (shots_line_against_away$coefficients[[2]]*(elo_dif_1))+shots_line_against_away$coefficients[[1]]
    
    team1_shots <- mean(c(team1_shots,team2_shots_against), na.rm = T)
    
    team2_shots <-(shots_line_away$coefficients[[2]]*elo_dif_1)+shots_line_away$coefficients[[1]]
    team1_shots_against <- (shots_line_against_home$coefficients[[2]]*(elo_dif_2))+shots_line_against_home$coefficients[[1]]
    
    team2_shots <- mean(c(team2_shots,team1_shots_against), na.rm = T)
    
    
    om_shots <- mean(team1_shots,team2_shots)
    
    
    
    team1_sogs <- (sog_line_home$coefficients_home[[2]]*elo_dif_2)+sog_line_home$coefficients_home[[1]]
    team2_sogs_against <- (sog_line_against_away$coefficients[[2]]*(elo_dif_1))+sog_line_against_away$coefficients[[1]]
    
    team1_sogs <- mean(c(team1_sogs,team2_sogs_against), na.rm = T)
    
    
    team2_sogs <-(sog_line_away$coefficients[[2]]*elo_dif_1)+sog_line_away$coefficients[[1]]
    team1_sogs_against <- (sog_line_against_home$coefficients[[2]]*(elo_dif_2))+sog_line_against_home$coefficients[[1]]
    
    team2_sogs <- mean(c(team2_sogs,team1_sogs_against), na.rm = T)
    
    om_sogs <- mean(team1_sogs,team2_sogs)
    
    
    #Tackles
    
    team1_xg <- (xg_line_home$coefficients[[2]]*(elo_dif_2))+xg_line_home$coefficients[[1]]
    team2_xg_against <- (xg_against_line_away$coefficients[[2]]*(elo_dif_1))+xg_against_line_away$coefficients[[1]]
    
    team1_xg <- mean(c(team1_xg,team2_xg_against), na.rm = T)
    
    
    team2_xg <- (xg_line_away$coefficients[[2]]*(elo_dif_1))+xg_line_away$coefficients[[1]]
    team1_xg_against <- (xg_against_line_home$coefficients[[2]]*(elo_dif_2))+xg_against_line_home$coefficients[[1]]
    
    team2_xg <- mean(c(team2_xg,team1_xg_against), na.rm = T)
    
    om_xg <- mean(team1_xg,team2_xg)
    
    
    
    team1_xgot <- (xgot_line_home$coefficients[[2]]*(elo_dif_2))+xgot_line_home$coefficients[[1]]
    team2_xgot_against <- (xgot_against_line_away$coefficients[[2]]*(elo_dif_1))+xgot_against_line_away$coefficients[[1]]
    
    team1_xgot <- mean(c(team1_xgot,team2_xgot_against), na.rm = T)
    
    
    team2_xgot <- (xgot_line_away$coefficients[[2]]*(elo_dif_1))+xgot_line_away$coefficients[[1]]
    team1_xgot_against <- (xgot_against_line_home$coefficients[[2]]*(elo_dif_2))+xgot_against_line_home$coefficients[[1]]
    
    team2_xgot <- mean(c(team2_xgot,team1_xgot_against), na.rm = T)
    
    om_xgot <- mean(team1_xgot,team2_xgot)
    
    team1_xa <- (xa_line_home$coefficients[[2]]*(elo_dif_2))+xa_line_home$coefficients[[1]]
    team2_xa_against <- (xa_against_line_away$coefficients[[2]]*(elo_dif_1))+xa_against_line_away$coefficients[[1]]
    
    team1_xa <- mean(c(team1_xa,team2_xa_against), na.rm = T)
    
    team2_xa <- (xa_line_away$coefficients[[2]]*(elo_dif_1))+xa_line_away$coefficients[[1]]
    team1_xa_against <- (xa_against_line_home$coefficients[[2]]*(elo_dif_2))+xa_against_line_home$coefficients[[1]]
    
    team2_xa <- mean(c(team2_xa,team1_xa_against), na.rm = T)
    
    om_xa <- mean(team1_xa,team2_xa)
    
    m_stats[[g]] <- data.frame(manager_name = manager_df$manager_name[g],
                               m_passes,
                               m_tackles,
                               m_shots,
                               m_sogs,
                               m_xg,
                               m_xgot,
                               m_xa,
                               om_passes,
                               om_tackles,
                               om_shots,
                               om_sogs,
                               om_xg,
                               om_xgot,
                               om_xa)
    
  }
  
  
  m_stats
  
}

team_analysis <- function(teams){
  
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data")))
  
  all_teams <- teams
  team_list <- vector("list",1)
  for(a in 1:length(all_teams)){
    
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Team Data/",all_teams[a])))
    
    team_game_files <- list.files()
    team_game_list <- vector("list",1)
    for(b in 1:length(team_game_files)){
      mfg <- read.csv(team_game_files[b])
      team_game_list[[b]] <- mfg
    }
    all_team_games <- rbindlist(team_game_list, fill = T)
    
    if(length(which(duplicated(all_team_games[,c(1:10)])))==0){
      
    }else{
      all_team_games <- all_team_games[-which(duplicated(all_team_games[,c(1:10)])),]
    }
    
    all_team_games$is_home_team[all_team_games$is_home_team==" TRUE"] <- TRUE
    
    if(length(which(all_team_games$team_rating>200))==0){
      
    }else{
      all_team_games <- all_team_games[which(all_team_games$team_rating<200),]
    }
    
    if(length(which(all_team_games$team_rating==0))==0){
      
    }else{
      all_team_games <- all_team_games[which(all_team_games$team_rating!=0),]
    }
    
    team_afc <- all_team_games %>%
      group_by(match_id, is_home_team, team_name) %>%
      summarise(minutes = sum(stats_minutes_played, na.rm = T),
                games = length(stats_minutes_played),
                shots = sum(stats_total_shots, na.rm = T),
                shotsongoal = sum(stats_shot_accuracy, na.rm = T),
                tackles = sum(stats_tackles_won, na.rm = T),
                goals = sum(stats_goals, na.rm = T),
                assists = sum(stats_assists, na.rm = T),
                passes = sum(stats_accurate_passes[!is.na(stats_minutes_played)], na.rm = T),
                minutes_xg = sum(stats_minutes_played[!is.na(shotmap)], na.rm = T),
                expected_goals = sum(stats_expected_goals_x_g[!is.na(shotmap)], na.rm = T),
                expected_goals_ot = sum(stats_expected_goals_on_target_x_got[!is.na(shotmap)], na.rm = T),
                expected_ast = sum(stats_expected_assists_x_a[!is.na(shotmap)], na.rm = T),
                expected_goals_against = sum(stats_x_got_faced[!is.na(shotmap)], na.rm = T),
                goals_against = sum(stats_goals_conceded[!is.na(shotmap)], na.rm = T),
                avg_rating = mean(stats_fot_mob_rating[stats_fot_mob_rating!=0], na.rm = T),
                avg_elo = mean(team_rating[team_rating!=0], na.rm = T),
                avg_opp_elo = mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_elo_dif = mean(team_rating[team_rating!=0], na.rm = T) - mean(opp_rating[opp_rating!=0], na.rm = T),
                avg_quality = mean(team_rating[team_rating!=0], na.rm = T) + mean(opp_rating[opp_rating!=0], na.rm = T)
                #avg_elo_dif = mean(elo_dif)
      )
    
    opp_afc <- team_afc[which(team_afc$team_name!=all_teams[a]),]
    team_afc <- team_afc[which(team_afc$team_name==all_teams[a]),]
    
    h_avg_passes <- mean(as.numeric(as.character(team_afc$passes[team_afc$is_home_team==T])), na.rm = T)
    h_avg_shots <- mean(as.numeric(as.character(team_afc$shots[team_afc$is_home_team==T])), na.rm = T)
    h_avg_sogs <- mean(as.numeric(as.character(team_afc$shotsongoal[team_afc$is_home_team==T])), na.rm = T)
    h_avg_xa <- mean(as.numeric(as.character(team_afc$expected_ast[team_afc$minutes_xg!=0&team_afc$is_home_team==T])), na.rm = T)
    h_avg_xg <- mean(as.numeric(as.character(team_afc$expected_goals[team_afc$minutes_xg!=0&team_afc$is_home_team==T])), na.rm = T)
    h_avg_xgot <- mean(as.numeric(as.character(team_afc$expected_goals_ot[team_afc$minutes_xg!=0&team_afc$is_home_team==T])), na.rm = T)
    h_goal_xg_rate <- mean(as.numeric(as.character(team_afc$goals[team_afc$minutes_xg!=0&team_afc$is_home_team==T]))-
                             as.numeric(as.character(team_afc$expected_goals[team_afc$minutes_xg!=0&team_afc$is_home_team==T])), na.rm = T)
    h_goal_xa_rate <- mean(as.numeric(as.character(team_afc$assists[team_afc$minutes_xg!=0&team_afc$is_home_team==T]))-
                             as.numeric(as.character(team_afc$expected_ast[team_afc$minutes_xg!=0&team_afc$is_home_team==T])), na.rm = T)
    h_quality <-  mean(as.numeric(as.character(team_afc$avg_quality[team_afc$is_home_team==T])), na.rm = T)
    h_e_dif <-  mean(as.numeric(as.character(team_afc$avg_elo_dif[team_afc$is_home_team==T])), na.rm = T)
    
    a_avg_passes <- mean(as.numeric(as.character(team_afc$passes[team_afc$is_home_team==F])), na.rm = T)
    a_avg_shots <- mean(as.numeric(as.character(team_afc$shots[team_afc$is_home_team==F])), na.rm = T)
    a_avg_sogs <- mean(as.numeric(as.character(team_afc$shotsongoal[team_afc$is_home_team==F])), na.rm = T)
    a_avg_xa <- mean(as.numeric(as.character(team_afc$expected_ast[team_afc$minutes_xg!=0&team_afc$is_home_team==F])), na.rm = T)
    a_avg_xg <- mean(as.numeric(as.character(team_afc$expected_goals[team_afc$minutes_xg!=0&team_afc$is_home_team==F])), na.rm = T)
    a_avg_xgot <- mean(as.numeric(as.character(team_afc$expected_goals_ot[team_afc$minutes_xg!=0&team_afc$is_home_team==F])), na.rm = T)
    a_goal_xg_rate <- mean(as.numeric(as.character(team_afc$goals[team_afc$minutes_xg!=0&team_afc$is_home_team==F]))-
                             as.numeric(as.character(team_afc$expected_goals[team_afc$minutes_xg!=0&team_afc$is_home_team==F])), na.rm = T)
    a_goal_xa_rate <- mean(as.numeric(as.character(team_afc$assists[team_afc$minutes_xg!=0&team_afc$is_home_team==F]))-
                             as.numeric(as.character(team_afc$expected_ast[team_afc$minutes_xg!=0&team_afc$is_home_team==F])), na.rm = T)
    a_quality <-  mean(as.numeric(as.character(team_afc$avg_quality[team_afc$is_home_team==F])), na.rm = T)
    a_e_dif <-  mean(as.numeric(as.character(team_afc$avg_elo_dif[team_afc$is_home_team==F])), na.rm = T)
    
    oh_avg_passes <- mean(as.numeric(as.character(opp_afc$passes[opp_afc$is_home_team==T])), na.rm = T)
    oh_avg_shots <- mean(as.numeric(as.character(opp_afc$shots[opp_afc$is_home_team==T])), na.rm = T)
    oh_avg_sogs <- mean(as.numeric(as.character(opp_afc$shotsongoal[opp_afc$is_home_team==T])), na.rm = T)
    oh_avg_xa <- mean(as.numeric(as.character(opp_afc$expected_ast[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T])), na.rm = T)
    oh_avg_xg <- mean(as.numeric(as.character(opp_afc$expected_goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T])), na.rm = T)
    oh_avg_xgot <- mean(as.numeric(as.character(opp_afc$expected_goals_ot[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T])), na.rm = T)
    oh_goal_xg_rate <- mean(as.numeric(as.character(opp_afc$goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T]))-
                             as.numeric(as.character(opp_afc$expected_goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T])), na.rm = T)
    oh_goal_xa_rate <- mean(as.numeric(as.character(opp_afc$assists[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T]))-
                             as.numeric(as.character(opp_afc$expected_ast[opp_afc$minutes_xg!=0&opp_afc$is_home_team==T])), na.rm = T)
    oh_quality <-  mean(as.numeric(as.character(opp_afc$avg_quality[opp_afc$is_home_team==T])), na.rm = T)
    oh_e_dif <-  mean(as.numeric(as.character(opp_afc$avg_elo_dif[opp_afc$is_home_team==T])), na.rm = T)
    
    oa_avg_passes <- mean(as.numeric(as.character(opp_afc$passes[opp_afc$is_home_team==F])), na.rm = T)
    oa_avg_shots <- mean(as.numeric(as.character(opp_afc$shots[opp_afc$is_home_team==F])), na.rm = T)
    oa_avg_sogs <- mean(as.numeric(as.character(opp_afc$shotsongoal[opp_afc$is_home_team==F])), na.rm = T)
    oa_avg_xa <- mean(as.numeric(as.character(opp_afc$expected_ast[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F])), na.rm = T)
    oa_avg_xg <- mean(as.numeric(as.character(opp_afc$expected_goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F])), na.rm = T)
    oa_avg_xgot <- mean(as.numeric(as.character(opp_afc$expected_goals_ot[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F])), na.rm = T)
    oa_goal_xg_rate <- mean(as.numeric(as.character(opp_afc$goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F]))-
                             as.numeric(as.character(opp_afc$expected_goals[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F])), na.rm = T)
    oa_goal_xa_rate <- mean(as.numeric(as.character(opp_afc$assists[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F]))-
                             as.numeric(as.character(opp_afc$expected_ast[opp_afc$minutes_xg!=0&opp_afc$is_home_team==F])), na.rm = T)
    oa_quality <-  mean(as.numeric(as.character(opp_afc$avg_quality[opp_afc$is_home_team==F])), na.rm = T)
    oa_e_dif <-  mean(as.numeric(as.character(opp_afc$avg_elo_dif[opp_afc$is_home_team==F])), na.rm = T)
    
    team_list[[a]] <- data.frame(team = all_teams[a],
                                 h_avg_passes,
                                 h_avg_shots,
                                 h_avg_sogs,
                                 h_avg_xa,
                                 h_avg_xg,
                                 h_avg_xgot,
                                 h_goal_xg_rate,
                                 h_goal_xa_rate,
                                 h_quality,
                                 h_e_dif,
                                 
                                 a_avg_passes,
                                 a_avg_shots ,
                                 a_avg_sogs,
                                 a_avg_xa ,
                                 a_avg_xg,
                                 a_avg_xgot,
                                 a_goal_xg_rate,
                                 a_goal_xa_rate,
                                 a_quality ,
                                 a_e_dif ,
                                 
                                 oh_avg_passes ,
                                 oh_avg_shots,
                                 oh_avg_sogs,
                                 oh_avg_xa,
                                 oh_avg_xg ,
                                 oh_avg_xgot,
                                 oh_goal_xg_rate ,
                                 oh_goal_xa_rate ,
                                 oh_quality ,
                                 oh_e_dif ,
                                 
                                 oa_avg_passes,
                                 oa_avg_shots ,
                                 oa_avg_sogs ,
                                 oa_avg_xa ,
                                 oa_avg_xg ,
                                 oa_avg_xgot,
                                 oa_goal_xg_rate ,
                                 oa_goal_xa_rate ,
                                 oa_quality ,
                                 oa_e_dif
    )
    
    print(paste0("Done with ",all_teams[a]))
    
  }
  team_df <- rbindlist(team_list)
  team_df
  
}



lineup_ratings  <- function(team1, team1_squad,lineup_stats,formation1,positions1,team1_player_data){
  table_formation <- unlist(data.frame(table(team1_player_data$position_row[team1_player_data$position_row!=0]))[,2])
  
  formation_numbers <- strsplit(formation1,"-")[[1]]
  df1 <- data.frame(formation_numbers, position_number = positions1)
  
  ratings_list <- vector("list",1)
  for(a in 1:length(df1$formation_numbers)){
    position_data <- data.frame(minutes = team1_player_data$minutes[team1_player_data$position_row==df1$position_number[a]],
    player = team1_player_data$page_url_2[team1_player_data$position_row==df1$position_number[a]],
    rating = team1_player_data$avg_rating[team1_player_data$position_row==df1$position_number[a]])
    if(length(which(is.nan(position_data$rating)))==0){
      
    }else{
      position_data <- position_data[-which(is.nan(position_data$rating)),]
    }
    
    position_data <- position_data[which(position_data$minutes>=45),]
    position_data <- position_data[order(-position_data$rating),]
    max_ratings <- sum(position_data$rating[1:df1$formation_numbers[a]])
    l_p <- sum(lineup_stats$avg_rating[lineup_stats$positionRow==df1$position_number[a]&lineup_stats$team==team1])
    ratings_list[[a]] <- rep(l_p/max_ratings,df1$formation_numbers[a])
    print(ratings_list)
  }
  
  lineup_ratings <- mean(unlist(ratings_list))
}

get_game_data_by_team <- function(league_lines, fixtures){
  
  stat_game <- league_lines[[1]][[1]]
  pass_line_home <- league_lines[[1]][[2]]
  tkl_line_home <- league_lines[[1]][[3]]
  shots_line_home <- league_lines[[1]][[4]]
  sog_line_home <- league_lines[[1]][[5]]
  pass_line_against_home <- league_lines[[1]][[6]]
  tkl_line_against_home <- league_lines[[1]][[7]]
  shots_line_against_home <- league_lines[[1]][[8]]
  sog_line_against_home <- league_lines[[1]][[9]]
  pass_line_away <- league_lines[[1]][[10]]
  tkl_line_away <- league_lines[[1]][[11]]
  shots_line_away <- league_lines[[1]][[12]]
  sog_line_away <- league_lines[[1]][[13]]
  pass_line_against_away <- league_lines[[1]][[14]]
  tkl_line_against_away <- league_lines[[1]][[15]]
  shots_line_against_away <- league_lines[[1]][[16]]
  sog_line_against_away <- league_lines[[1]][[17]]
  xg_line_home<- league_lines[[1]][[18]]
  xg_line_away<- league_lines[[1]][[19]]
  xgot_line_home<- league_lines[[1]][[20]]
  xgot_line_away<- league_lines[[1]][[21]]
  xa_line_home<- league_lines[[1]][[22]]
  xa_line_away<- league_lines[[1]][[23]]
  xg_against_line_home<- league_lines[[1]][[24]]
  xg_against_line_away<- league_lines[[1]][[25]]
  xgot_against_line_home<- league_lines[[1]][[26]]
  xgot_against_line_away<- league_lines[[1]][[27]]
  xa_against_line_home<- league_lines[[1]][[28]]
  xa_against_line_away<- league_lines[[1]][[29]]
  pass_line <- league_lines[[1]][[30]]
  tkl_line<- league_lines[[1]][[31]]
  shots_line<- league_lines[[1]][[32]]
  sog_line<- league_lines[[1]][[33]]
  xg_line<- league_lines[[1]][[34]]
  xa_line<- league_lines[[1]][[35]]
  xgot_line<- league_lines[[1]][[36]]
  
  
  fixtures$team1 <- fixtures$home$name
  fixtures$team2 <- fixtures$away$name
  
  fixtures$elo_dif_1 <- 0
  fixtures$elo_dif_2 <- 0
  fixtures$quality <- 0
  fixtures_stats <- vector("list",1)
  for(g in 1:length(fixtures$round)){
    
    
    
    
    fixtures$elo_dif_1[g] <- as.numeric(fixtures$spi1[g]-fixtures$spi2[g])
    fixtures$elo_dif_2[g] <- as.numeric(fixtures$spi2[g]-fixtures$spi1[g])
    fixtures$quality[g] <- as.numeric(fixtures$spi2[g]+fixtures$spi1[g])
    
    elo_dif_1 <- as.numeric(fixtures$spi1[g]-fixtures$spi2[g])
    elo_dif_2 <- as.numeric(fixtures$spi2[g]-fixtures$spi1[g])
    quality <- as.numeric(fixtures$spi2[g]+fixtures$spi1[g])
    
    
    #Passes Overall 
    
    o_passes <- (pass_line$coefficients[[2]]*quality)+pass_line$coefficients[[1]]
    
    #Passes 
    team1_passes <- (pass_line_home$coefficients[[2]]*elo_dif_1)+pass_line_home$coefficients[[1]]
    team2_passes_against <- (pass_line_against_away$coefficients[[2]]*(elo_dif_2))+pass_line_against_away$coefficients[[1]]
    
    team1_passes <- mean(c(team1_passes,team2_passes_against), na.rm = T)
    
    team2_passes <-(pass_line_away$coefficients[[2]]*elo_dif_2)+pass_line_away$coefficients[[1]]
    team1_passes_against <- (pass_line_against_home$coefficients[[2]]*(elo_dif_1))+pass_line_against_home$coefficients[[1]]
    
    team2_passes <- mean(c(team2_passes,team1_passes_against), na.rm = T)
    
    passes_1 <- (team1_passes/sum(team1_passes,team2_passes))*o_passes
    passes_2 <- (team2_passes/sum(team1_passes,team2_passes))*o_passes
    
    #Tackles
    
    team1_tackles <- (tkl_line_home$coefficients[[2]]*(elo_dif_1))+tkl_line_home$coefficients[[1]]
    team2_tackles_against <- (tkl_line_against_away$coefficients[[2]]*(elo_dif_2))+tkl_line_against_away$coefficients[[1]]
    
    team1_tackles <- mean(c(team1_tackles,team2_tackles_against), na.rm = T)
    
    team2_tackles <- (tkl_line_away$coefficients[[2]]*(elo_dif_2))+tkl_line_away$coefficients[[1]]
    team1_tackles_against <- (tkl_line_against_home$coefficients[[2]]*(elo_dif_1))+tkl_line_against_home$coefficients[[1]]
    
    team2_tackles <- mean(c(team2_tackles,team1_tackles_against), na.rm = T)

    
    
    team1_shots <- (shots_line_home$coefficients[[2]]*elo_dif_1)+shots_line_home$coefficients[[1]]
    team2_shots_against <- (shots_line_against_away$coefficients[[2]]*(elo_dif_2))+shots_line_against_away$coefficients[[1]]
    
    team1_shots <- mean(c(team1_shots,team2_shots_against), na.rm = T)
    
    team2_shots <-(shots_line_away$coefficients[[2]]*elo_dif_2)+shots_line_away$coefficients[[1]]
    team1_shots_against <- (shots_line_against_home$coefficients[[2]]*(elo_dif_1))+shots_line_against_home$coefficients[[1]]
    
    team2_shots <- mean(c(team2_shots,team1_shots_against), na.rm = T)
    
    
    
    
    
    
    team1_sogs <- (sog_line_home$coefficients_home[[2]]*elo_dif_1)+sog_line_home$coefficients_home[[1]]
    team2_sogs_against <- (sog_line_against_away$coefficients[[2]]*(elo_dif_2))+sog_line_against_away$coefficients[[1]]
    
    team1_sogs <- mean(c(team1_sogs,team2_sogs_against), na.rm = T)
    
   
    team2_sogs <-(sog_line_away$coefficients[[2]]*elo_dif_2)+sog_line_away$coefficients[[1]]
    team1_sogs_against <- (sog_line_against_home$coefficients[[2]]*(elo_dif_1))+sog_line_against_home$coefficients[[1]]
    
    team2_sogs <- mean(c(team2_sogs,team1_sogs_against), na.rm = T)
    
    
    
    #Tackles
    
    team1_xg <- (xg_line_home$coefficients[[2]]*(elo_dif_1))+xg_line_home$coefficients[[1]]
    team2_xg_against <- (xg_against_line_away$coefficients[[2]]*(elo_dif_2))+xg_against_line_away$coefficients[[1]]
    
    team1_xg <- mean(c(team1_xg,team2_xg_against), na.rm = T)
    
    
    team2_xg <- (xg_line_away$coefficients[[2]]*(elo_dif_2))+xg_line_away$coefficients[[1]]
    team1_xg_against <- (xg_against_line_home$coefficients[[2]]*(elo_dif_1))+xg_against_line_home$coefficients[[1]]
    
    team2_xg <- mean(c(team2_xg,team1_xg_against), na.rm = T)
    
   
    
    
    team1_xgot <- (xgot_line_home$coefficients[[2]]*(elo_dif_1))+xgot_line_home$coefficients[[1]]
    team2_xgot_against <- (xgot_against_line_away$coefficients[[2]]*(elo_dif_2))+xgot_against_line_away$coefficients[[1]]
    
    team1_xgot <- mean(c(team1_xgot,team2_xgot_against), na.rm = T)
    
    
    team2_xgot <- (xgot_line_away$coefficients[[2]]*(elo_dif_2))+xgot_line_away$coefficients[[1]]
    team1_xgot_against <- (xgot_against_line_home$coefficients[[2]]*(elo_dif_1))+xgot_against_line_home$coefficients[[1]]
    
    team2_xgot <- mean(c(team2_xgot,team1_xgot_against), na.rm = T)
    
    
    
    team1_xa <- (xa_line_home$coefficients[[2]]*(elo_dif_1))+xa_line_home$coefficients[[1]]
    team2_xa_against <- (xa_against_line_away$coefficients[[2]]*(elo_dif_2))+xa_against_line_away$coefficients[[1]]
    
    team1_xa <- mean(c(team1_xa,team2_xa_against), na.rm = T)
    
    team2_xa <- (xa_line_away$coefficients[[2]]*(elo_dif_2))+xa_line_away$coefficients[[1]]
    team1_xa_against <- (xa_against_line_home$coefficients[[2]]*(elo_dif_1))+xa_against_line_home$coefficients[[1]]
    
    team2_xa <- mean(c(team2_xa,team1_xa_against), na.rm = T)
    
    fixtures_stats[[g]] <- data.frame(passes_1,
    passes_2,
    team1_tackles,
    team2_tackles,
    team1_shots,
    team2_shots,
    team1_sogs,
    team2_sogs,
    team1_xg,
    team2_xg,
    team1_xgot,
    team2_xgot,
    team1_xa,
    team2_xa)
    
  }
  
  
  list(fixtures_stats,fixtures)
  
}

#fixtures <- fixtures_2

get_game_player_data <- function(lineup_stats, formation_lines, fixtures, game_data, team1, team2){
  
  
  
  tackles_line_1_home <- formation_lines[[1]]
  tackles_line_2_home<- formation_lines[[2]]
  tackles_line_3_home<- formation_lines[[3]]
  tackles_line_4_home<- formation_lines[[4]]
  tackles_line_5_home<- formation_lines[[5]]
  passes_line_1_home<- formation_lines[[6]]
  passes_line_2_home<- formation_lines[[7]]
  passes_line_3_home<- formation_lines[[8]]
  passes_line_4_home<- formation_lines[[9]]
  passes_line_5_home<- formation_lines[[10]]
  shots_line_1_home<- formation_lines[[11]]
  shots_line_2_home<- formation_lines[[12]]
  shots_line_3_home<- formation_lines[[13]]
  shots_line_4_home<- formation_lines[[14]]
  shots_line_5_home<- formation_lines[[15]]
  shotsongoal_line_1_home<- formation_lines[[16]]
  shotsongoal_line_2_home<- formation_lines[[17]]
  shotsongoal_line_3_home<- formation_lines[[18]]
  shotsongoal_line_4_home<- formation_lines[[19]]
  shotsongoal_line_5_home<- formation_lines[[20]]
  tackles_line_1_away<- formation_lines[[21]]
  tackles_line_2_away<- formation_lines[[22]]
  tackles_line_3_away<- formation_lines[[23]]
  tackles_line_4_away<- formation_lines[[24]]
  tackles_line_5_away<- formation_lines[[25]]
  passes_line_1_away<- formation_lines[[26]]
  passes_line_2_away<- formation_lines[[27]]
  passes_line_3_away<- formation_lines[[28]]
  passes_line_4_away<- formation_lines[[29]]
  passes_line_5_away<- formation_lines[[30]]
  shots_line_1_away<- formation_lines[[31]]
  shots_line_2_away<- formation_lines[[32]]
  shots_line_3_away<- formation_lines[[33]]
  shots_line_4_away<- formation_lines[[34]]
  shots_line_5_away<- formation_lines[[35]]
  shotsongoal_line_1_away<- formation_lines[[36]]
  shotsongoal_line_2_away<- formation_lines[[37]]
  shotsongoal_line_3_away<- formation_lines[[38]]
  shotsongoal_line_4_away<- formation_lines[[39]]
  shotsongoal_line_5_away<- formation_lines[[40]]
  xg_line_1_home<- formation_lines[[41]]
  xg_line_2_home<- formation_lines[[42]]
  xg_line_3_home<- formation_lines[[43]]
  xg_line_4_home<- formation_lines[[44]]
  xg_line_5_home<- formation_lines[[45]]
  xgot_line_1_home<- formation_lines[[46]]
  xgot_line_2_home<- formation_lines[[47]]
  xgot_line_3_home<- formation_lines[[48]]
  xgot_line_4_home<- formation_lines[[49]]
  xgot_line_5_home<- formation_lines[[50]]
  xa_line_1_home<- formation_lines[[51]]
  xa_line_2_home<- formation_lines[[52]]
  xa_line_3_home<- formation_lines[[53]]
  xa_line_4_home<- formation_lines[[54]]
  xa_line_5_home<- formation_lines[[55]]
  xg_line_1_away<- formation_lines[[56]]
  xg_line_2_away<- formation_lines[[57]]
  xg_line_3_away<- formation_lines[[58]]
  xg_line_4_away<- formation_lines[[59]]
  xg_line_5_away<- formation_lines[[60]]
  xgot_line_1_away<- formation_lines[[61]]
  xgot_line_2_away<- formation_lines[[62]]
  xgot_line_3_away<- formation_lines[[63]]
  xgot_line_4_away<- formation_lines[[64]]
  xgot_line_5_away<- formation_lines[[65]]
  xa_line_1_away<- formation_lines[[66]]
  xa_line_2_away<- formation_lines[[67]]
  xa_line_3_away<- formation_lines[[68]]
  xa_line_4_away<- formation_lines[[69]]
  xa_line_5_away<- formation_lines[[70]]
  
  lineup_stats$player <- lineup_stats$pageUrl
  
  lineup_stats$player_name <- lineup_stats$pageUrl2
  
  lineup_stats$shxpos <- 0
  lineup_stats$sogxpos<- 0
  lineup_stats$passxpos<- 0
  lineup_stats$tklxpos<- 0
  lineup_stats$xgxpos<- 0
  lineup_stats$xgotxpos<- 0
  lineup_stats$xaxpos<- 0
  lineup_stats$shot_range_mid <- 0
  lineup_stats$sog_range_mid <- 0
  lineup_stats$pass_range_mid <- 0
  lineup_stats$tackle_range_mid <- 0
  lineup_stats$xg_range_mid <- 0
  lineup_stats$xgot_range_mid <- 0
  lineup_stats$xa_range_mid <- 0
  fixtures$elo_dif_1 <- as.numeric(fixtures$spi1-fixtures$spi2)
  fixtures$elo_dif_2 <- as.numeric(fixtures$spi2-fixtures$spi1)
  for(h in 1:length(lineup_stats$player)){
    
    lineup_stats$shot_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$team1_shots,game_data$team2_shots)
    lineup_stats$sog_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$team1_sogs,game_data$team2_sogs)
    lineup_stats$pass_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$passes_1,game_data$passes_2)
    lineup_stats$tackle_range_mid[h] <-ifelse(lineup_stats$team[h]==team1, game_data$team1_tackles,game_data$team2_tackles)
    lineup_stats$xg_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$team1_xg,game_data$team2_xg)
    lineup_stats$xgot_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$team1_xgot,game_data$team2_xgot)
    lineup_stats$xa_range_mid[h] <- ifelse(lineup_stats$team[h]==team1, game_data$team1_xa,game_data$team2_xa)
    
    
    
    if(lineup_stats$positionRow[h]==0){
      
      next()
      
    }else{
      
      
      if(lineup_stats$positionRow[h]==1){
        
        if(any(grepl(lineup_stats$team[h],fixtures$team1))){
          
          
          lineup_stats$shxpos[h] <- mean(c(shots_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shots_line_1_home$coefficients[[1]]))
         
          
          lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shotsongoal_line_1_home$coefficients[[1]]))
          
          
          
          lineup_stats$passxpos[h] <- mean(c(passes_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+passes_line_1_home$coefficients[[1]]))
          
          
          
          lineup_stats$tklxpos[h] <- mean(c(tackles_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+tackles_line_1_home$coefficients[[1]]))
         
          lineup_stats$xgxpos[h] <- mean(c(xg_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_1_home$coefficients[[1]]))
          
         
          
          lineup_stats$xgotxpos[h] <- mean(c(xgot_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_1_home$coefficients[[1]]))
          
          
          
          lineup_stats$xaxpos[h] <- mean(c(xa_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_1_home$coefficients[[1]]))
          
          
          
        }else{
          
          lineup_stats$shxpos[h] <- mean(c(shots_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shots_line_1_away$coefficients[[1]]))
          
          lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shotsongoal_line_1_away$coefficients[[1]]))
          
          
          lineup_stats$passxpos[h] <- mean(c(passes_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+passes_line_1_away$coefficients[[1]]))
          
          
          lineup_stats$tklxpos[h] <- mean(c(tackles_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+tackles_line_1_away$coefficients[[1]]))
          
          lineup_stats$xgxpos[h] <- mean(c(xg_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xg_line_1_away$coefficients[[1]]))
          
          lineup_stats$xgotxpos[h] <- mean(c(xgot_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xgot_line_1_away$coefficients[[1]]))
          
          lineup_stats$xaxpos[h] <- mean(c(xa_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xa_line_1_away$coefficients[[1]]))
          
          
          
          
        }
        
        
      }else{
        
        
        if(lineup_stats$positionRow[h]==2){
          
          if(any(grepl(lineup_stats$team[h],fixtures$team1))){
            
            lineup_stats$shxpos[h] <- mean(c(shots_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shots_line_2_home$coefficients[[1]]))
            
            lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shotsongoal_line_2_home$coefficients[[1]]))
            
            
           lineup_stats$passxpos[h] <- mean(c(passes_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+passes_line_2_home$coefficients[[1]]))
            
            
            lineup_stats$tklxpos[h] <- mean(c(tackles_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+tackles_line_2_home$coefficients[[1]]))
            
            lineup_stats$xgxpos[h] <- mean(c(xg_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_2_home$coefficients[[1]]))
            
            lineup_stats$xgotxpos[h] <- mean(c(xgot_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_2_home$coefficients[[1]]))
            
            lineup_stats$xaxpos[h] <- mean(c(xa_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_2_home$coefficients[[1]]))
            
            
            
          }else{
            
            lineup_stats$shxpos[h] <- mean(c(shots_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shots_line_2_away$coefficients[[1]]))
            
            lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shotsongoal_line_2_away$coefficients[[1]]))
            
            
            lineup_stats$passxpos[h] <- mean(c(passes_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+passes_line_2_away$coefficients[[1]]))
            
            
           lineup_stats$tklxpos[h] <- mean(c(tackles_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+tackles_line_2_away$coefficients[[1]]))
           lineup_stats$xgxpos[h] <- mean(c(xg_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xg_line_2_away$coefficients[[1]]))
            
           lineup_stats$xgotxpos[h] <- mean(c(xgot_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xgot_line_2_away$coefficients[[1]]))
             lineup_stats$xaxpos[h] <- mean(c(xa_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xa_line_2_away$coefficients[[1]]))
            
            
            
            
          }
          
          
        }else{
          
          
          if(lineup_stats$positionRow[h]==3){
            
            if(any(grepl(lineup_stats$team[h],fixtures$team1))){
              
               
              lineup_stats$shxpos[h] <- mean(c(shots_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shots_line_3_home$coefficients[[1]]))
             
              lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shotsongoal_line_3_home$coefficients[[1]]))
              
             lineup_stats$passxpos[h] <- mean(c(passes_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+passes_line_3_home$coefficients[[1]]))
              
             
              lineup_stats$tklxpos[h] <- mean(c(tackles_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+tackles_line_3_home$coefficients[[1]]))
             
              
              lineup_stats$xgxpos[h] <- mean(c(xg_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_3_home$coefficients[[1]]))
              
              
              lineup_stats$xgotxpos[h] <- mean(c(xgot_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_3_home$coefficients[[1]]))
              
             
              lineup_stats$xaxpos[h] <- mean(c(xa_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_3_home$coefficients[[1]]))
              
              
              
            }else{
              
              
              
              lineup_stats$shxpos[h] <- mean(c(shots_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shots_line_3_away$coefficients[[1]]))
              
              
              
              lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shotsongoal_line_3_away$coefficients[[1]]))
              
              
              
              
              lineup_stats$passxpos[h] <- mean(c(passes_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+passes_line_3_away$coefficients[[1]]))
              
             
              
              lineup_stats$tklxpos[h] <- mean(c(tackles_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+tackles_line_3_away$coefficients[[1]]))
              
              
             
              
              lineup_stats$xgxpos[h] <- mean(c(xg_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xg_line_3_away$coefficients[[1]]))
              
              
              
              lineup_stats$xgotxpos[h] <- mean(c(xgot_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xgot_line_3_away$coefficients[[1]]))
             
              
              lineup_stats$xaxpos[h] <- mean(c(xa_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xa_line_3_away$coefficients[[1]]))
              
              
              
            }
            
            
          }else{
            
            
            if(lineup_stats$positionRow[h]==4){
              
              if(any(grepl(lineup_stats$team[h],fixtures$team1))){
                
               
                
                lineup_stats$shxpos[h] <- mean(c(shots_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shots_line_4_home$coefficients[[1]]))
                
               
                
                lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shotsongoal_line_4_home$coefficients[[1]]))
                
                
               
                
                lineup_stats$passxpos[h] <- mean(c(passes_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+passes_line_4_home$coefficients[[1]]))
                
                
               
                
                lineup_stats$tklxpos[h] <- mean(c(tackles_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+tackles_line_4_home$coefficients[[1]]))
                
                
                if(is.na(xg_line_4_home)){
                  lineup_stats$xgxpos[h] <- mean(c(xg_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_5_home$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xgotxpos[h] <- mean(c(xgot_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_5_home$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xaxpos[h] <- mean(c(xa_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_5_home$coefficients[[1]]))
                  
                }else{
                  lineup_stats$xgxpos[h] <- mean(c(xg_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_4_home$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xgotxpos[h] <- mean(c(xgot_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_4_home$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xaxpos[h] <- mean(c(xa_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_4_home$coefficients[[1]]))
                  
                }
                
                
                
              }else{
                
               
                
                lineup_stats$shxpos[h] <- mean(c(shots_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shots_line_4_away$coefficients[[1]]))
               
                
                lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shotsongoal_line_4_away$coefficients[[1]]))
                
               
                
                lineup_stats$passxpos[h] <- mean(c(passes_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+passes_line_4_away$coefficients[[1]]))
                
               
                
                lineup_stats$tklxpos[h] <- mean(c(tackles_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+tackles_line_4_away$coefficients[[1]]))
                
                
               
                
                lineup_stats$xgxpos[h] <- mean(c(xg_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xg_line_4_away$coefficients[[1]]))
               
                
                lineup_stats$xgotxpos[h] <- mean(c(xgot_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xgot_line_4_away$coefficients[[1]]))
               
                
                lineup_stats$xaxpos[h] <- mean(c(xa_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xa_line_4_away$coefficients[[1]]))
                
                
                
              }
              
              
              
              
              
              
            }else{
              
              
              if(lineup_stats$positionRow[h]==5){
                
                if(any(grepl(lineup_stats$team[h],fixtures$team1))){
                  
                  
                  lineup_stats$shxpos[h] <- mean(c(shots_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shots_line_5_home$coefficients[[1]]))
                  
                 
                  lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+shotsongoal_line_5_home$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$passxpos[h] <- mean(c(passes_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+passes_line_5_home$coefficients[[1]]))
                  
                 
                  
                  lineup_stats$tklxpos[h] <- mean(c(tackles_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+tackles_line_5_home$coefficients[[1]]))
                 
                  
                  lineup_stats$xgxpos[h] <- mean(c(xg_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xg_line_5_home$coefficients[[1]]))
                 
                  
                  lineup_stats$xgotxpos[h] <- mean(c(xgot_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xgot_line_5_home$coefficients[[1]]))
                  
                 
                  
                  lineup_stats$xaxpos[h] <- mean(c(xa_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==lineup_stats$team[h])]+xa_line_5_home$coefficients[[1]]))
                  
                  
                  
                }else{
                  
                  
                  
                  lineup_stats$shxpos[h] <- mean(c(shots_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shots_line_5_away$coefficients[[1]]))
                 
                  
                  lineup_stats$sogxpos[h] <- mean(c(shotsongoal_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+shotsongoal_line_5_away$coefficients[[1]]))
                  
                 
                  
                  lineup_stats$passxpos[h] <- mean(c(passes_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+passes_line_5_away$coefficients[[1]]))
                
                  lineup_stats$tklxpos[h] <- mean(c(tackles_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+tackles_line_5_away$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xgxpos[h] <- mean(c(xg_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xg_line_5_away$coefficients[[1]]))
                  
                 
                  
                  lineup_stats$xgotxpos[h] <- mean(c(xgot_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xgot_line_5_away$coefficients[[1]]))
                  
                  
                  
                  lineup_stats$xaxpos[h] <- mean(c(xa_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==lineup_stats$team[h])]+xa_line_5_away$coefficients[[1]]))
                  
                  
                  
                  
                }
                
                
              }else{
                
                
                
                
                
              } 
              
              
            } 
            
            
          } 
          
          
        } 
        
        
      }
      
      
    }
    
    
    
    
  }
  
  lineup_stats$passxpos[lineup_stats$positionRow==0] <- ifelse(lineup_stats$minutes[lineup_stats$positionRow==0]>=180,lineup_stats$pass_90min[lineup_stats$positionRow==0],
                                                             15)
  lineup_stats$shxpos[lineup_stats$shxpos<0] <- 0.0001
  lineup_stats$sogxpos[lineup_stats$sogxpos<0]<- 0.0001
  lineup_stats$passxpos[lineup_stats$passxpos<0]<- 0.0001
  lineup_stats$tklxpos[lineup_stats$tklxpos<0]<- 0.0001
  lineup_stats$xgxpos[lineup_stats$xgxpos<0]<- 0.0001
  lineup_stats$xgotxpos[lineup_stats$xgotxpos<0]<- 0.0001
  lineup_stats$xaxpos[lineup_stats$xaxpos<0]<- 0.0001
  
  lineup_stats$passxpos2[lineup_stats$team==team1] <- lineup_stats$passxpos[lineup_stats$team==team1]*unique(lineup_stats$pass_range_mid[lineup_stats$team==team1])/sum(lineup_stats$passxpos[lineup_stats$team==team1])
                                                            
  lineup_stats$tklxpos2[lineup_stats$team==team1] <- lineup_stats$tklxpos[lineup_stats$team==team1]*unique(lineup_stats$tackle_range_mid[lineup_stats$team==team1])/sum(lineup_stats$tklxpos[lineup_stats$team==team1])
                                                          
  lineup_stats$shxpos2[lineup_stats$team==team1] <- lineup_stats$shxpos[lineup_stats$team==team1]*unique(lineup_stats$shot_range_mid[lineup_stats$team==team1])/sum(lineup_stats$shxpos[lineup_stats$team==team1])
                                                      
  lineup_stats$sogxpos2[lineup_stats$team==team1] <- lineup_stats$sogxpos[lineup_stats$team==team1]*unique(lineup_stats$sog_range_mid[lineup_stats$team==team1])/sum(lineup_stats$sogxpos[lineup_stats$team==team1])
                                                          
  lineup_stats$xgxpos2[lineup_stats$team==team1] <- lineup_stats$xgxpos[lineup_stats$team==team1]*unique(lineup_stats$xg_range_mid[lineup_stats$team==team1])/sum(lineup_stats$xgxpos[lineup_stats$team==team1])
                                                       
  lineup_stats$xgotxpos2[lineup_stats$team==team1] <- lineup_stats$xgotxpos[lineup_stats$team==team1]*unique(lineup_stats$xgot_range_mid[lineup_stats$team==team1])/sum(lineup_stats$xgotxpos[lineup_stats$team==team1])
                                                            
  lineup_stats$xaxpos2[lineup_stats$team==team1] <- lineup_stats$xaxpos[lineup_stats$team==team1]*unique(lineup_stats$xa_range_mid[lineup_stats$team==team1])/sum(lineup_stats$xaxpos[lineup_stats$team==team1])
                                                        
  
  lineup_stats$passxpos2[lineup_stats$team==team2] <- lineup_stats$passxpos[lineup_stats$team==team2]*unique(lineup_stats$pass_range_mid[lineup_stats$team==team2])/sum(lineup_stats$passxpos[lineup_stats$team==team2])
                                                            
  lineup_stats$tklxpos2[lineup_stats$team==team2] <- lineup_stats$tklxpos[lineup_stats$team==team2]*unique(lineup_stats$tackle_range_mid[lineup_stats$team==team2])/sum(lineup_stats$tklxpos[lineup_stats$team==team2])
                                                           
  lineup_stats$shxpos2[lineup_stats$team==team2] <- lineup_stats$shxpos[lineup_stats$team==team2]*unique(lineup_stats$shot_range_mid[lineup_stats$team==team2])/sum(lineup_stats$shxpos[lineup_stats$team==team2])
                                                          
  lineup_stats$sogxpos2[lineup_stats$team==team2] <- lineup_stats$sogxpos[lineup_stats$team==team2]*unique(lineup_stats$sog_range_mid[lineup_stats$team==team2])/sum(lineup_stats$sogxpos[lineup_stats$team==team2])
                                                        
  lineup_stats$xgxpos2[lineup_stats$team==team2] <- lineup_stats$xgxpos[lineup_stats$team==team2]*unique(lineup_stats$xg_range_mid[lineup_stats$team==team2])/sum(lineup_stats$xgxpos[lineup_stats$team==team2])
                                                         
  lineup_stats$xgotxpos2[lineup_stats$team==team2] <- lineup_stats$xgotxpos[lineup_stats$team==team2]*unique(lineup_stats$xgot_range_mid[lineup_stats$team==team2])/sum(lineup_stats$xgotxpos[lineup_stats$team==team2])
                                                          
  lineup_stats$xaxpos2[lineup_stats$team==team2] <- lineup_stats$xaxpos[lineup_stats$team==team2]*unique(lineup_stats$xa_range_mid[lineup_stats$team==team2])/sum(lineup_stats$xaxpos[lineup_stats$team==team2])
              
  
    
  lineup_stats$passxpos[lineup_stats$team==team1] <- (lineup_stats$passxpos[lineup_stats$team==team1]+lineup_stats$passxpos2[lineup_stats$team==team1])/2
  lineup_stats$tklxpos[lineup_stats$team==team1] <- (lineup_stats$tklxpos[lineup_stats$team==team1]+lineup_stats$tklxpos2[lineup_stats$team==team1])/2
  lineup_stats$shxpos[lineup_stats$team==team1] <- (lineup_stats$shxpos[lineup_stats$team==team1]+lineup_stats$shxpos2[lineup_stats$team==team1])/2
  lineup_stats$sogxpos[lineup_stats$team==team1] <- (lineup_stats$sogxpos[lineup_stats$team==team1]+lineup_stats$sogxpos2[lineup_stats$team==team1])/2
  lineup_stats$xgxpos[lineup_stats$team==team1] <- (lineup_stats$xgxpos[lineup_stats$team==team1]+lineup_stats$xgxpos2[lineup_stats$team==team1])/2
  lineup_stats$xgotxpos[lineup_stats$team==team1] <- (lineup_stats$xgotxpos[lineup_stats$team==team1]+lineup_stats$xgotxpos2[lineup_stats$team==team1])/2
  lineup_stats$xaxpos[lineup_stats$team==team1] <- (lineup_stats$xaxpos[lineup_stats$team==team1]+lineup_stats$xaxpos2[lineup_stats$team==team1])/2
  
  lineup_stats$passxpos[lineup_stats$team==team2] <- (lineup_stats$passxpos[lineup_stats$team==team2]+lineup_stats$passxpos2[lineup_stats$team==team2])/2
  lineup_stats$tklxpos[lineup_stats$team==team2] <- (lineup_stats$tklxpos[lineup_stats$team==team2]+lineup_stats$tklxpos2[lineup_stats$team==team2])/2
  lineup_stats$shxpos[lineup_stats$team==team2] <- (lineup_stats$shxpos[lineup_stats$team==team2]+lineup_stats$shxpos2[lineup_stats$team==team2])/2
  lineup_stats$sogxpos[lineup_stats$team==team2] <- (lineup_stats$sogxpos[lineup_stats$team==team2]+lineup_stats$sogxpos2[lineup_stats$team==team2])/2
  lineup_stats$xgxpos[lineup_stats$team==team2] <- (lineup_stats$xgxpos[lineup_stats$team==team2]+lineup_stats$xgxpos2[lineup_stats$team==team2])/2
  lineup_stats$xgotxpos[lineup_stats$team==team2] <- (lineup_stats$xgotxpos[lineup_stats$team==team2]+lineup_stats$xgotxpos2[lineup_stats$team==team2])/2
  lineup_stats$xaxpos[lineup_stats$team==team2] <- (lineup_stats$xaxpos[lineup_stats$team==team2]+lineup_stats$xaxpos2[lineup_stats$team==team2])/2
  
  
  
  lineup_stats$expected_tackles <- 0
  lineup_stats$expected_passes <- 0
  lineup_stats$expected_shots <- 0
  lineup_stats$expected_sogs <- 0
  lineup_stats$expected_xg <- 0
  lineup_stats$expected_xgot <- 0
  lineup_stats$expected_xa <- 0
  
  lineup_stats$pweight <- 0
  lineup_stats$tweight <- 0
  lineup_stats$sweight <- 0
  lineup_stats$sogweight <- 0
  lineup_stats$xgweight <- 0
  lineup_stats$xgotweight <- 0
  lineup_stats$xaweight <- 0
  
  lineup_stats$player_pass_share <- 0
  lineup_stats$player_tkl_share <- 0
  lineup_stats$player_shot_share <- 0
  lineup_stats$player_sog_share <- 0
  lineup_stats$player_xg_share <- 0
  lineup_stats$player_xgot_share <- 0
  lineup_stats$player_xa_share <- 0
  
  for(k in 1:length(lineup_stats$player)){
    
    if(lineup_stats$positionRow[k]==0){
      
      next()
      
    }else{
      
      lineup_stats$pweight[k] <- lineup_stats$minutes[k]/sum(lineup_stats$minutes[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$tweight[k] <- lineup_stats$minutes[k]/sum(lineup_stats$minutes[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$sweight[k] <- lineup_stats$minutes[k]/sum(lineup_stats$minutes[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$sogweight[k] <- lineup_stats$minutes[k]/sum(lineup_stats$minutes[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xgweight[k] <- lineup_stats$minutes_xg[k]/sum(lineup_stats$minutes_xg[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xgotweight[k] <- lineup_stats$minutes_xg[k]/sum(lineup_stats$minutes_xg[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xaweight[k] <- lineup_stats$minutes_xg[k]/sum(lineup_stats$minutes_xg[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      
      
      lineup_stats$player_pass_share[k] <- lineup_stats$pass_90min[k]/sum(lineup_stats$pass_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_tkl_share[k] <- lineup_stats$tkl_90min[k]/sum(lineup_stats$tkl_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_shot_share[k] <- lineup_stats$shots_90min[k]/sum(lineup_stats$shots_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_sog_share[k] <- lineup_stats$sog_90min[k]/sum(lineup_stats$sog_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xg_share[k] <- lineup_stats$xg_90min[k]/sum(lineup_stats$xg_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xgot_share[k] <- lineup_stats$xgot_90min[k]/sum(lineup_stats$xgot_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xa_share[k] <- lineup_stats$xa_90min[k]/sum(lineup_stats$xa_90min[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      
    }
    
    
  }
  
  for(l in 1:length(lineup_stats$player)){
    
    if(lineup_stats$positionRow[l]==0){
      
      next()
      
    }else{
      
      lineup_stats$pweight[l] <- ifelse(is.nan(lineup_stats$pweight[l]),1,
                                       ifelse(lineup_stats$pweight[l]==0,min(lineup_stats$pweight[lineup_stats$pweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$pweight[l]))
      lineup_stats$tweight[l] <- ifelse(is.nan(lineup_stats$tweight[l]),1,
                                       ifelse(lineup_stats$tweight[l]==0,min(lineup_stats$tweight[lineup_stats$tweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$tweight[l]))
      lineup_stats$sweight[l] <- ifelse(is.nan(lineup_stats$sweight[l]),1,
                                       ifelse(lineup_stats$sweight[l]==0,min(lineup_stats$sweight[lineup_stats$sweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$sweight[l]))
      lineup_stats$sogweight[l] <- ifelse(is.nan(lineup_stats$sogweight[l]),1,
                                         ifelse(lineup_stats$sogweight[l]==0,min(lineup_stats$sogweight[lineup_stats$sogweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$sogweight[l]))
      
      lineup_stats$xgweight[l] <- ifelse(is.nan(lineup_stats$xgweight[l]),1,
                                        ifelse(lineup_stats$xgweight[l]==0,min(lineup_stats$xgweight[lineup_stats$xgweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$xgweight[l]))
      lineup_stats$xgotweight[l] <- ifelse(is.nan(lineup_stats$xgotweight[l]),1,
                                          ifelse(lineup_stats$xgotweight[l]==0,min(lineup_stats$xgotweight[lineup_stats$xgotweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$xgotweight[l]))
      lineup_stats$xaweight[l] <- ifelse(is.nan(lineup_stats$xaweight[l]),1,
                                        ifelse(lineup_stats$xaweight[l]==0,min(lineup_stats$xaweight[lineup_stats$xaweight!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$xaweight[l]))
      
      
      lineup_stats$player_pass_share[l] <- ifelse(is.nan(lineup_stats$player_pass_share[l]),1,
                                                 ifelse(lineup_stats$player_pass_share[l]==0,min(lineup_stats$player_pass_share[lineup_stats$player_pass_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_pass_share[l]))
      lineup_stats$player_tkl_share[l] <- ifelse(is.nan(lineup_stats$player_tkl_share[l]),1,
                                                ifelse(lineup_stats$player_tkl_share[l]==0,min(lineup_stats$player_tkl_share[lineup_stats$player_tkl_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_tkl_share[l]))
      lineup_stats$player_shot_share[l] <- ifelse(is.nan(lineup_stats$player_shot_share[l]),1,
                                                 ifelse(lineup_stats$player_shot_share[l]==0,min(lineup_stats$player_shot_share[lineup_stats$player_shot_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_shot_share[l]))
      lineup_stats$player_sog_share[l] <- ifelse(is.nan(lineup_stats$player_sog_share[l]),1,
                                                ifelse(lineup_stats$player_sog_share[l]==0,min(lineup_stats$player_sog_share[lineup_stats$player_sog_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_sog_share[l]))
      lineup_stats$player_xg_share[l] <- ifelse(is.nan(lineup_stats$player_xg_share[l]),1,
                                               ifelse(lineup_stats$player_xg_share[l]==0,min(lineup_stats$player_xg_share[lineup_stats$player_xg_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_xg_share[l]))
      lineup_stats$player_xgot_share[l] <- ifelse(is.nan(lineup_stats$player_xgot_share[l]),1,
                                                 ifelse(lineup_stats$player_xgot_share[l]==0,min(lineup_stats$player_xgot_share[lineup_stats$player_xgot_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_xgot_share[l]))
      lineup_stats$player_xa_share[l] <- ifelse(is.nan(lineup_stats$player_xa_share[l]),1,
                                               ifelse(lineup_stats$player_xa_share[l]==0,min(lineup_stats$player_xa_share[lineup_stats$player_xa_share!=0&lineup_stats$team==lineup_stats$team[l]&lineup_stats$positionRow==lineup_stats$positionRow[l]]),lineup_stats$player_xa_share[l]))
      
    }
    
    
  }
  
  lineup_stats$pweight_fin <- 0
  lineup_stats$tweight_fin <- 0
  lineup_stats$sweight_fin <- 0
  lineup_stats$sogweight_fin <- 0
  lineup_stats$xgweight_fin <- 0
  lineup_stats$xgotweight_fin <- 0
  lineup_stats$xaweight_fin <- 0
  
  lineup_stats$player_pass_share_fin <- 0
  lineup_stats$player_tkl_share_fin <- 0
  lineup_stats$player_shot_share_fin <- 0
  lineup_stats$player_sog_share_fin <- 0
  lineup_stats$player_xg_share_fin <- 0
  lineup_stats$player_xgot_share_fin <- 0
  lineup_stats$player_xa_share_fin <- 0
  
  for(k in 1:length(lineup_stats$player)){
    
    if(lineup_stats$positionRow[k]==0){
      
      next()
      
    }else{
      
      lineup_stats$pweight_fin[k] <- lineup_stats$pweight[k]/sum(lineup_stats$pweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$tweight_fin[k] <- lineup_stats$tweight[k]/sum(lineup_stats$tweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$sweight_fin[k] <- lineup_stats$sweight[k]/sum(lineup_stats$sweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$sogweight_fin[k] <- lineup_stats$sogweight[k]/sum(lineup_stats$sogweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xgweight_fin[k] <- lineup_stats$xgweight[k]/sum(lineup_stats$xgweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xgotweight_fin[k] <- lineup_stats$xgotweight[k]/sum(lineup_stats$xgotweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$xaweight_fin[k] <- lineup_stats$xaweight[k]/sum(lineup_stats$xaweight[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      
      
      lineup_stats$player_pass_share_fin[k] <- lineup_stats$player_pass_share[k]/sum(lineup_stats$player_pass_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_tkl_share_fin[k] <- lineup_stats$player_tkl_share[k]/sum(lineup_stats$player_tkl_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_shot_share_fin[k] <- lineup_stats$player_shot_share[k]/sum(lineup_stats$player_shot_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_sog_share_fin[k] <- lineup_stats$player_sog_share[k]/sum(lineup_stats$player_sog_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xg_share_fin[k] <- lineup_stats$player_xg_share[k]/sum(lineup_stats$player_xg_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xgot_share_fin[k] <- lineup_stats$player_xgot_share[k]/sum(lineup_stats$player_xgot_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      lineup_stats$player_xa_share_fin[k] <- lineup_stats$player_xa_share[k]/sum(lineup_stats$player_xa_share[lineup_stats$team==lineup_stats$team[k]&lineup_stats$positionRow==lineup_stats$positionRow[k]])
      
    }
    
    
  }
  
  
  for(j in 1:length(lineup_stats$player)){
    
    
    if(lineup_stats$positionRow[j]==0){
      
      tot_passes <- sum(lineup_stats$passxpos[lineup_stats$team==lineup_stats$team[j]])
      
      tot_passes_adj <- tot_passes
      
      lineup_stats$expected_passes[j] <- lineup_stats$passxpos[j]*(tot_passes_adj/tot_passes)
      
    }else{
      
      
      pos_passes <- sum(lineup_stats$passxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      sum_pass_weight <- sum(lineup_stats$pweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      player_passes <- (pos_passes*lineup_stats$player_pass_share_fin[j])
      lineup_stats$expected_passes[j] <- player_passes
      
      
      
      pos_tackles <- sum(lineup_stats$tklxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      sum_tackle_weight <- sum(lineup_stats$tweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      player_tackles <- (pos_tackles*lineup_stats$player_tkl_share_fin[j])
      lineup_stats$expected_tackles[j] <- player_tackles
      
      
      
     
      
      pos_shots <- sum(lineup_stats$shxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      sum_shots_weight <- sum(lineup_stats$sweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      player_shots <- (pos_shots*lineup_stats$player_shot_share_fin[j])
      
      lineup_stats$expected_shots[j] <- player_shots
      
      
      pos_sogs <- sum(lineup_stats$sogxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      sum_sogs_weight <- sum(lineup_stats$sogweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      player_sogs <- (pos_sogs*lineup_stats$player_sog_share_fin[j])
      
      lineup_stats$expected_sogs[j] <- player_sogs
      
     
      
      pos_xg <- sum(lineup_stats$xgxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      sum_xg_weight <- sum(lineup_stats$xgweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      player_xg <- (pos_xg*lineup_stats$player_xg_share_fin[j])
      
      lineup_stats$expected_xg[j] <- player_xg
      
     
      
      pos_xgot <- sum(lineup_stats$xgotxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      sum_xgot_weight <- sum(lineup_stats$xgotweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      player_xgot <- (pos_xgot*lineup_stats$player_xgot_share_fin[j])
      
      lineup_stats$expected_xgot[j] <- player_xgot
     
      
      pos_xa <- sum(lineup_stats$xaxpos[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      sum_xa_weight <- sum(lineup_stats$xaweight_fin[lineup_stats$team==lineup_stats$team[j]&lineup_stats$positionRow==lineup_stats$positionRow[j]])
      
      player_xa <- (pos_xa*lineup_stats$player_xa_share_fin[j])
      
      lineup_stats$expected_xa[j] <- player_xa
      
    }
    
    
  }
  
  lineup_stats <- data.frame(lineup_stats)
  
  player_stats_game <- lineup_stats
  
  
  
  player_stats_game$min_p90min <- player_stats_game$minutes/player_stats_game$games
 
  
  player_stats_game$min_p90min[player_stats_game$positionRow==0] <- 90
  player_stats_game$min_p90min <- ifelse(player_stats_game$min_p90min>90,90,player_stats_game$min_p90min)
  
  
  for(b in 1:length(player_stats_game$pageUrl)){
    
    team_mins <- player_stats_game$min_p90min[player_stats_game$team==player_stats_game$team[b]]
    
    team_max_mins <- team_mins[order(-team_mins)][c(1:7)]
    
    if(is.element(player_stats_game$min_p90min[b],team_max_mins)){
      
      player_stats_game$min_p90min[b] <- 90
      
    }else{
      
      
      
    }
  }
  
  player_stats_game$min_share <- 0
  
  for(b in 1:length(player_stats_game$pageUrl)){
    
    player_stats_game$min_share[b] <- player_stats_game$min_p90min[b]/max(player_stats_game$min_p90min[player_stats_game$team==player_stats_game$team[b]])
    
  }
  
  player_stats_game$exp_min <- 90*player_stats_game$min_share
  
  player_stats_game$expected_passes <- player_stats_game$expected_passes*player_stats_game$exp_min/90
  
  player_stats_game$expected_tackles <- player_stats_game$expected_tackles*player_stats_game$exp_min/90
  
  player_stats_game$expected_shots <- player_stats_game$expected_shots*player_stats_game$exp_min/90
  
  player_stats_game$expected_sogs <- player_stats_game$expected_sogs*player_stats_game$exp_min/90
  
  player_stats_game$expected_xg <- player_stats_game$expected_xg*player_stats_game$exp_min/90
  
  player_stats_game$expected_xa <- player_stats_game$expected_xa*player_stats_game$exp_min/90
  
  player_stats_game
  
}




game_predict <- function(league_lines, fixtures, lines, player_stats_game, league, goalie_ratings, date){
  
  
  h_sd <- league_lines[[1]][[37]]
  a_sd <- league_lines[[1]][[38]]
  
  stat_game <- league_lines[[1]][[1]]
  
  test1_1 <- stat_game$shots[!is.na(stat_game$expected_goals_ot)&stat_game$expected_goals_ot>0]
  test1_2 <- stat_game$shotsongoal[!is.na(stat_game$expected_goals_ot)&stat_game$expected_goals_ot>0]
  test2 <- stat_game$expected_goals_ot[!is.na(stat_game$expected_goals_ot)&stat_game$expected_goals_ot>0]
  
  
  xg_exp <- summary(lm(test2~test1_1+test1_2))
  wpct_1 <- 0
  wpct_X <- 0
  wpct_2 <- 0
  pct_overs <- 0
  pct_unders <- 0
  
  
  
  
  
  
  
  
  
  
  card <- ""
  scheduled_time <- force_tz(as.POSIXct(fixtures$status$utcTime[1], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
  q <- 1
  model_data <- data.frame(matrix(0,length(fixtures$round),56))
  
  colnames(model_data) <- c("confirmed","home","away","date_time","league",
                            "h_wpct_odds","x_wpct_odds","a_wpct_odds","h_wpct_mean","x_wpct_mean","a_wpct_mean",
                            "h_spct_odds","a_spct_odds","h_spct_mean","a_spct_mean",
                            "total_odds","opct_odds","upct_odds","total_mean","opct_mean","upct_mean",
                            "h_score_pass_shot","a_score_pass_shot","o_pct_pass_shot","u_pct_pass_shot","h_wpct_pass_shot","x_wpct_pass_shot","a_wpct_pass_shot",
                            "h_spct_pass_shot","a_spct_pass_shot",
                            "h_score_xg","a_score_xg","o_pct_xg","u_pct_xg","h_wpct_xg","x_wpct_xg","a_wpct_xg",
                            "h_spct_xg","a_spct_xg",
                            "h_score_xgot","a_score_xgot","o_pct_xgot","u_pct_xgot","x_wpct_xgot","a_wpct_xgot",
                            "h_spct_xgot","a_spct_xgot",
                            "h_score_xa","a_score_xa","o_pct_xa","u_pct_xa","h_wpct_xa","x_wpct_xa","a_wpct_xa",
                            "h_spct_xa","a_spct_xa"
  )
  model_data$league <- league
  model_data$date_time <- force_tz(as.POSIXct(fixtures$status$utcTime[1], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
  
  
  home_attg <- vector("list",length(fixtures$round))
  away_attg <- vector("list",length(fixtures$round))
  
  
  for(j in 1:length(fixtures$round)){
    
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling"),"/All Match Data"))
    match_files <- list.files()
    if(fixtures$id[j] %in% match_files){
      
    }else{
      dir.create(paste0(wspc,"Fotmob Soccer Modelling/All Match Data/",fixtures$id[j]))
    }
    setwd(paste0(wspc,"Fotmob Soccer Modelling/All Match Data/",fixtures$id))
    sink(iconv(paste0(gsub("\\.","",paste0(gsub("-","",date)," ",paste0(l$ccode[l$id==id[zz]]," ",l$name[l$id==id[zz]]),"_",team1," v ",team2,"_card")),".txt"),"latin1","ascii",sub = ""), split = TRUE)
    
    print(paste0("Updated at ",Sys.time()))
    print("_____________________________________________________________________")
    
    game_end_check <- lines$BARSTOOL_TEAM1_ML[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    
    if(is.na(game_end_check)){
      next()
    }
    if(fixtures$status$finished[j]==TRUE){
      next()
    }
    model_data$confirmed[j] <- unique(player_stats_game$confirmed[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])
    model_data$home[j] <- fixtures$home$name[j]
    model_data$away[j] <- fixtures$away$name[j]
    model_data$date_time[j] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
    if(model_data$date_time[j]<Sys.time()){
      next()
    }
    print("-----Model Predictions Below-----")
    
    print("--Pass/Shot Model--")
    
    #Pass/Model
    
    h_score <- xg_exp$coefficients[[1]]+
      (xg_exp$coefficients[[2]]*(sum(player_stats_game$expected_shots[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])))+
      (xg_exp$coefficients[[3]]*(sum(player_stats_game$expected_sogs[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])))
    
    a_score <- xg_exp$coefficients[[1]]+
      (xg_exp$coefficients[[2]]*(sum(player_stats_game$expected_shots[player_stats_game$team==fixtures$away$name[j]&player_stats_game$match_id==fixtures$id[j]])))+
      (xg_exp$coefficients[[3]]*(sum(player_stats_game$expected_sogs[player_stats_game$team==fixtures$away$name[j]&player_stats_game$match_id==fixtures$id[j]])))
    
    h_score <- h_score+goalie_ratings[[2]]
    a_score <- a_score+goalie_ratings[[1]]
    
    print(paste0("Score: ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
    print(paste0("Total: ",round(h_score,4)+round(a_score,4)))
    
    
    
    set.seed(123)
    n_sims <- 10000
    team_one_eg <- h_score
    team_two_eg <- a_score
    team_one_wins <- 0
    team_one_draws <- 0
    team_one_losses <- 0
    team_two_wins <- 0
    team_two_draws <- 0
    team_two_losses <- 0
    overs <- 0 
    unders <- 0
    h_gsc_list_1 <- vector("list",1)
    a_gsc_list_1 <- vector("list",1)
    for (i in 1:n_sims) {
      
      h_goalscorers <- "none"
      a_goalscorers <- "none"
      
      team_one_goals <- rpois(1, lambda = h_score)
      team_two_goals <- rpois(1, lambda = a_score)
      
      if (team_one_goals > team_two_goals) {
        team_one_wins <- team_one_wins + 1
        team_two_losses <- team_two_losses + 1
      } else if (team_two_goals > team_one_goals) {
        team_one_losses <- team_one_losses + 1
        team_two_wins <- team_two_wins + 1
      } else {
        team_one_draws <- team_one_draws + 1
        team_two_draws <- team_two_draws + 1
      }
      
      if ((team_one_goals + team_two_goals)>=2.5) {
        overs <- overs + 1
      } else {
        unders <- unders + 1
      }
      
      if(team_one_goals==0){
        
      }else{
        for(u in 1:team_one_goals){
          h_goalscorers[u] <-  sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$home$name[j]],"own goal"),
                                      1,
                                      prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]],
                                               (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]])))) 
        }
      }
      
      
      if(team_two_goals==0){
        
      }else{
        
        for(u in 1:team_two_goals){
          a_goalscorers[u] <- sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$away$name[j]],"own goal"),
                                     1,
                                     prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]],
                                              (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]])))) 
        }
        
      }
      
      
      h_gsc_list_1[[i]] <- unique(h_goalscorers)
      a_gsc_list_1[[i]] <- unique(a_goalscorers)
      
    }
    team_one_win_pct <- team_one_wins / n_sims
    team_one_draw_pct <- team_one_draws / n_sims
    team_two_win_pct <- team_two_wins / n_sims
    team_two_draw_pct <- team_two_draws / n_sims
    over_pct <- overs / n_sims
    under_pct <- unders/n_sims
    
    wpct_1[1] <- team_one_win_pct
    wpct_X[1] <- team_one_draw_pct
    wpct_2[1] <- team_two_win_pct
    pct_overs[1] <- over_pct
    pct_unders[1] <- under_pct
    
    print(paste0(fixtures$home$name[j]," %: ", round(team_one_win_pct * 100, 2)))
    print(paste0("Draw %: ", round(team_one_draw_pct * 100, 2)))
    print(paste0(fixtures$away$name[j]," %: ", round(team_two_win_pct * 100, 2)))
    
    model_data$h_score_pass_shot[j] <- h_score
    model_data$a_score_pass_shot[j] <- a_score
    model_data$h_wpct_pass_shot[j] <- team_one_win_pct
    model_data$x_wpct_pass_shot[j] <- team_one_draw_pct
    model_data$a_wpct_pass_shot[j] <- team_two_win_pct
    model_data$o_pct_pass_shot[j] <- over_pct
    model_data$u_pct_pass_shot[j] <- under_pct
    model_data$h_spct_pass_shot[j] <- ifelse(h_score>a_score,team_one_win_pct,(team_one_win_pct+team_one_draw_pct))
    model_data$a_spct_pass_shot[j] <- ifelse(h_score<a_score,team_two_win_pct,(team_two_win_pct+team_one_draw_pct))
    
    print("--XG Model--")
    
    #XG Model
    
    h_score <- sum(player_stats_game$expected_xg[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])
    a_score <- sum(player_stats_game$expected_xg[player_stats_game$team==fixtures$away$name[j]&player_stats_game$match_id==fixtures$id[j]])
    
    h_score <- h_score+goalie_ratings[[2]]
    a_score <- a_score+goalie_ratings[[1]]
    
    print(paste0("Score: ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
    print(paste0("Total: ",round(h_score,4)+round(a_score,4)))
    
    set.seed(123)
    n_sims <- 10000
    team_one_eg <- h_score
    team_two_eg <- a_score
    team_one_wins <- 0
    team_one_draws <- 0
    team_one_losses <- 0
    team_two_wins <- 0
    team_two_draws <- 0
    team_two_losses <- 0
    overs <- 0 
    unders <- 0
    h_gsc_list_2 <- vector("list",1)
    a_gsc_list_2 <- vector("list",1)
    for (i in 1:n_sims) {
      
      h_goalscorers <- "none"
      a_goalscorers <- "none"
      
      
      team_one_goals <- rpois(1, lambda = h_score)
      team_two_goals <- rpois(1, lambda = a_score)
      
      
      
      if (team_one_goals > team_two_goals) {
        team_one_wins <- team_one_wins + 1
        team_two_losses <- team_two_losses + 1
      } else if (team_two_goals > team_one_goals) {
        team_one_losses <- team_one_losses + 1
        team_two_wins <- team_two_wins + 1
      } else {
        team_one_draws <- team_one_draws + 1
        team_two_draws <- team_two_draws + 1
      }
      
      if ((team_one_goals + team_two_goals)>=2.5) {
        overs <- overs + 1
      } else {
        unders <- unders + 1
      }
      
      if(team_one_goals==0){
        
      }else{
        for(u in 1:team_one_goals){
          h_goalscorers[u] <-  sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$home$name[j]],"own goal"),
                                      1,
                                      prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]],
                                               (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]])))) 
        }
      }
      
      
      if(team_two_goals==0){
        
      }else{
        
        for(u in 1:team_two_goals){
          a_goalscorers[u] <- sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$away$name[j]],"own goal"),
                                     1,
                                     prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]],
                                              (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]])))) 
        }
        
      }
      
      
      h_gsc_list_2[[i]] <- unique(h_goalscorers)
      a_gsc_list_2[[i]] <- unique(a_goalscorers)
      
    }
    team_one_win_pct <- team_one_wins / n_sims
    team_one_draw_pct <- team_one_draws / n_sims
    team_two_win_pct <- team_two_wins / n_sims
    team_two_draw_pct <- team_two_draws / n_sims
    over_pct <- overs / n_sims
    under_pct <- unders/n_sims
    
    wpct_1[2] <- team_one_win_pct
    wpct_X[2] <- team_one_draw_pct
    wpct_2[2] <- team_two_win_pct
    pct_overs[2] <- over_pct
    pct_unders[2] <- under_pct
    
    print(paste0(fixtures$home$name[j]," %: ", round(team_one_win_pct * 100, 2)))
    print(paste0("Draw %: ", round(team_one_draw_pct * 100, 2)))
    print(paste0(fixtures$away$name[j]," %: ", round(team_two_win_pct * 100, 2)))
    
    model_data$h_score_xg[j] <- h_score
    model_data$a_score_xg[j] <- a_score
    model_data$h_wpct_xg[j] <- team_one_win_pct
    model_data$x_wpct_xg[j] <- team_one_draw_pct
    model_data$a_wpct_xg[j] <- team_two_win_pct
    model_data$o_pct_xg[j] <- over_pct
    model_data$u_pct_xg[j] <- under_pct
    model_data$h_spct_xg[j] <- ifelse(h_score>a_score,team_one_win_pct,(team_one_win_pct+team_one_draw_pct))
    model_data$a_spct_xg[j] <- ifelse(h_score<a_score,team_two_win_pct,(team_two_win_pct+team_one_draw_pct))
    
    print("--XGoT Model--")
    
    #XGoT Model
    
    h_score <- sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])
    a_score <- sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]&player_stats_game$match_id==fixtures$id[j]])
    
    h_score <- h_score+goalie_ratings[[2]]
    a_score <- a_score+goalie_ratings[[1]]
    
    print(paste0("Score: ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
    print(paste0("Total: ",round(h_score,4)+round(a_score,4)))
    
    set.seed(123)
    n_sims <- 10000
    team_one_eg <- h_score
    team_two_eg <- a_score
    team_one_wins <- 0
    team_one_draws <- 0
    team_one_losses <- 0
    team_two_wins <- 0
    team_two_draws <- 0
    team_two_losses <- 0
    overs <- 0 
    unders <- 0
    h_gsc_list_3 <- vector("list",1)
    a_gsc_list_3 <- vector("list",1)
    for (i in 1:n_sims) {
      
      h_goalscorers <- "none"
      a_goalscorers <- "none"
      
      team_one_goals <- rpois(1, lambda = h_score)
      team_two_goals <- rpois(1, lambda = a_score)
      
      
      
      if (team_one_goals > team_two_goals) {
        team_one_wins <- team_one_wins + 1
        team_two_losses <- team_two_losses + 1
      } else if (team_two_goals > team_one_goals) {
        team_one_losses <- team_one_losses + 1
        team_two_wins <- team_two_wins + 1
      } else {
        team_one_draws <- team_one_draws + 1
        team_two_draws <- team_two_draws + 1
      }
      
      if ((team_one_goals + team_two_goals)>=2.5) {
        overs <- overs + 1
      } else {
        unders <- unders + 1
      }
      
      if(team_one_goals==0){
        
      }else{
        for(u in 1:team_one_goals){
          h_goalscorers[u] <-  sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$home$name[j]],"own goal"),
                                      1,
                                      prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]],
                                               (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]])))) 
        }
      }
      
      
      if(team_two_goals==0){
        
      }else{
        
        for(u in 1:team_two_goals){
          a_goalscorers[u] <- sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$away$name[j]],"own goal"),
                                     1,
                                     prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]],
                                              (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]])))) 
        }
        
      }
      
      
      h_gsc_list_3[[i]] <- unique(h_goalscorers)
      a_gsc_list_3[[i]] <- unique(a_goalscorers)
      
    }
    team_one_win_pct <- team_one_wins / n_sims
    team_one_draw_pct <- team_one_draws / n_sims
    team_two_win_pct <- team_two_wins / n_sims
    team_two_draw_pct <- team_two_draws / n_sims
    over_pct <- overs / n_sims
    under_pct <- unders/n_sims
    
    wpct_1[3] <- team_one_win_pct
    wpct_X[3] <- team_one_draw_pct
    wpct_2[3] <- team_two_win_pct
    pct_overs[3] <- over_pct
    pct_unders[3] <- under_pct
    
    print(paste0(fixtures$home$name[j]," %: ", round(team_one_win_pct * 100, 2)))
    print(paste0("Draw %: ", round(team_one_draw_pct * 100, 2)))
    print(paste0(fixtures$away$name[j]," %: ", round(team_two_win_pct * 100, 2)))
    
    model_data$h_score_xgot[j] <- h_score
    model_data$a_score_xgot[j] <- a_score
    model_data$h_wpct_xgot[j] <- team_one_win_pct
    model_data$x_wpct_xgot[j] <- team_one_draw_pct
    model_data$a_wpct_xgot[j] <- team_two_win_pct
    model_data$o_pct_xgot[j] <- over_pct
    model_data$u_pct_xgot[j] <- under_pct
    model_data$h_spct_xgot[j] <- ifelse(h_score>a_score,team_one_win_pct,(team_one_win_pct+team_one_draw_pct))
    model_data$a_spct_xgot[j] <- ifelse(h_score<a_score,team_two_win_pct,(team_two_win_pct+team_one_draw_pct))
    
    print("--XA Model--")
    
    #XA Model
    
    h_conv_rate <- mean(stat_game$expected_goals_ot[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE])
    a_conv_rate <- mean(stat_game$expected_goals_ot[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE])
    
    h_score <- sum(player_stats_game$expected_xa[player_stats_game$team==fixtures$home$name[j]&player_stats_game$match_id==fixtures$id[j]])+h_conv_rate
    a_score <- sum(player_stats_game$expected_xa[player_stats_game$team==fixtures$away$name[j]&player_stats_game$match_id==fixtures$id[j]])+a_conv_rate
    
    h_score <- h_score+goalie_ratings[[2]]
    a_score <- a_score+goalie_ratings[[1]]
    
    print(paste0("Score: ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
    print(paste0("Total: ",round(h_score,4)+round(a_score,4)))
    
    set.seed(123)
    n_sims <- 10000
    team_one_eg <- h_score
    team_two_eg <- a_score
    team_one_wins <- 0
    team_one_draws <- 0
    team_one_losses <- 0
    team_two_wins <- 0
    team_two_draws <- 0
    team_two_losses <- 0
    overs <- 0 
    unders <- 0
    h_gsc_list_0 <- vector("list",1)
    a_gsc_list_0 <- vector("list",1)
    for (i in 1:n_sims) {
      
      h_goalscorers <- "none"
      a_goalscorers <- "none"
      
      team_one_goals <- rpois(1, lambda = h_score)
      team_two_goals <- rpois(1, lambda = a_score)
      
      
      
      if (team_one_goals > team_two_goals) {
        team_one_wins <- team_one_wins + 1
        team_two_losses <- team_two_losses + 1
      } else if (team_two_goals > team_one_goals) {
        team_one_losses <- team_one_losses + 1
        team_two_wins <- team_two_wins + 1
      } else {
        team_one_draws <- team_one_draws + 1
        team_two_draws <- team_two_draws + 1
      }
      
      if ((team_one_goals + team_two_goals)>=2.5) {
        overs <- overs + 1
      } else {
        unders <- unders + 1
      }
      
      if(team_one_goals==0){
        
      }else{
        for(u in 1:team_one_goals){
          h_goalscorers[u] <-  sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$home$name[j]],"own goal"),
                                      1,
                                      prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]],
                                               (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]])))) 
        }
      }
      
      
      if(team_two_goals==0){
        
      }else{
        
        for(u in 1:team_two_goals){
          a_goalscorers[u] <- sample(c(player_stats_game$pageUrl2[player_stats_game$team==fixtures$away$name[j]],"own goal"),
                                     1,
                                     prob = c(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]],
                                              (.05*sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]])))) 
        }
        
      }
      
      
      h_gsc_list_0[[i]] <- unique(h_goalscorers)
      a_gsc_list_0[[i]] <- unique(a_goalscorers)
      
    }
    team_one_win_pct <- team_one_wins / n_sims
    team_one_draw_pct <- team_one_draws / n_sims
    team_two_win_pct <- team_two_wins / n_sims
    team_two_draw_pct <- team_two_draws / n_sims
    over_pct <- overs / n_sims
    under_pct <- unders/n_sims
    
    wpct_1[4] <- team_one_win_pct
    wpct_X[4] <- team_one_draw_pct
    wpct_2[4] <- team_two_win_pct
    pct_overs[4] <- over_pct
    pct_unders[4] <- under_pct
    
    print(paste0(fixtures$home$name[j]," %: ", round(team_one_win_pct * 100, 2)))
    print(paste0("Draw %: ", round(team_one_draw_pct * 100, 2)))
    print(paste0(fixtures$away$name[j]," %: ", round(team_two_win_pct * 100, 2)))
    
    model_data$h_score_xa[j] <- h_score
    model_data$a_score_xa[j] <- a_score
    model_data$h_wpct_xa[j] <- team_one_win_pct
    model_data$x_wpct_xa[j] <- team_one_draw_pct
    model_data$a_wpct_xa[j] <- team_two_win_pct
    model_data$o_pct_xa[j] <- over_pct
    model_data$u_pct_xa[j] <- under_pct
    model_data$h_spct_xa[j] <- ifelse(h_score>a_score,team_one_win_pct,(team_one_win_pct+team_one_draw_pct))
    model_data$a_spct_xa[j] <- ifelse(h_score<a_score,team_two_win_pct,(team_two_win_pct+team_one_draw_pct))
   
    spct_1 <- wpct_1+wpct_X
    spct_2 <- wpct_2+wpct_X
    
    print("--Mean 4 Models--")
    
    print(paste0(fixtures$home$name[j]," has a mean win % of ",
                 round((mean(wpct_1)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2),"%; a min win % of ",
                 round(min(wpct_1) * 100, 2),  "%; a max win % of ",round(max(wpct_1) * 100, 2),"%."))
    print(paste0("Draw has a mean win % of ",
                 round((mean(wpct_X)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2),
                 "%; a min win % of ",
                 round(min(wpct_X) * 100, 2),  "%; a max win % of ",round(max(wpct_X) * 100, 2),"%."))
    print(paste0(fixtures$away$name[j]," has a mean win % of ",
                 round((mean(wpct_2)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2),
                 "%; a min win % of ",
                 round(min(wpct_2) * 100, 2),  "%; a max win of % ",round(max(wpct_2) * 100, 2),"%."))
    
    print(paste0(fixtures$home$name[j]," has a mean win of the Double Chance of ",
                 round((mean(spct_1))*100, 2),"%; a min win % of ",
                 round(min(spct_1) * 100, 2),  "%, a max win % of ",round(max(spct_1) * 100, 2),"%."))
    print(paste0(fixtures$away$name[j]," has a mean win of the Double Chance of ",
                 round((mean(spct_2))*100, 2),
                 "%; a min win % of ",
                 round(min(spct_2) * 100, 2),  "%, a max win % of ",round(max(spct_2) * 100, 2),"%."))
    
    
    model_data$h_wpct_mean[j] <- round((mean(wpct_1)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2)
    model_data$x_wpct_mean[j] <- round((mean(wpct_X)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2)
    model_data$a_wpct_mean[j] <- round((mean(wpct_2)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2)
    model_data$h_spct_mean[j] <- ifelse( model_data$h_wpct_mean[j]>model_data$a_wpct_mean[j],model_data$h_wpct_mean[j],(model_data$h_wpct_mean[j]+model_data$x_wpct_mean[j]))
    model_data$a_spct_mean[j] <- ifelse(model_data$h_wpct_mean[j]<model_data$a_wpct_mean[j],model_data$a_wpct_mean[j],(model_data$a_wpct_mean[j]+model_data$x_wpct_mean[j]))
    
    
    print(paste0("The mean likelihood that there are more than ", 2.5," goals is ",round(mean(pct_overs)*100,2), "%; max % is ",
                 round(max(pct_overs)*100,2)," and min % is ",round(min(pct_overs)*100,2),"%."))
    
    print(paste0("The mean likelihood that there are fewer than ", 2.5," goals is ",round(mean(pct_unders)*100,2), "%; max % is ",
                 round(max(pct_unders)*100,2)," and min % is ",round(min(pct_unders)*100,2),"%."))
    
    
    model_data$opct_mean[j] <- round(mean(unlist(pct_overs))*100,2)
    model_data$upct_mean[j] <- round(mean(unlist(pct_unders))*100,2)
    
    print("--Sportsbook Odds--")
    
    h_odds <- lines$BARSTOOL_TEAM1_ML[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    a_odds <- lines$BARSTOOL_TEAM2_ML[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    d_odds <- lines$BARSTOOL_X_ML[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    
    h_spread_odds <- lines$BARSTOOL_TEAM1_SPREAD_LINE[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    a_spread_odds <- lines$BARSTOOL_TEAM2_SPREAD_LINE[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    
    total <- lines$BARSTOOL_TOTAL[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    total_line_over <- lines$BARSTOOL_OVER_LINE[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    total_line_under <- lines$BARSTOOL_UNDER_LINE[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
    
    print(paste0(fixtures$home$name[j]," Barstool Odds are: ", round(1/as.numeric(h_odds) * 100, 2)))
    print(paste0("The Barstool Odds of a Draw are: ", round(1/as.numeric(d_odds) * 100, 2)))
    print(paste0(fixtures$away$name[j]," Barstool Odds are: ", round(1/as.numeric(a_odds) * 100, 2)))
    
    print(paste0(fixtures$home$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(h_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                             round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                             round(1/as.numeric(a_odds) * 100, 2))),3)))
    print(paste0("The Adjusted Barstool Odds of a Draw are: ", round(100*round(1/as.numeric(d_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                    round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                    round(1/as.numeric(a_odds) * 100, 2))),3)))
    print(paste0(fixtures$away$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(a_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                             round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                             round(1/as.numeric(a_odds) * 100, 2))),3)))
    
    if(lines$BARSTOOL_OVER_LINE[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]==""){
      next()
    }
    
    print(paste0(fixtures$home$name[j],"'s Barstool Odds to cover the ",
                 lines$BARSTOOL_TEAM1_SPREAD[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1],
                 " spread are: ", round(1/as.numeric(h_spread_odds) * 100, 2)))
    print(paste0(fixtures$away$name[j],"'s Barstool Odds to cover the ",
                 lines$BARSTOOL_TEAM2_SPREAD[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1],
                 " spread are: ", round(1/as.numeric(a_spread_odds) * 100, 2)))
    
    print(paste0("Barstool's line for the total is ", total, " at ",total_line_over,
                 ", which is a ",round(1/as.numeric(total_line_over)*100,2),"% chance of the over ",2.5, " hitting.",
                 " The line for the under is ",total_line_under,", which is a ",round(1/as.numeric(total_line_under)*100,2),"% chance of the under ",2.5, " hitting."))
    
    model_data$h_wpct_odds[j] <- round(1/as.numeric(h_odds) * 100, 2)
    model_data$x_wpct_odds[j] <- round(1/as.numeric(d_odds) * 100, 2)
    model_data$a_wpct_odds[j] <- round(1/as.numeric(a_odds) * 100, 2)
    model_data$total_odds[j] <- "2.5"
    model_data$opct_odds[j] <- round(1/as.numeric(total_line_over)*100,2)
    model_data$upct_odds[j] <- round(1/as.numeric(total_line_under)*100,2)
    model_data$h_spct_odds[j] <- round(1/as.numeric(h_spread_odds) * 100, 2)
    model_data$a_spct_odds[j] <- round(1/as.numeric(a_spread_odds) * 100, 2)
    
    
    print("-------------------------------------------------------------------------")
    
    home_attg[[j]] <- data.frame(table(unlist(c(unlist(h_gsc_list_0),
                                                unlist(h_gsc_list_1),
                                                unlist(h_gsc_list_2),
                                                unlist(h_gsc_list_3)))))
    
    away_attg[[j]] <- data.frame(table(unlist(c(unlist(a_gsc_list_0),
                                                unlist(a_gsc_list_1),
                                                unlist(a_gsc_list_2),
                                                unlist(a_gsc_list_3)))))
    
    print("_____________________________________________________________________________________________________________________________")
    
    
    pl_games <- list.files()
    if(is.element(paste0(model_data$confirmed[j]," lineups for ",model_data$home[j], " v ", model_data$away[j]," on ",as.Date(model_data$date_time[j]),".csv"), pl_games)&
       model_data$confirmed[j]=="confirmed"){
      
      
      
    }else{
      
      write.csv(model_data[j,],paste0(model_data$confirmed[j]," lineups for ",model_data$home[j], " v ", model_data$away[j]," on ",as.Date(model_data$date_time[j]),".csv"), row.names = F)
      
    }
    
    
    
    
    
    if((min(wpct_1)*100)>(1/as.numeric(h_odds) * 100)){
      card[q] <- fixtures$home$name[j]
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    
    if((min(wpct_X)*100)>(1/as.numeric(d_odds) * 100)){
      card[q] <- paste0("draw ",fixtures$home$name[j],"/",fixtures$away$name[j])
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    
    if((min(wpct_2)*100)>(1/as.numeric(a_odds) * 100)){
      card[q] <- fixtures$away$name[j]
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    
    if((min(pct_unders)*100)>(1/as.numeric(total_line_under)*100)){
      card[q] <- paste0("under ",fixtures$home$name[j],"/",fixtures$away$name[j])
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    
    if((min(pct_overs)*100)>(1/as.numeric(total_line_over)*100)){
      card[q] <- paste0("over ",fixtures$home$name[j],"/",fixtures$away$name[j])
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    if((min(spct_1) * 100)>(1/as.numeric(h_spread_odds) * 100)&lines$BARSTOOL_TEAM1_SPREAD[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]>0){
      card[q] <- paste0(fixtures$home$name[j]," ATS")
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
    if((min(spct_2) * 100)>(1/as.numeric(a_spread_odds) * 100)&lines$BARSTOOL_TEAM2_SPREAD[lines$BARSTOOL_TEAM1==fixtures$home$name[j]&lines$BARSTOOL_TEAM2==fixtures$away$name[j]][1]>0){
      card[q] <- paste0(fixtures$away$name[j]," ATS")
      scheduled_time[q] <- force_tz(as.POSIXct(fixtures$status$utcTime[j], format = "%Y-%m-%dT%H:%M:%OSZ"), "America/Los_Angeles")-28800
      q <- q+1
    }
  }
  
  print(data.frame(bet = card, date_time = scheduled_time))
  cards <- data.frame(bet = card, date_time = scheduled_time)
  game_model_list <- model_data
  
  
  list(cards,game_model_list,home_attg,away_attg)
}

send_to_git <- function(league,team1,team2){
  Sys.setenv(GITHUB_PAT = "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")
  closeAllConnections()
  dir.create("Soccer_Models")
  # Repository details
  repo_url <- "https://github.com/mbrownsword/Soccer_Models.git"
  file_path <- paste0(getwd(),"/",iconv(paste0(gsub("\\.","",paste0(gsub("-","",date)," ",paste0(l$ccode[l$id==id[zz]]," ",l$name[l$id==id[zz]]),"_",team1," v ",team2,"_card")),".txt"),"latin1","ascii",sub = ""))
  file_name <- iconv(paste0(gsub("\\.","",paste0(gsub("-","",date)," ",paste0(l$ccode[l$id==id[zz]]," ",l$name[l$id==id[zz]]),"_",team1," v ",team2,"_card")),".txt"),"latin1","ascii",sub = "")
  
  # Clone the repository
  repo <- git2r::clone(repo_url, paste0(getwd(),"/","Soccer_Models"))
  
  # Add the file to the repository
  writeLines(readLines(file_name), file.path(paste0(getwd(),"/","Soccer_Models/"), file_name))
  git2r::config(repo, user.name = "mbrownsword", user.email = "mbrownsword6@gmail.com")
  git2r::add(repo, file_name)
  # Commit the changes
  git2r::commit(repo, paste0("New commit ID #",sample(1000,1)))
  status_repo <- git2r::status(repo)
  
  
  # Push the changes
  cred <- cred_token()
  
  git2r::push(file.path(paste0(getwd(),"/","Soccer_Models")),credentials = cred)
  Loc <- paste0(getwd(),"/","Soccer_Models")
  shell( glue::glue("rmdir /s /q \"{Loc}\" ") )
}


player_props_no_passes <- function(player_prop_list,player_stats_game,league){
  all_player_tackles_lines <- player_prop_list[[1]]
  all_player_shots_lines <- player_prop_list[[2]]
  all_player_sogs_lines <- player_prop_list[[3]]
  all_player_goals_lines <- player_prop_list[[4]]
  all_player_ast_lines <- player_prop_list[[5]]
  all_player_tackles_lines$number <- all_player_tackles_lines$number
  all_player_shots_lines$number <- all_player_shots_lines$number
  all_player_sogs_lines$number <- all_player_sogs_lines$number
  all_player_goals_lines$number <- all_player_goals_lines$number
  all_player_ast_lines$number <- all_player_ast_lines$number
  
  all_player_tackles_lines$player <- stri_trans_general(str = all_player_tackles_lines$player, id = "Latin-ASCII")
  all_player_shots_lines$player <- stri_trans_general(str = all_player_shots_lines$player, id = "Latin-ASCII")
  all_player_sogs_lines$player <- stri_trans_general(str = all_player_sogs_lines$player, id = "Latin-ASCII")
  all_player_goals_lines$player <- stri_trans_general(str = all_player_goals_lines$player, id = "Latin-ASCII")
  all_player_ast_lines$player <- stri_trans_general(str = all_player_ast_lines$player, id = "Latin-ASCII")
  
  
  
  
  
  
  key_player_sogs <- player_stats_game[,c("player_name"      ,         "positionRow"                ,       "team"     ,
                                          "expected_sogs","exp_min"  )]
  
  key_player_shots <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                           "expected_shots", "exp_min" )]
  
  
  key_player_tackles <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                             "expected_tackles","exp_min")]
  
  
  
  
  key_player_xg <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                        "expected_xgot","exp_min")]
  
  
  key_player_xa <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                        "expected_xa","exp_min" )]
  
  
  key_player_tackles$player_name <- stri_trans_general(str = key_player_tackles$player_name, id = "Latin-ASCII")
  key_player_shots$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
  key_player_sogs$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")
  key_player_xg$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
  key_player_xa$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")
  
  
  
  
  all_player_goals_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_goals_lines$player)){
    game_key_player_goals <- key_player_xg[which(key_player_xg$team==team_df3$f_team[team_df3$b_team==all_player_goals_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_goals_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_goals$player_name))){
      
      
      
      mu <- game_key_player_goals$expected_xgot[which(grepl(h_name,game_key_player_goals$player_name))]
      x <- all_player_goals_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_goals_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_goals_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_goals$player_name)))==1){
        
        mu <- game_key_player_goals$expected_xgot[which(grepl(h_name_2,game_key_player_goals$player_name))]
        x <- all_player_goals_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_goals_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_goals_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_goals$player_name)))==1){
          
          mu <- game_key_player_goals$expected_xgot[which(grepl(h_name_3,game_key_player_goals$player_name))]
          x <- all_player_goals_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_goals_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_goals_lines$prob_dif <- all_player_goals_lines$ex_prob-all_player_goals_lines$prob_goals
  
  all_player_ast_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_ast_lines$player)){
    game_key_player_ast <- key_player_xa[which(key_player_xa$team==team_df3$f_team[team_df3$b_team==all_player_ast_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_ast_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_ast$player_name))){
      
      
      
      mu <- game_key_player_ast$expected_xa[which(grepl(h_name,game_key_player_ast$player_name))]
      x <- all_player_ast_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_ast_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_ast_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_ast$player_name)))==1){
        
        mu <- game_key_player_ast$expected_xa[which(grepl(h_name_2,game_key_player_ast$player_name))]
        x <- all_player_ast_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_ast_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_ast_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_ast$player_name)))==1){
          
          mu <- game_key_player_ast$expected_xa[which(grepl(h_name_3,game_key_player_ast$player_name))]
          x <- all_player_ast_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_ast_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_ast_lines$prob_dif <- all_player_ast_lines$ex_prob-all_player_ast_lines$prob_ast
  
  all_player_tackles_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_tackles_lines$player)){
    game_key_player_tackles <- key_player_tackles[which(key_player_tackles$team==team_df3$f_team[team_df3$b_team==all_player_tackles_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_tackles_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_tackles$player_name))){
      
      
      
      mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name,game_key_player_tackles$player_name))]
      x <- all_player_tackles_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_tackles_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_tackles_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_tackles$player_name)))==1){
        
        mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name_2,game_key_player_tackles$player_name))]
        x <- all_player_tackles_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_tackles_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_tackles_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_tackles$player_name)))==1){
          
          mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name_3,game_key_player_tackles$player_name))]
          x <- all_player_tackles_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_tackles_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_tackles_lines$prob_dif <- all_player_tackles_lines$ex_prob-all_player_tackles_lines$prob_tackle
  
  
  
  all_player_shots_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_shots_lines$player)){
    game_key_player_shots <- key_player_shots[which(key_player_shots$team==team_df3$f_team[team_df3$b_team==all_player_shots_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_shots_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_shots$player_name))){
      
      mu <- game_key_player_shots$expected_shots[which(grepl(h_name,game_key_player_shots$player_name))]
      x <- all_player_shots_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_shots_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_shots_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_shots$player_name)))==1){
        
        mu <- game_key_player_shots$expected_shots[which(grepl(h_name_2,game_key_player_shots$player_name))]
        x <- all_player_shots_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_shots_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_shots_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_shots$player_name)))==1){
          
          mu <- game_key_player_shots$expected_shots[which(grepl(h_name_3,game_key_player_shots$player_name))]
          x <- all_player_shots_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_shots_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_shots_lines$prob_dif <- all_player_shots_lines$ex_prob-all_player_shots_lines$prob_shots
  
  all_player_sogs_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_sogs_lines$player)){
    game_key_player_sogs <- key_player_sogs[which(key_player_sogs$team==team_df3$f_team[team_df3$b_team==all_player_sogs_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_sogs_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_sogs$player_name))){
      
      mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name,game_key_player_sogs$player_name))]
      x <- all_player_sogs_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_sogs_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_sogs_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_sogs$player_name)))==1){
        
        mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name_2,game_key_player_sogs$player_name))]
        x <- all_player_sogs_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_sogs_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_sogs_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_sogs$player_name)))==1){
          
          mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name_3,game_key_player_sogs$player_name))]
          x <- all_player_sogs_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_sogs_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_sogs_lines$prob_dif <- all_player_sogs_lines$ex_prob-all_player_sogs_lines$prob_sogs
  
  all_player_sogs_lines <- all_player_sogs_lines[all_player_sogs_lines$ex_prob>0,]
  all_player_shots_lines <- all_player_shots_lines[all_player_shots_lines$ex_prob>0,]
  
  
  all_player_tackles_lines <- all_player_tackles_lines[all_player_tackles_lines$ex_prob>0,]
  if(length(all_player_goals_lines$ex_prob>0)==0){
    
  }else{
    all_player_goals_lines <- all_player_goals_lines[all_player_goals_lines$ex_prob>0,]
  }
  if(length(all_player_ast_lines$ex_prob>0)==0){
    
  }else{
    all_player_ast_lines <- all_player_ast_lines[all_player_ast_lines$ex_prob>0,]
  }
  
  
  
  
  tackles_lines <- all_player_tackles_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  shots_lines <- all_player_shots_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  sogs_lines <- all_player_sogs_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  goals_lines <- all_player_goals_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  ast_lines <- all_player_ast_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  
  
  
  all_player_tackles_lines$statistic <- "tackles"
  all_player_shots_lines$statistic <- "shots"
  all_player_sogs_lines$statistic <- "sogs"
  all_player_goals_lines$statistic <- "goals"
  all_player_ast_lines$statistic <- "assists"
  
  all_player_tackles_lines$league <- league
  all_player_shots_lines$league <- league
  all_player_sogs_lines$league <- league
  all_player_goals_lines$league <- league
  all_player_ast_lines$league <- league
  
  prop_df <- rbind(all_player_tackles_lines,
                   all_player_shots_lines,
                   all_player_sogs_lines,
                   all_player_goals_lines,
                   all_player_ast_lines, fill = TRUE)
  
  
  
  all_overs <- prop_df[which(prop_df$prob_dif>.10),]
  if(length(all_overs$player)==0){
    
  }else{
    print(kable(all_overs))
  }
  
  list(prop_df,player_stats_game)
}



player_props_all <- function(player_prop_list,player_stats_game,league){
  
  all_player_tackles_lines <- player_prop_list[[1]]
  all_player_passes_lines <- player_prop_list[[2]]
  all_player_shots_lines <- player_prop_list[[3]]
  all_player_sogs_lines <- player_prop_list[[4]]
  all_player_goals_lines <- player_prop_list[[5]]
  all_player_ast_lines <- player_prop_list[[6]]
  all_player_tackles_lines$number <- all_player_tackles_lines$number
  all_player_passes_lines$number <- all_player_passes_lines$number
  all_player_shots_lines$number <- all_player_shots_lines$number
  all_player_sogs_lines$number <- all_player_sogs_lines$number
  all_player_goals_lines$number <- all_player_goals_lines$number
  all_player_ast_lines$number <- all_player_ast_lines$number
  
  all_player_tackles_lines$player <- stri_trans_general(str = all_player_tackles_lines$player, id = "Latin-ASCII")
  all_player_passes_lines$player <- stri_trans_general(str = all_player_passes_lines$player, id = "Latin-ASCII")
  all_player_shots_lines$player <- stri_trans_general(str = all_player_shots_lines$player, id = "Latin-ASCII")
  all_player_sogs_lines$player <- stri_trans_general(str = all_player_sogs_lines$player, id = "Latin-ASCII")
  all_player_goals_lines$player <- stri_trans_general(str = all_player_goals_lines$player, id = "Latin-ASCII")
  all_player_ast_lines$player <- stri_trans_general(str = all_player_ast_lines$player, id = "Latin-ASCII")
  
  
  
  key_player_sogs <- player_stats_game[,c("player_name"      ,         "positionRow"                ,       "team"     ,
                                          "expected_sogs","exp_min"  )]
  
  key_player_shots <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                           "expected_shots", "exp_min" )]
  
  
  key_player_tackles <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                             "expected_tackles","exp_min")]
  
  
  key_player_passes <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                            "expected_passes","exp_min" )]
  
  key_player_xg <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                        "expected_xgot","exp_min")]
  
  
  key_player_xa <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                        "expected_xa","exp_min" )]
  
  
  key_player_tackles$player_name <- stri_trans_general(str = key_player_tackles$player_name, id = "Latin-ASCII")
  key_player_passes$player_name <- stri_trans_general(str = key_player_passes$player_name, id = "Latin-ASCII")
  key_player_shots$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
  key_player_sogs$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")
  key_player_xg$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
  key_player_xa$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")
  
  all_player_goals_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_goals_lines$player)){
    game_key_player_goals <- key_player_xg[which(key_player_xg$team==team_df3$f_team[team_df3$b_team==all_player_goals_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_goals_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_goals$player_name))){
      
      
      
      mu <- game_key_player_goals$expected_xgot[which(grepl(h_name,game_key_player_goals$player_name))]
      x <- all_player_goals_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_goals_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_goals_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_goals$player_name)))==1){
        
        mu <- game_key_player_goals$expected_xgot[which(grepl(h_name_2,game_key_player_goals$player_name))]
        x <- all_player_goals_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_goals_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_goals_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_goals$player_name)))==1){
          
          mu <- game_key_player_goals$expected_xgot[which(grepl(h_name_3,game_key_player_goals$player_name))]
          x <- all_player_goals_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_goals_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_goals_lines$prob_dif <- all_player_goals_lines$ex_prob-all_player_goals_lines$prob_goals
  
  all_player_ast_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_ast_lines$player)){
    game_key_player_ast <- key_player_xa[which(key_player_xa$team==team_df3$f_team[team_df3$b_team==all_player_ast_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_ast_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_ast$player_name))){
      
      
      
      mu <- game_key_player_ast$expected_xa[which(grepl(h_name,game_key_player_ast$player_name))]
      x <- all_player_ast_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_ast_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_ast_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_ast$player_name)))==1){
        
        mu <- game_key_player_ast$expected_xa[which(grepl(h_name_2,game_key_player_ast$player_name))]
        x <- all_player_ast_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_ast_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_ast_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_ast$player_name)))==1){
          
          mu <- game_key_player_ast$expected_xa[which(grepl(h_name_3,game_key_player_ast$player_name))]
          x <- all_player_ast_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_ast_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_ast_lines$prob_dif <- all_player_ast_lines$ex_prob-all_player_ast_lines$prob_ast
  
  all_player_tackles_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_tackles_lines$player)){
    game_key_player_tackles <- key_player_tackles[which(key_player_tackles$team==team_df3$f_team[team_df3$b_team==all_player_tackles_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_tackles_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_tackles$player_name))){
      
      
      
      mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name,game_key_player_tackles$player_name))]
      x <- all_player_tackles_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_tackles_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_tackles_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_tackles$player_name)))==1){
        
        mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name_2,game_key_player_tackles$player_name))]
        x <- all_player_tackles_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_tackles_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_tackles_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_tackles$player_name)))==1){
          
          mu <- game_key_player_tackles$expected_tackles[which(grepl(h_name_3,game_key_player_tackles$player_name))]
          x <- all_player_tackles_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_tackles_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_tackles_lines$prob_dif <- all_player_tackles_lines$ex_prob-all_player_tackles_lines$prob_tackle
  
  all_player_passes_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_passes_lines$player)){
    game_key_player_passes <- key_player_passes[which(key_player_passes$team==team_df3$f_team[team_df3$b_team==all_player_passes_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_passes_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_passes$player_name))){
      
      mu <- game_key_player_passes$expected_passes[which(grepl(h_name,game_key_player_passes$player_name))]
      x <- all_player_passes_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_passes_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_passes_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_passes$player_name)))==1){
        
        mu <- game_key_player_passes$expected_passes[which(grepl(h_name_2,game_key_player_passes$player_name))]
        x <- all_player_passes_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_passes_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_passes_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_passes$player_name)))==1){
          
          mu <- game_key_player_passes$expected_passes[which(grepl(h_name_3,game_key_player_passes$player_name))]
          x <- all_player_passes_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_passes_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  all_player_passes_lines$prob_dif <- all_player_passes_lines$ex_prob-all_player_passes_lines$prob_passes
  
  all_player_shots_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_shots_lines$player)){
    game_key_player_shots <- key_player_shots[which(key_player_shots$team==team_df3$f_team[team_df3$b_team==all_player_shots_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_shots_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_shots$player_name))){
      
      mu <- game_key_player_shots$expected_shots[which(grepl(h_name,game_key_player_shots$player_name))]
      x <- all_player_shots_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_shots_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_shots_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_shots$player_name)))==1){
        
        mu <- game_key_player_shots$expected_shots[which(grepl(h_name_2,game_key_player_shots$player_name))]
        x <- all_player_shots_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_shots_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_shots_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_shots$player_name)))==1){
          
          mu <- game_key_player_shots$expected_shots[which(grepl(h_name_3,game_key_player_shots$player_name))]
          x <- all_player_shots_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_shots_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_shots_lines$prob_dif <- all_player_shots_lines$ex_prob-all_player_shots_lines$prob_shots
  
  all_player_sogs_lines$ex_prob <- 0
  options(scipen = 999)
  for(a in 1:length(all_player_sogs_lines$player)){
    game_key_player_sogs <- key_player_sogs[which(key_player_sogs$team==team_df3$f_team[team_df3$b_team==all_player_sogs_lines$team[a]]),]
    h_name <- tolower(gsub(" ","-",all_player_sogs_lines$player[a]))
    
    if(any(grepl(h_name,game_key_player_sogs$player_name))){
      
      mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name,game_key_player_sogs$player_name))]
      x <- all_player_sogs_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_sogs_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_2 <- tolower(paste0(strsplit(all_player_sogs_lines$player[a]," ")[[1]][1],"-"))
      
      if(length(which(grepl(h_name_2,game_key_player_sogs$player_name)))==1){
        
        mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name_2,game_key_player_sogs$player_name))]
        x <- all_player_sogs_lines$number[a]
        probability <- ppois(x, lambda = mu, lower.tail = FALSE)
        
        # Print the probability
        probability
        
        all_player_sogs_lines$ex_prob[a] <- round(probability,4)
        
      }else{
        
        h_name_3 <- tolower(paste0("-",strsplit(all_player_sogs_lines$player[a]," ")[[1]][2]))
        
        if(length(which(grepl(h_name_3,game_key_player_sogs$player_name)))==1){
          
          mu <- game_key_player_sogs$expected_sogs[which(grepl(h_name_3,game_key_player_sogs$player_name))]
          x <- all_player_sogs_lines$number[a]
          probability <- ppois(x, lambda = mu, lower.tail = FALSE)
          
          # Print the probability
          probability
          
          all_player_sogs_lines$ex_prob[a] <- round(probability,4)
          
        }else{
          
          
          
          
        }
        
        
      }
      
      
    }
    
    
    
  }
  
  all_player_sogs_lines$prob_dif <- all_player_sogs_lines$ex_prob-all_player_sogs_lines$prob_sogs
  
  all_player_sogs_lines <- all_player_sogs_lines[all_player_sogs_lines$ex_prob>0,]
  all_player_shots_lines <- all_player_shots_lines[all_player_shots_lines$ex_prob>0,]
  if(length(all_player_passes_lines$ex_prob>0)==0){
    
  }else{
    all_player_passes_lines <- all_player_passes_lines[all_player_passes_lines$ex_prob>0,]
  }
  
  all_player_tackles_lines <- all_player_tackles_lines[all_player_tackles_lines$ex_prob>0,]
  if(length(all_player_goals_lines$ex_prob>0)==0){
    
  }else{
    all_player_goals_lines <- all_player_goals_lines[all_player_goals_lines$ex_prob>0,]
  }
  if(length(all_player_ast_lines$ex_prob>0)==0){
    
  }else{
    all_player_ast_lines <- all_player_ast_lines[all_player_ast_lines$ex_prob>0,]
  }
  
  
  #plot(all_player_passes_lines$prob_passes,all_player_passes_lines$number)
  
  if(length(which(all_player_passes_lines$number<7.5))==0){
    
  }else{
    all_player_passes_lines <- all_player_passes_lines[-which(all_player_passes_lines$number<7.5),]
  }
  
  
  
  pass_lines <- all_player_passes_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  tackles_lines <- all_player_tackles_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  shots_lines <- all_player_shots_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  sogs_lines <- all_player_sogs_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  goals_lines <- all_player_goals_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  ast_lines <- all_player_ast_lines %>%
    group_by(player,team) %>%
    summarise(mpd = mean(prob_dif),
              maxpd = max(prob_dif),
              minpd = min(prob_dif))
  
  
  
  
  all_player_tackles_lines$statistic <- "tackles"
  all_player_passes_lines$statistic <- "passes"
  all_player_shots_lines$statistic <- "shots"
  all_player_sogs_lines$statistic <- "sogs"
  all_player_goals_lines$statistic <- "goals"
  all_player_ast_lines$statistic <- "assists"
  
  all_player_tackles_lines$league <- league
  all_player_passes_lines$league <- league
  all_player_shots_lines$league <- league
  all_player_sogs_lines$league <- league
  all_player_goals_lines$league <- league
  all_player_ast_lines$league <- league
  
  prop_df <- rbind(all_player_tackles_lines,
                   all_player_passes_lines,
                   all_player_shots_lines,
                   all_player_sogs_lines,
                   all_player_goals_lines,
                   all_player_ast_lines, fill = TRUE)
  
  
  all_overs <- prop_df[which(prop_df$prob_dif>.10),]
  if(length(all_overs$player)==0){
    
  }else{
    print(kable(all_overs))
  }
  
  list(prop_df,player_stats_game)
  
}

running_lines <- function(leagues_df, league_id, team_df2){
  
  
  setwd(paste0(wspc,"Fotmob Soccer Modelling"))
  source("book_functions.R")
  
  sport_codes <- read_xlsx("book_codes_all.xlsx", sheet = "overall")
  sport_matt_id <- 4
  matt_id <- league_id
  whsa <- barstool_games(sport_codes,sport_matt_id,leagues_df,matt_id)
  
  lines <- whsa
  
  for(i in 1:length(team_df2$team_1)){
    
    lines$BARSTOOL_TEAM1[lines$BARSTOOL_TEAM1==team_df2$team_1[i]] <- team_df2$team_2[i]
    lines$BARSTOOL_TEAM2[lines$BARSTOOL_TEAM2==team_df2$team_1[i]] <- team_df2$team_2[i]
    
  }
  
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Galatasaray A.S."] <- "Galatasaray"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Trabzonspor A.S."] <- "Trabzonspor"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Antalyaspor A.S." ] <- "Antalyaspor"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Fatih Karagumruk"] <- "Fatih Karagümrük"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Ankaragucu"] <- "Ankaragucu"
  lines$BARSTOOL_TEAM1[stri_trans_general(lines$BARSTOOL_TEAM1, "latin-ascii")=="Besiktas A.S."] <- "Besiktas"
  
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Galatasaray A.S."] <- "Galatasaray"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Trabzonspor A.S."] <- "Trabzonspor"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Antalyaspor A.S." ] <- "Antalyaspor"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Istanbul Basaksehir"] <- "Istanbul Basaksehir"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Kasimpasa"] <- "Kasimpasa"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Fatih Karagumruk"] <- "Fatih Karagümrük"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Ankaragucu"] <- "Ankaragucu"
  lines$BARSTOOL_TEAM2[stri_trans_general(lines$BARSTOOL_TEAM2, "latin-ascii")=="Besiktas A.S."] <- "Besiktas"
  
  lines
}

player_prop_lines <- function(league){
  
  games <- read_json(paste0("https://bv2.digitalsportstech.com/api/sgmGames?sb=ait&league=",league))
  
  player_tackles_lines <- vector("list",1)
  player_passes_lines <- vector("list",1)
  player_shots_lines <- vector("list",1)
  player_sogs_lines <- vector("list",1)
  player_goals_lines <- vector("list",1)
  player_ast_lines <- vector("list",1)
  
  for(k in 1:length(games)){
    
    if(games[[k]]$status=="inplay"){
      next()
    }
    
    tackles_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Tackles"))
    if(league=="seriea" | league == "dfl"){
      
    }else{
      player_lines <- vector("list",1)
      passes_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Passes"))
      for(i in 1:length(passes_lines[[1]]$players)){
        
        number <- 0
        odds <- 0
        
        player <- passes_lines[[1]]$players[[i]]$name
        
        team <- passes_lines[[1]]$players[[i]]$team
        for(j in 1:length(passes_lines[[1]]$players[[i]]$markets)){
          
          number[j] <-  passes_lines[[1]]$players[[i]]$markets[[j]]$value
          odds[j] <-  passes_lines[[1]]$players[[i]]$markets[[j]]$odds
          
        }
        
        player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                        team = rep(team,length(number)),
                                        number = number,
                                        odds = odds)
        
        
      }
      
      
      player_passes_lines[[k]] <- rbindlist(player_lines)
      all_player_passes_lines <- rbindlist(player_passes_lines)
      all_player_passes_lines$prob_passes <- round(1/all_player_passes_lines$odds,4)
    }
    
    shots_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Shots"))
    sogs_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Shots%20on%20Goal"))
    goals_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Goals"))
    ast_lines <- read_json(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Assists"))
    
    player_lines <- vector("list",1)
    
    for(i in 1:length(ast_lines[[1]]$players)){
      
      number <- 0
      odds <- 0
      
      player <- ast_lines[[1]]$players[[i]]$name
      team <- ast_lines[[1]]$players[[i]]$team
      for(j in 1:length(ast_lines[[1]]$players[[i]]$markets)){
        
        number[j] <-  ast_lines[[1]]$players[[i]]$markets[[j]]$value
        odds[j] <-  ast_lines[[1]]$players[[i]]$markets[[j]]$odds
        
      }
      
      player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                      team = rep(team,length(number)),
                                      number = number,
                                      odds = odds)
      
      
    }
    
    
    player_ast_lines[[k]] <- rbindlist(player_lines)
    
    player_lines <- vector("list",1)
    
    for(i in 1:length(goals_lines[[1]]$players)){
      
      number <- 0
      odds <- 0
      
      player <- goals_lines[[1]]$players[[i]]$name
      team <- goals_lines[[1]]$players[[i]]$team
      for(j in 1:length(goals_lines[[1]]$players[[i]]$markets)){
        
        number[j] <-  goals_lines[[1]]$players[[i]]$markets[[j]]$value
        odds[j] <-  goals_lines[[1]]$players[[i]]$markets[[j]]$odds
        
      }
      
      player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                      team = rep(team,length(number)),
                                      number = number,
                                      odds = odds)
      
      
    }
    
    
    player_goals_lines[[k]] <- rbindlist(player_lines)
    
    player_lines <- vector("list",1)
    
    for(i in 1:length(tackles_lines[[1]]$players)){
      
      number <- 0
      odds <- 0
      
      player <- tackles_lines[[1]]$players[[i]]$name
      team <- tackles_lines[[1]]$players[[i]]$team
      for(j in 1:length(tackles_lines[[1]]$players[[i]]$markets)){
        
        number[j] <-  tackles_lines[[1]]$players[[i]]$markets[[j]]$value
        odds[j] <-  tackles_lines[[1]]$players[[i]]$markets[[j]]$odds
        
      }
      
      player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                      team = rep(team,length(number)),
                                      number = number,
                                      odds = odds)
      
      
    }
    
    
    player_tackles_lines[[k]] <- rbindlist(player_lines)
    
    
    
    for(i in 1:length(shots_lines[[1]]$players)){
      
      number <- 0
      odds <- 0
      
      player <- shots_lines[[1]]$players[[i]]$name
      team <- shots_lines[[1]]$players[[i]]$team
      for(j in 1:length(shots_lines[[1]]$players[[i]]$markets)){
        
        number[j] <-  shots_lines[[1]]$players[[i]]$markets[[j]]$value
        odds[j] <-  shots_lines[[1]]$players[[i]]$markets[[j]]$odds
        
      }
      
      player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                      team = rep(team,length(number)),
                                      number = number,
                                      odds = odds)
      
      
    }
    
    
    player_shots_lines[[k]] <- rbindlist(player_lines)
    
    for(i in 1:length(sogs_lines[[1]]$players)){
      
      number <- 0
      odds <- 0
      
      player <- sogs_lines[[1]]$players[[i]]$name
      
      team <- sogs_lines[[1]]$players[[i]]$team
      for(j in 1:length(sogs_lines[[1]]$players[[i]]$markets)){
        
        number[j] <-  sogs_lines[[1]]$players[[i]]$markets[[j]]$value
        odds[j] <-  sogs_lines[[1]]$players[[i]]$markets[[j]]$odds
        
      }
      
      player_lines[[i]] <- data.frame(player = rep(player,length(number)),
                                      team = rep(team,length(number)),
                                      number = number,
                                      odds = odds)
      
    }
    
    
    player_sogs_lines[[k]] <- rbindlist(player_lines)
    
  }
  
  all_player_tackles_lines <- rbindlist(player_tackles_lines)
  
  all_player_shots_lines <- rbindlist(player_shots_lines)
  all_player_sogs_lines <- rbindlist(player_sogs_lines)
  all_player_goals_lines <- rbindlist(player_goals_lines)
  all_player_ast_lines <- rbindlist(player_ast_lines)
  
  all_player_tackles_lines$prob_tackle <- round(1/all_player_tackles_lines$odds,4)
  
  all_player_shots_lines$prob_shots <- round(1/all_player_shots_lines$odds,4)
  all_player_sogs_lines$prob_sogs <- round(1/all_player_sogs_lines$odds,4)
  all_player_goals_lines$prob_goals <- round(1/all_player_goals_lines$odds,4)
  all_player_ast_lines$prob_ast <- round(1/all_player_ast_lines$odds,4)
  
  if(league=="seriea" | league == "dfl"){
    player_prop_list <- list(all_player_tackles_lines,
                             all_player_shots_lines,
                             all_player_sogs_lines,
                             all_player_goals_lines,
                             all_player_ast_lines)
  }else{
    player_prop_list <- list(all_player_tackles_lines,
                             all_player_passes_lines,
                             all_player_shots_lines,
                             all_player_sogs_lines,
                             all_player_goals_lines,
                             all_player_ast_lines)
  }
  
  
}



fanduel_goalscorers <- function(sport_codes,sport_matt_id,leagues_df,matt_id){
  sport_matt_id <- 4
  fanduel_sports <- read_json("https://sbapi.co.sportsbook.fanduel.com/api/application-context?dataEntries=POPULAR_BETTING,QUICK_LINKS,AZ_BETTING,EVENT_TYPES,TEASER_COMPS&currencyCode=USD&exchangeLocale=en_US&language=en&regionCode=NAMERICA&_ak=FhMFpcPWXMeyZxOx")
  
  sport_ids <- names(fanduel_sports$eventTypes)
  fd_sports <- ""
  for(u in 1:length(sport_ids)){
    
    fd_sports[u] <- fanduel_sports$eventTypes[[u]]$name
    
  }
  
  fd_sport_df <- data.frame(sport_ids,fd_sports)
  
  fd_sport_id <- sport_codes$fanduel_code[sport_codes$mattid==sport_matt_id]
  
  fanduel_comps <- read_json(paste0("https://sbapi.co.sportsbook.fanduel.com/api/content-managed-page?betexRegion=GBR&capiJurisdiction=intl",
                                    "&currencyCode=USD&exchangeLocale=en_US&includePrices=true&includeRaceCards=false&includeSeo=true",
                                    "&language=en&regionCode=NAMERICA&timezone=America%2FNew_York&includeMarketBlurbs=true&_ak=FhMFpcPWXMeyZxOx&page=SPORT&eventTypeId=",
                                    fanduel_sports$eventTypes[[as.character(fd_sport_id)]]$eventTypeId))
  
  league_ids <- names(fanduel_comps$attachments$competitions)
  league_names <- ""
  for(r in 1:length(league_ids)){
    
    league_names[r] <- fanduel_comps$attachments$competitions[[league_ids[r]]]$name
    
    
    
  }
  
  fd_leagues <- data.frame(league_names, league_ids)
  
  
  
  games_json <- read_json(paste0("https://sbapi.co.sportsbook.fanduel.com/api/competition-page?betexRegion=GBR",
                                 "&capiJurisdiction=intl&currencyCode=USD&exchangeLocale=en_US&includeBadges=true&includeLayout=false",
                                 "&includePrices=true&language=en&regionCode=NAMERICA&_ak=FhMFpcPWXMeyZxOx",
                                 "&eventTypeId=",
                                 fanduel_sports$eventTypes[[as.character(fd_sport_id)]]$eventTypeId,
                                 "&competitionId=",
                                 as.character(leagues_df$fd_code_2[leagues_df$mattid==matt_id]),
                                 "&includeStaticCards=false"))
  game_ids <- names(games_json$attachments$events)
  
  
  gs_list <- vector("list",1)
  
  for(s in 1:length(game_ids)){
    game_json  <- read_json(paste0("https://sbapi.nj.sportsbook.fanduel.com/api/event-page?betexRegion=GBR&capiJurisdiction=intl&currencyCode=USD&exchangeLocale=en_US&includePrices=true&language=en&priceHistory=1&regionCode=NAMERICA&_ak=FhMFpcPWXMeyZxOx&eventId=",
                                   game_ids[s],
                                   "&tab=all"))
    
    
    
    
    
    if(length(game_json$attachments$markets)==0 | length(game_json$attachments$markets)==1){
      next()
    }else{
      
      marketid <- names(game_json$attachments$markets)
      
      for(bb in 1:length(marketid)){
        if(game_json$attachments$markets[[marketid[bb]]]$marketName=="Anytime Goalscorer"){
          test <- rbindlist(game_json$attachments$markets[[marketid[bb]]]$runners, fill = T) %>% unnest_wider(winRunnerOdds) 
          
          test <- test %>% unnest_wider(decimalOdds)
          test <- test[-which(is.na(test$decimalOdds)),]
          test$game_name <- game_json$attachments$events[[1]]$name
        }else{
          
        }
      }
      
      
    }
    if(exists("test")){
      
    }else{
      next()
    }
    gs_list[[s]] <- test
    rm(test)
  }
  
  gs <- rbindlist(gs_list, fill = T)
  
  gs <- gs[,c("runnerName","decimalOdds","game_name")]
  gs
}

goalie_adjustments <- function(lineup_stats, league_lines, fixtures){
  
  stat_game <- league_lines[[1]][[1]]
  
  test1_h <- (stat_game$goals_against[stat_game$expected_goals_against!=0&stat_game$is_home_team==TRUE]-stat_game$expected_goals_against[stat_game$expected_goals_against!=0&stat_game$is_home_team==TRUE])/90
  elo_difs_home <- stat_game$avg_elo_dif[stat_game$expected_goals_against!=0&stat_game$is_home_team==TRUE]
  
  g_exp_h <- summary(lm(test1_h~elo_difs_home))
  
  test1_a <- (stat_game$goals_against[stat_game$expected_goals_against!=0&stat_game$is_home_team==FALSE]-stat_game$expected_goals_against[stat_game$expected_goals_against!=0&stat_game$is_home_team==FALSE])/90
  elo_difs_away <- stat_game$avg_elo_dif[stat_game$expected_goals_against!=0&stat_game$is_home_team==FALSE]
  
  g_exp_a <- summary(lm(test1_a~elo_difs_away))
  
  elo_dif_1 <- fixtures$spi1-fixtures$spi2
  elo_dif_2 <- fixtures$spi2-fixtures$spi1
  
  gr_1 <- (g_exp_h$coefficients[[2]]*elo_dif_1)+g_exp_h$coefficients[[1]]
  gr_2 <- (g_exp_a$coefficients[[2]]*elo_dif_2)+g_exp_a$coefficients[[1]]
  
  
  goalie_stats <- lineup_stats[lineup_stats$positionRow==0,]
  goalie_rating <- ifelse(goalie_stats$minutes_xg>360,(goalie_stats$goals_against-goalie_stats$expected_goals_against)/goalie_stats$minutes_xg,
                          mean((stat_game$goals_against[stat_game$expected_goals_against!=0]-stat_game$expected_goals_against[stat_game$expected_goals_against!=0])/90, na.rm = T))
  goalie1 <- mean(c(goalie_rating[1],gr_1))*90
  goalie2 <- mean(c(goalie_rating[2],gr_2))*90
  
  goalies <- list(goalie1,goalie2)
}

sub_adjustments <- function(team1_subs,team2_subs,player_stats_game, team1,team2){
  
  home_xg_rate <- sum(player_stats_game$expected_xg[player_stats_game$team==team1&
                                                      !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xg_90min[player_stats_game$team==team1&
                                                                                                                             !is.na(player_stats_game$xg_90min)])*
    (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                     !is.na(player_stats_game$xg_90min)])-90)/900)
  home_xgot_rate <- sum(player_stats_game$expected_xgot[player_stats_game$team==team1&
                                                          !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xgot_90min[player_stats_game$team==team1&
                                                                                                                                   !is.na(player_stats_game$xg_90min)])*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  home_xa_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team1&
                                                      !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team1&
                                                                                                                             !is.na(player_stats_game$xg_90min)])*
                                                                                           (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                                                                                                            !is.na(player_stats_game$xg_90min)])-90)/900)
  home_passes_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team1&
                                                          !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team1&
                                                                                                                                 !is.na(player_stats_game$xg_90min)])*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  home_shots_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team1&
                                                         !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team1&
                                                                                                                                !is.na(player_stats_game$xg_90min)])*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  home_sogs_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team1&
                                                        !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team1&
                                                                                                                               !is.na(player_stats_game$xg_90min)])*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team1&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  
  away_xg_rate <- sum(player_stats_game$expected_xg[player_stats_game$team==team2&
                                                      !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xg_90min[player_stats_game$team==team2&
                                                                                                                             !is.na(player_stats_game$xg_90min)], na.rm = T)*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  away_xgot_rate <- sum(player_stats_game$expected_xgot[player_stats_game$team==team2&
                                                          !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xgot_90min[player_stats_game$team==team2&
                                                                                                                                   !is.na(player_stats_game$xg_90min)])*
                                                                                           (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                            !is.na(player_stats_game$xg_90min)])-90)/900)
  away_xa_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team2&
                                                      !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team2&
                                                                                                                             !is.na(player_stats_game$xg_90min)])*
                                                                                       (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                        !is.na(player_stats_game$xg_90min)])-90)/900)
  away_passes_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team2&
                                                          !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team2&
                                                                                                                                 !is.na(player_stats_game$xg_90min)])*
                                                                                           (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                            !is.na(player_stats_game$xg_90min)])-90)/900)
  away_shots_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team2&
                                                         !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team2&
                                                                                                                                !is.na(player_stats_game$xg_90min)])*
                                                                                          (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                           !is.na(player_stats_game$xg_90min)])-90)/900)
  away_sogs_rate <- sum(player_stats_game$expected_xa[player_stats_game$team==team2&
                                                        !is.na(player_stats_game$xg_90min)])/(sum(player_stats_game$xa_90min[player_stats_game$team==team2&
                                                                                                                               !is.na(player_stats_game$xg_90min)])*
                                                                                         (sum(player_stats_game$exp_min[player_stats_game$team==team2&
                                                                                                                          !is.na(player_stats_game$xg_90min)])-90)/900)
  
  subs_out <- player_stats_game[which(player_stats_game$exp_min<90),]
  
  subs_out_home_minutes <- subs_out$exp_min[subs_out$isHomeTeam==TRUE]
  subs_out_away_minutes <- subs_out$exp_min[subs_out$isHomeTeam==FALSE]
  
  subs_out_home_minutes <- subs_out_home_minutes[order(subs_out_home_minutes)]
  subs_out_away_minutes <- subs_out_away_minutes[order(subs_out_away_minutes)]
  
  subs1 <- team1_subs[order(-team1_subs$minutes_xg),]
  subs2 <- team2_subs[order(-team2_subs$minutes_xg),]
  
  subs1 <- subs1[c(1:4),]
  subs2 <- subs2[c(1:4),]

  subs1$expected_sogs <- subs1$sog_90min*((90-subs_out_home_minutes)/90)*home_sogs_rate
  subs1$expected_shots <- subs1$shots_90min*((90-subs_out_home_minutes)/90)*home_shots_rate
  subs1$expected_xg <- subs1$xg_90min*((90-subs_out_home_minutes)/90)*home_xg_rate
  subs1$expected_xgot <-subs1$xgot_90min*((90-subs_out_home_minutes)/90)*home_xgot_rate
  subs1$expected_xa <- subs1$xa_90min*((90-subs_out_home_minutes)/90)*home_xa_rate
  subs1$exp_min <- (90-subs_out_home_minutes)
  subs1$team <- team1
  subs1$match_id <-  unique(player_stats_game$match_id)
  subs1$confirmed <- unique(player_stats_game$confirmed)
  subs2$expected_sogs <- subs2$sog_90min*((90-subs_out_away_minutes)/90)*home_sogs_rate
  subs2$expected_shots <- subs2$shots_90min*((90-subs_out_away_minutes)/90)*home_shots_rate
  subs2$expected_xg <- subs2$xg_90min*((90-subs_out_away_minutes)/90)*home_xg_rate
  subs2$expected_xgot <- subs2$xgot_90min*((90-subs_out_away_minutes)/90)*home_xgot_rate
  subs2$expected_xa <- subs2$xa_90min*((90-subs_out_away_minutes)/90)*home_xa_rate
  subs2$exp_min <- (90-subs_out_away_minutes)
  subs2$team <- team2
  subs2$match_id <- unique(player_stats_game$match_id)
  subs2$confirmed <- unique(player_stats_game$confirmed)
  
  subs1$expected_sogs[is.nan(subs1$expected_sogs)] <- 0
  subs1$expected_shots[is.nan(subs1$expected_shots)] <- 0
  subs1$expected_xg[is.nan(subs1$expected_xg)] <- 0
  subs1$expected_xgot[is.nan(subs1$expected_xgot)] <- 0
  subs1$expected_xa[is.nan(subs1$expected_xa)] <- 0
  
  subs2$expected_sogs[is.nan(subs2$expected_sogs)] <- 0
  subs2$expected_shots[is.nan(subs2$expected_shots)] <- 0
  subs2$expected_xg[is.nan(subs2$expected_xg)] <- 0
  subs2$expected_xgot[is.nan(subs2$expected_xgot)] <- 0
  subs2$expected_xa[is.nan(subs2$expected_xa)] <- 0
  
  player_stats_game <- rbindlist(list(player_stats_game,subs1,subs2), fill = T)
  player_stats_game
}



