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
  team_df <- team_df[-which(duplicated(team_df)),]
  team_df
}



team_df <- get_team_codes(wspc)


  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Data")))
  
  days <- c("20230215")
  
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
         unique(game_data$league)!="Europa Conference League Final Stage"&unique(game_data$league)!="AFC Cup Final Stage"&
         unique(game_data$league)!="CAF Champions League Grp. E"&unique(game_data$league)!="Copa Sudamericana Qualification"&unique(game_data$league)!="Recopa Sudamericana"&
         unique(game_data$league)!="CAF Champions League Grp. A"&unique(game_data$league)!="CAF Champions League Grp. C"&unique(game_data$league)!="CAF Champions League Grp. F"&
         unique(game_data$league)!="CAF Champions League Grp. B"&unique(game_data$league)!="CAF Champions League Grp. D"&unique(game_data$league)!="CAF Champions League Grp. G"&
         unique(game_data$league)!="CAF Champions League Grp. H"&unique(game_data$league)!="CAF Champions League Grp. I"&unique(game_data$league)!="CAF Champions League Grp. J"&
         unique(game_data$league)!="CAF Champions League Grp. K"&unique(game_data$league)!="CAF Champions League Grp. L"&unique(game_data$league)!="CAF Champions League Final Stage"&
         unique(game_data$league)!="CAF Confederation Cup Grp. A"&unique(game_data$league)!="CAF Confederation Cup Grp. C"&unique(game_data$league)!="CAF Confederation Cup Grp. F"&
         unique(game_data$league)!="CAF Confederation Cup Grp. B"&unique(game_data$league)!="CAF Confederation Cup Grp. D"&unique(game_data$league)!="CAF Confederation Cup Grp. G"&
         unique(game_data$league)!="CAF Confederation Cup Grp. H"&unique(game_data$league)!="CAF Confederation Cup Grp. I"&unique(game_data$league)!="CAF Confederation Cup Grp. J"&
         unique(game_data$league)!="CAF Confederation Cup Grp. K"&unique(game_data$league)!="CAF Confederation Cup Grp. L"&unique(game_data$league)!="CAF Confederation Cup Final Stage"&
         unique(game_data$league)!="Women's Champions League Grp. E"&unique(game_data$league)!="Women's Champions League Final Stage"&
         unique(game_data$league)!="Women's Champions League Grp. A"&unique(game_data$league)!="Women's Champions League Grp. C"&unique(game_data$league)!="Women's Champions League Grp. F"&
         unique(game_data$league)!="Women's Champions League Grp. B"&unique(game_data$league)!="Women's Champions League Grp. D"&unique(game_data$league)!="Women's Champions League Grp. G"&
         unique(game_data$league)!="Women's Champions League Grp. H"&unique(game_data$league)!="Sidemen Charity Match"){
        
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
    print(paste0("Done with ",days[1:e]))
  }
  

