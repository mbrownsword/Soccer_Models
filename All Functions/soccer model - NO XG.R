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
library(lubridate)
library(gh)
library(git2r)
l <- fotmob_get_league_ids(cached = FALSE)
#View(l)
wspc <- "C:/Users/CORSAIR GAMING/Documents/"

setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All Functions")))
source("functions_for_the_rest_of_us.R")

setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
 ###### Team Codes ######
sport_codes <- read_xlsx("book_codes_all.xlsx", sheet = "overall")
sport_matt_id <- 4
leagues_df <- data.frame(read_xlsx("book_codes_all.xlsx",sheet = sport_codes$dk_code[sport_codes$mattid==sport_matt_id]), stringsAsFactors = F)
leagues_df[is.na(leagues_df)] <- ""	
leagues_df[length(leagues_df$mattid)+1,] <- ""
leagues_df$b_code_1[length(leagues_df$mattid)] <- "france"
leagues_df$b_code_2[length(leagues_df$mattid)] <- "coupe_de_france"
leagues_df$sport[length(leagues_df$mattid)] <- "soccer"
leagues_df$mattid[length(leagues_df$mattid)] <- length(leagues_df$mattid)

leagues_df[length(leagues_df$mattid)+1,] <- ""
leagues_df$b_code_1[length(leagues_df$mattid)] <- "japan"
leagues_df$b_code_2[length(leagues_df$mattid)] <- "j1-league"

leagues_df$sport[length(leagues_df$mattid)] <- "soccer"
leagues_df$mattid[length(leagues_df$mattid)] <- length(leagues_df$mattid)

leagues_df$b_code_2[8] <- "a-league"

team_df <-get_team_codes(wspc)
team_df2 <- get_team_codes2(wspc)
team_df3 <- get_team_codes3(wspc)


date <- Sys.Date()+1

id <- c(223)

league_id <- c(56)


prop_indicator <- "none"

zz <- 1
yy <- 1


special  <- F

####### LOOP ########

for(zz in 1:length(id)){
  lines <- running_lines(leagues_df, league_id[zz], team_df2)
  setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
  fixtures <- get_fixture_ratings(id[zz],date,team_df)
  manual_add_1 <- vector("list",length(fixtures$round))
  manual_add_2 <- vector("list",length(fixtures$round))
  eliminate_1 <- vector("list",length(fixtures$round))
  eliminate_2 <- vector("list",length(fixtures$round))
  for(yy in 1){
    
    
    team1_squad <- team_squad(fixtures$home$id[yy])
    team1_squad$id <- unlist(team1_squad$id)
    team1_squad$my.var <- unlist(team1_squad$my.var)
    team1_squad$name <- unlist(team1_squad$name)
    team1_squad$ccode <- unlist(team1_squad$ccode)
    team1_squad$cname <- unlist(team1_squad$cname)
    team1_squad <- data.frame(team1_squad)
    if(length(manual_add_1[[yy]])==0){
      
    }else{
      team1_squad[(nrow(team1_squad) + 1),] <- manual_add_1[[yy]]
    }
    if(length(eliminate_1[[yy]])==0){
      
    }else{
      team1_squad <- team1_squad[-(team1_squad$name %in% eliminate_1[[yy]][3] ),]
    }
    
    team1_player_data <- team_player_stats(team1_squad)
    team1_year_data <- team1_player_data[[2]]
    team1_year_data <- team1_year_data[as.numeric(as.character(team1_year_data$year))>=2022,]
    team1_player_data <- team1_player_data[[1]]
    
    team2_squad <- team_squad(fixtures$away$id[yy])
    team2_squad$id <- unlist(team2_squad$id)
    team2_squad$my.var <- unlist(team2_squad$my.var)
    team2_squad$name <- unlist(team2_squad$name)
    team2_squad$ccode <- unlist(team2_squad$ccode)
    team2_squad$cname <- unlist(team2_squad$cname)
    team2_squad <- data.frame(team2_squad)
    if(length(manual_add_2[[yy]])==0){
      
    }else{
      team2_squad[(length(team2_squad$my.var)+1),] <- manual_add_2[[yy]]
    }
    if(length(eliminate_2[[yy]])==0){
      
    }else{
      team2_squad <- team2_squad[-(team2_squad$name %in% eliminate_2[[yy]][3] ),]
    }
    team2_player_data <- team_player_stats(team2_squad)
    team2_year_data <- team2_player_data[[2]]
    team2_year_data <- team2_year_data[as.numeric(as.character(team2_year_data$year))>=2022,]
    team2_player_data <- team2_player_data[[1]]
    
    
    league <- paste0(l$ccode[l$id==id[zz]]," ",l$name[l$id==id[zz]])
    
    z1 <- read_json(paste0("https://www.fotmob.com/api/teams?id=",fixtures$home$id[yy]))
    
    l_z1 <- paste0(z1$history$tables$current[[1]]$link[[1]]$ccode[[1]]," ",z1$history$tables$current[[1]]$link[[1]]$name[[1]])
    
    z2 <- read_json(paste0("https://www.fotmob.com/api/teams?id=",fixtures$away$id[yy]))
    
    l_z2 <- paste0(z2$history$tables$current[[1]]$link[[1]]$ccode[[1]]," ",z2$history$tables$current[[1]]$link[[1]]$name[[1]])
    fixtures$league_1 <- "" 
    fixtures$league_2 <- ""
    fixtures$league_1[yy] <- l_z1 
    fixtures$league_2[yy] <- l_z2
    u_l <-  unlist(unique(c(l_z1,l_z2))[order(unique(c(l_z1,l_z2)))])
    
    l2 <- c(league,u_l)
    league <- unique(l2)
    league <- trimws(league)
    
    if(length(which(league=="NED KNVB Cup"))==0){
      
    }else{
      league <- league[-which(league=="NED KNVB Cup")] 
    }
    if(length(which(league=="The Atlantic Cup"))==0){
      
    }else{
      league <- league[-which(league=="The Atlantic Cup")] 
    }
    if(length(which(league=="INT Champions League"))==0){
      
    }else{
      league <- league[-which(league=="INT Champions League")]
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
      league_files <- list.files()
      cl_files <- league_files[which(grepl("INT Champions League", league_files))]
      league <- c(league,cl_files)
    }
    if(length(which(league=="INT Europa League"))==0){
      
    }else{
      league <- league[-which(league=="INT Europa League")]
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
      league_files <- list.files()
      cl_files <- league_files[which(grepl("INT Europa League", league_files))]
      league <- c(league,cl_files)
    }
    if(length(which(league=="INT Europa Conference League"))==0){
      
    }else{
      league <- league[-which(league=="INT Europa Conference League")]
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
      league_files <- list.files()
      cl_files <- league_files[which(grepl("INT Europa Conference League", league_files))]
      league <- c(league,cl_files)
    }
    
    if(league!="MEX Liga MX"){
      setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling/All League Data")))
      league_files <- list.files()
      league <- league[which(league %in% league_files)]
    }
    
    
    setwd(paste0(paste0(wspc,"Fotmob Soccer Modelling")))
    league_lines <- league_analysis(league)
    
    
    team1 <- fixtures$home$name[yy]
    team2 <- fixtures$away$name[yy]
    
    manager1 <- unlist(team1_squad$name[team1_squad$my.var=="coach"])
    manager1_id <- unlist(team1_squad$id[team1_squad$my.var=="coach"]) 
    manager2 <- unlist(team2_squad$name[team2_squad$my.var=="coach"]) 
    manager2_id <- unlist(team2_squad$id[team2_squad$my.var=="coach"]) 
    
    manager_df <- manager_analysis(manager_ids = c(manager1_id, manager2_id))
    
    if(length(which(duplicated(manager_df[,c(1:2)])))==0){
      
    }else{
      manager_df <- manager_df[-which(duplicated(manager_df[,c(1:2)])),]
    }
    
    
    manager_list_e <- manager_expected(league_lines,manager_df)
    
    manager_df_e <- rbindlist(manager_list_e)
    
    m_pr <- 0
    m_shr <- 0
    m_sogr <- 0
    m_xgr <- 0
    m_xgotr <- 0
    m_xar <- 0
    
    
    m_pr[1] <- mean(c(manager_df$m_avg_passes[1]/manager_df_e$m_passes[1],
                      manager_df$o_avg_passes[2]/manager_df_e$om_passes[2]))
    m_shr[1] <- mean(c(manager_df$m_avg_shots[1]/manager_df_e$m_shots[1],
                       manager_df$o_avg_shots[2]/manager_df_e$om_shots[2]))
    m_sogr[1] <- mean(c(manager_df$m_avg_sogs[1]/manager_df_e$m_sogs[1],
                        manager_df$o_avg_sogs[2]/manager_df_e$om_sogs[2]))
    m_xgr[1] <- mean(c(manager_df$m_avg_xg[1]/manager_df_e$m_xg[1],
                       manager_df$o_avg_xg[2]/manager_df_e$om_xg[2]))
    m_xgotr[1] <- mean(c(manager_df$m_avg_xgot[1]/manager_df_e$m_xgot[1],
                         manager_df$o_avg_xgot[2]/manager_df_e$om_xgot[2]))
    m_xar[1] <- mean(c(manager_df$m_avg_xa[1]/manager_df_e$m_xa[1],
                       manager_df$o_avg_xa[2]/manager_df_e$om_xa[2]))
    
    m_pr[2] <- mean(c(manager_df$m_avg_passes[2]/manager_df_e$m_passes[2],
                      manager_df$o_avg_passes[1]/manager_df_e$om_passes[1]))
    m_shr[2] <- mean(c(manager_df$m_avg_shots[2]/manager_df_e$m_shots[2],
                       manager_df$o_avg_shots[1]/manager_df_e$om_shots[1]))
    m_sogr[2] <- mean(c(manager_df$m_avg_sogs[2]/manager_df_e$m_sogs[2],
                        manager_df$o_avg_sogs[1]/manager_df_e$om_sogs[1]))
    m_xgr[2] <- mean(c(manager_df$m_avg_xg[2]/manager_df_e$m_xg[2],
                       manager_df$o_avg_xg[1]/manager_df_e$om_xg[1]))
    m_xgotr[2] <- mean(c(manager_df$m_avg_xgot[2]/manager_df_e$m_xgot[2],
                         manager_df$o_avg_xgot[1]/manager_df_e$om_xgot[1]))
    m_xar[2] <- mean(c(manager_df$m_avg_xa[2]/manager_df_e$m_xa[2],
                       manager_df$o_avg_xa[1]/manager_df_e$om_xa[1]))
    
    t <- read_json(paste0("https://www.fotmob.com/api/matchDetails?matchId=",
                          fixtures$id[yy],
                          "&ccode3=USA&timezone=America%2FLos_Angeles&refresh=true&includeBuzzTab=false&acceptLanguage=en-US"))
    
    if(length(t$content$lineup)==0){
      
    }else{
      if(length(t$content$lineup$lineup[[1]]$players)==0){
        
          lineup_1 <- predicted_lineup(team1_player_data,manager_df[1,],fixtures[yy,],team1,team2,team1, team1_year_data)
          lineup_2 <- predicted_lineup(team2_player_data,manager_df[2,],fixtures[yy,],team1,team2,team2,team2_year_data)
          
          lineup_1[[1]]$team <- team1
          lineup_1[[1]]$is_home_team <- TRUE
          lineup_1[[1]]$isHomeTeam <- TRUE
          lineup_2[[1]]$team <- team2
          lineup_2[[1]]$is_home_team <- FALSE
          lineup_2[[1]]$isHomeTeam <- FALSE
          
          lineup_stats <- rbind(lineup_1[[1]],lineup_2[[1]])
          lineup_stats$confirmed <- "matt predicted"
          lineup_stats$pageUrl <- lineup_stats$page_url
          lineup_stats$pageUrl2 <- lineup_stats$page_url_2
          lineup_stats$positionRow <- lineup_stats$position_row
          lineup_stats$match_id <- fixtures$id[yy]
          
          lineup_1[[2]]$team <- team1
          lineup_2[[2]]$team <- team2
          
          team1_subs <- lineup_1[[2]]
          team2_subs <- lineup_2[[2]]
          
          team1_subs$confirmed <- "matt predicted"
          team1_subs$pageUrl <- team1_subs$page_url
          team1_subs$pageUrl2 <- team1_subs$page_url_2
          team1_subs$positionRow <- team1_subs$position_row
          team1_subs$is_home_team <- TRUE
          team1_subs$isHomeTeam <- TRUE
          team1_subs$match_id <- fixtures$id[yy]
          
          team2_subs$confirmed <- "matt predicted"
          team2_subs$pageUrl <- team2_subs$page_url
          team2_subs$pageUrl2 <- team2_subs$page_url_2
          team2_subs$positionRow <- team2_subs$position_row
          team2_subs$is_home_team <- FALSE
          team2_subs$isHomeTeam <- FALSE
          team2_subs$match_id <- fixtures$id[yy]
          if(is.element("role",colnames(team1_subs))){
            team1_subs <- team1_subs[team1_subs$role!="Keeper",]
            team2_subs <- team2_subs[team2_subs$role!="Keeper",]
          }else{
            team1_subs <- team1_subs[team1_subs$expected_goals_against==0,]
            team2_subs <- team2_subs[team2_subs$expected_goals_against==0,]
          }
      }else{
        full_lineups <- team_lineup(id, date, fixtures[yy,], team1_player_data, team2_player_data)
        
        lineup_df <- full_lineups[[1]]
        team1_subs <- full_lineups[[2]]
        team2_subs <- full_lineups[[3]]
        
        if(is.element("role",colnames(team1_subs))){
          team1_subs <- team1_subs[team1_subs$role!="Keeper",]
          team2_subs <- team2_subs[team2_subs$role!="Keeper",]
        }else{
          team1_subs <- team1_subs[team1_subs$expected_goals_against==0,]
          team2_subs <- team2_subs[team2_subs$expected_goals_against==0,]
        }
        
        
        if(special==T){
          lineup_df$positionRow[lineup_df$pageUrl2=="/players/675088/rodri"] <- 1
        }
        
        
        
        lineup_game <- lineup_df[lineup_df$team==fixtures$home$name[yy] | lineup_df$team==fixtures$away$name[yy],]
        
        lineup_game$row_id <- c(1:length(lineup_game$id))
        
        lineup_stats <- merge(lineup_game, rbindlist(list(team1_player_data,team2_player_data), fill = T), by.x = c("pageUrl",
                                                                                                                    "positionRow"), by.y = c("page_url","position_row"),
                              all.x = TRUE)
        lineup_stats <- data.frame(lineup_stats)
        if(length(which(is.na(lineup_stats$pass_90min)))==0){
          for(i in 1:length(lineup_stats$pageUrl)){
            if(lineup_stats$minutes[i]==0){
              col_select <- c(which(colnames(lineup_stats)=="page_url_2"):which(colnames(lineup_stats)=="xa_rate_90_min"))
              replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$page_url==lineup_stats$pageUrl[i],c(3:30)]
              if(length(replace_data$page_url_2)==0){
                print(paste0(lineup_stats$pageUrl2[i]," is making his fucking debut."))
                replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$position_row==lineup_stats$positionRow[i],c(3:30)]
                replace_data <- replace_data[replace_data$avg_rating==min(replace_data$avg_rating[replace_data$minutes_xg>180]),]
                lineup_stats[i,c(col_select[2:length(col_select)])] <- replace_data[1,c(2:27)]
                lineup_stats[i,c(col_select[1])] <- lineup_stats$pageUrl2[i]
                next()
              }
              lineup_stats[i,c(col_select)] <- replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),]
              print(paste0(replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),1]," has only played ",replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),2]," tracked minutes via Fotmob"))
            }else{
              next()
            }
          }
        }else{
          for(i in 1:length(lineup_stats$pageUrl)){
            if(is.na(lineup_stats$pass_90min[i])){
              col_select <- c(which(colnames(lineup_stats)=="page_url_2"):which(colnames(lineup_stats)=="xa_rate_90_min"))
              replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$page_url==lineup_stats$pageUrl[i],c(3:30)]
              if(length(replace_data$page_url_2)==0){
                print(paste0(lineup_stats$pageUrl2[i]," is making his fucking debut."))
                replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$position_row==lineup_stats$positionRow[i],c(3:30)]
                replace_data <- replace_data[replace_data$avg_rating==min(replace_data$avg_rating[replace_data$minutes_xg>180]),]
                lineup_stats[i,c(col_select[2:length(col_select)])] <- replace_data[1,c(2:27)]
                lineup_stats[i,c(col_select[1])] <- lineup_stats$pageUrl2[i]
                next()
              }
              lineup_stats[i,c(col_select)] <- replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),]
              print(paste0(replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),1]," has only played ",replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),2]," tracked minutes via Fotmob"))
            }else{
              next()
            }
          }
          for(i in 1:length(lineup_stats$pageUrl)){
            if(lineup_stats$minutes[i]<45){
              col_select <- c(which(colnames(lineup_stats)=="page_url_2"):which(colnames(lineup_stats)=="xa_rate_90_min"))
              replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$page_url==lineup_stats$pageUrl[i],c(3:30)]
              replace_data <- replace_data[replace_data$minutes>=45,]
              if(length(replace_data$page_url_2)==0){
                print(paste0(lineup_stats$pageUrl2[i]," is making his fucking debut."))
                if(lineup_stats$positionRow[i]==0){
                  replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$expected_goals_against>0&
                                                                                                               data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$position_row==lineup_stats$positionRow[i],c(3:30)] 
                }else{
                  replace_data <- data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))[data.frame(rbindlist(list(team1_player_data,team2_player_data), fill = T))$position_row==lineup_stats$positionRow[i],c(3:30)]
                }
                replace_data <- replace_data[replace_data$avg_rating==min(replace_data$avg_rating[replace_data$minutes_xg>180]),]
                lineup_stats[i,c(col_select[2:length(col_select)])] <- replace_data[1,c(2:27)]
                lineup_stats[i,c(col_select[1])] <- lineup_stats$pageUrl2[i]
                next()
              }
              lineup_stats[i,c(col_select)] <- replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),]
              print(paste0(replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),1]," has only played ",replace_data[which(replace_data$minutes==max(replace_data$minutes)[1]),2]," tracked minutes via Fotmob"))
            }else{
              next()
            }
          }
          
          
        }
        
        lineup_stats <- lineup_stats[order(lineup_stats$row_id),]
      }
    }
    
    
    
    
    
    
    
    
    formation1 <- paste0(unlist(data.frame(table(lineup_stats$positionRow[lineup_stats$team==team1]))$Freq[-1]),collapse = "-")
    positions1 <- unique(lineup_stats$positionRow[lineup_stats$positionRow!=0&lineup_stats$team==team1])[order(unique(lineup_stats$positionRow[lineup_stats$positionRow!=0&lineup_stats$team==team1]))]
    formation2 <- paste0(unlist(data.frame(table(lineup_stats$positionRow[lineup_stats$team==team2]))$Freq[-1]),collapse = "-")
    positions2 <- unique(lineup_stats$positionRow[lineup_stats$positionRow!=0&lineup_stats$team==team2])[order(unique(lineup_stats$positionRow[lineup_stats$positionRow!=0&lineup_stats$team==team2]))]
    
    formation_matchup <- paste0(formation1," v. ",formation2)
    
    
    
    formation_lines <- formation_analysis(formation_matchup)
    
    
    
    teams_comp <- team_analysis(teams = c(team1,team2))
    
    h_pass_rate_1 <- teams_comp$h_avg_passes[1]/(teams_comp$h_avg_passes[1]+teams_comp$a_avg_passes[1])
    oh_pass_rate_1 <- teams_comp$oh_avg_passes[1]/(teams_comp$oh_avg_passes[1]+teams_comp$oa_avg_passes[1])
    h_shot_rate_1 <- teams_comp$h_avg_shots[1]/(teams_comp$h_avg_shots[1]+teams_comp$a_avg_shots[1])
    oh_shot_rate_1 <- teams_comp$oh_avg_shots[1]/(teams_comp$oh_avg_shots[1]+teams_comp$oa_avg_shots[1])
    h_sog_rate_1 <- teams_comp$h_avg_sogs[1]/(teams_comp$h_avg_sogs[1]+teams_comp$a_avg_sogs[1])
    oh_sog_rate_1 <- teams_comp$oh_avg_sogs[1]/(teams_comp$oh_avg_sogs[1]+teams_comp$oa_avg_sogs[1])
    h_xg_rate_1 <- teams_comp$h_avg_xg[1]/(teams_comp$h_avg_xg[1]+teams_comp$a_avg_xg[1])
    oh_xg_rate_1 <- teams_comp$oh_avg_xg[1]/(teams_comp$oh_avg_xg[1]+teams_comp$oa_avg_xg[1])
    h_xa_rate_1 <- teams_comp$h_avg_xa[1]/(teams_comp$h_avg_xa[1]+teams_comp$a_avg_xa[1])
    oh_xa_rate_1 <- teams_comp$oh_avg_xa[1]/(teams_comp$oh_avg_xa[1]+teams_comp$oa_avg_xa[1])
    h_xgot_rate_1 <- teams_comp$h_avg_xgot[1]/(teams_comp$h_avg_xgot[1]+teams_comp$a_avg_xgot[1])
    oh_xgot_rate_1 <- teams_comp$oh_avg_xgot[1]/(teams_comp$oh_avg_xgot[1]+teams_comp$oa_avg_xgot[1])
    
    h_pass_rate_2 <- teams_comp$h_avg_passes[2]/(teams_comp$h_avg_passes[2]+teams_comp$a_avg_passes[2])
    oh_pass_rate_2 <- teams_comp$oh_avg_passes[2]/(teams_comp$oh_avg_passes[2]+teams_comp$oa_avg_passes[2])
    h_shot_rate_2 <- teams_comp$h_avg_shots[2]/(teams_comp$h_avg_shots[2]+teams_comp$a_avg_shots[2])
    oh_shot_rate_2 <- teams_comp$oh_avg_shots[2]/(teams_comp$oh_avg_shots[2]+teams_comp$oa_avg_shots[2])
    h_sog_rate_2 <- teams_comp$h_avg_sogs[2]/(teams_comp$h_avg_sogs[2]+teams_comp$a_avg_sogs[2])
    oh_sog_rate_2 <- teams_comp$oh_avg_sogs[2]/(teams_comp$oh_avg_sogs[2]+teams_comp$oa_avg_sogs[2])
    h_xg_rate_2 <- teams_comp$h_avg_xg[2]/(teams_comp$h_avg_xg[2]+teams_comp$a_avg_xg[2])
    oh_xg_rate_2 <- teams_comp$oh_avg_xg[2]/(teams_comp$oh_avg_xg[2]+teams_comp$oa_avg_xg[2])
    h_xa_rate_2 <- teams_comp$h_avg_xa[2]/(teams_comp$h_avg_xa[2]+teams_comp$a_avg_xa[2])
    oh_xa_rate_2 <- teams_comp$oh_avg_xa[2]/(teams_comp$oh_avg_xa[2]+teams_comp$oa_avg_xa[2])
    h_xgot_rate_2 <- teams_comp$h_avg_xgot[2]/(teams_comp$h_avg_xgot[2]+teams_comp$a_avg_xgot[2])
    oh_xgot_rate_2 <- teams_comp$oh_avg_xgot[2]/(teams_comp$oh_avg_xgot[2]+teams_comp$oa_avg_xgot[2])
    
    a_pass_rate_1 <- teams_comp$a_avg_passes[1]/(teams_comp$h_avg_passes[1]+teams_comp$a_avg_passes[1])
    oa_pass_rate_1 <- teams_comp$oa_avg_passes[1]/(teams_comp$oh_avg_passes[1]+teams_comp$oa_avg_passes[1])
    a_shot_rate_1 <- teams_comp$a_avg_shots[1]/(teams_comp$h_avg_shots[1]+teams_comp$a_avg_shots[1])
    oa_shot_rate_1 <- teams_comp$oa_avg_shots[1]/(teams_comp$oh_avg_shots[1]+teams_comp$oa_avg_shots[1])
    a_sog_rate_1 <- teams_comp$a_avg_sogs[1]/(teams_comp$h_avg_sogs[1]+teams_comp$a_avg_sogs[1])
    oa_sog_rate_1 <- teams_comp$oa_avg_sogs[1]/(teams_comp$oh_avg_sogs[1]+teams_comp$oa_avg_sogs[1])
    a_xg_rate_1 <- teams_comp$a_avg_xg[1]/(teams_comp$h_avg_xg[1]+teams_comp$a_avg_xg[1])
    oa_xg_rate_1 <- teams_comp$oa_avg_xg[1]/(teams_comp$oh_avg_xg[1]+teams_comp$oa_avg_xg[1])
    a_xa_rate_1 <- teams_comp$a_avg_xa[1]/(teams_comp$h_avg_xa[1]+teams_comp$a_avg_xa[1])
    oa_xa_rate_1 <- teams_comp$oa_avg_xa[1]/(teams_comp$oh_avg_xa[1]+teams_comp$oa_avg_xa[1])
    a_xgot_rate_1 <- teams_comp$a_avg_xgot[1]/(teams_comp$h_avg_xgot[1]+teams_comp$a_avg_xgot[1])
    oa_xgot_rate_1 <- teams_comp$oa_avg_xgot[1]/(teams_comp$oh_avg_xgot[1]+teams_comp$oa_avg_xgot[1])
    
    a_pass_rate_2 <- teams_comp$a_avg_passes[2]/(teams_comp$h_avg_passes[2]+teams_comp$a_avg_passes[2])
    oa_pass_rate_2 <- teams_comp$oa_avg_passes[2]/(teams_comp$oh_avg_passes[2]+teams_comp$oa_avg_passes[2])
    a_shot_rate_2 <- teams_comp$a_avg_shots[2]/(teams_comp$h_avg_shots[2]+teams_comp$a_avg_shots[2])
    oa_shot_rate_2 <- teams_comp$oa_avg_shots[2]/(teams_comp$oh_avg_shots[2]+teams_comp$oa_avg_shots[2])
    a_sog_rate_2 <- teams_comp$a_avg_sogs[2]/(teams_comp$h_avg_sogs[2]+teams_comp$a_avg_sogs[2])
    oa_sog_rate_2 <- teams_comp$oa_avg_sogs[2]/(teams_comp$oh_avg_sogs[2]+teams_comp$oa_avg_sogs[2])
    a_xg_rate_2 <- teams_comp$a_avg_xg[2]/(teams_comp$h_avg_xg[2]+teams_comp$a_avg_xg[2])
    oa_xg_rate_2 <- teams_comp$oa_avg_xg[2]/(teams_comp$oh_avg_xg[2]+teams_comp$oa_avg_xg[2])
    a_xa_rate_2 <- teams_comp$a_avg_xa[2]/(teams_comp$h_avg_xa[2]+teams_comp$a_avg_xa[2])
    oa_xa_rate_2 <- teams_comp$oa_avg_xa[2]/(teams_comp$oh_avg_xa[2]+teams_comp$oa_avg_xa[2])
    a_xgot_rate_2 <- teams_comp$a_avg_xgot[2]/(teams_comp$h_avg_xgot[2]+teams_comp$a_avg_xgot[2])
    oa_xgot_rate_2 <- teams_comp$oa_avg_xgot[2]/(teams_comp$oh_avg_xgot[2]+teams_comp$oa_avg_xgot[2])
    
    h_pass_rate <- mean(c(h_pass_rate_1,oh_pass_rate_2))/sum(mean(c(h_pass_rate_1,oh_pass_rate_2)),
                                                             mean(c(h_pass_rate_2,oh_pass_rate_1)))
    a_pass_rate <- mean(c(a_pass_rate_2,oa_pass_rate_1))/sum(mean(c(a_pass_rate_1,oa_pass_rate_2)),
                                                             mean(c(a_pass_rate_2,oa_pass_rate_1)))
    h_shot_rate <- mean(c(h_shot_rate_1,oh_shot_rate_2))/sum(mean(c(h_shot_rate_1,oh_shot_rate_2)),
                                                             mean(c(h_shot_rate_2,oh_shot_rate_1)))
    a_shot_rate <- mean(c(a_shot_rate_2,oa_shot_rate_1))/sum(mean(c(a_shot_rate_1,oa_shot_rate_2)),
                                                             mean(c(a_shot_rate_2,oa_shot_rate_1)))
    h_sog_rate <- mean(c(h_sog_rate_1,oh_sog_rate_2))/sum(mean(c(h_sog_rate_1,oh_sog_rate_2)),
                                                          mean(c(h_sog_rate_2,oh_sog_rate_1)))
    a_sog_rate <- mean(c(a_sog_rate_2,oa_sog_rate_1))/sum(mean(c(a_sog_rate_1,oa_sog_rate_2)),
                                                          mean(c(a_sog_rate_2,oa_sog_rate_1)))
    h_xg_rate <- mean(c(h_xg_rate_1,oh_xg_rate_2))/sum(mean(c(h_xg_rate_1,oh_xg_rate_2)),
                                                       mean(c(h_xg_rate_2,oh_xg_rate_1)))
    a_xg_rate <- mean(c(a_xg_rate_2,oa_xg_rate_1))/sum(mean(c(a_xg_rate_1,oa_xg_rate_2)),
                                                       mean(c(a_xg_rate_2,oa_xg_rate_1)))
    h_xa_rate <- mean(c(h_xa_rate_1,oh_xa_rate_2))/sum(mean(c(h_xa_rate_1,oh_xa_rate_2)),
                                                       mean(c(h_xa_rate_2,oh_xa_rate_1)))
    a_xa_rate <- mean(c(a_xa_rate_2,oa_xa_rate_1))/sum(mean(c(a_xa_rate_1,oa_xa_rate_2)),
                                                       mean(c(a_xa_rate_2,oa_xa_rate_1)))
    h_xgot_rate <- mean(c(h_xgot_rate_1,oh_xgot_rate_2))/sum(mean(c(h_xgot_rate_1,oh_xgot_rate_2)),
                                                             mean(c(h_xgot_rate_2,oh_xgot_rate_1)))
    a_xgot_rate <- mean(c(a_xgot_rate_2,oa_xgot_rate_1))/sum(mean(c(a_xgot_rate_1,oa_xgot_rate_2)),
                                                             mean(c(a_xgot_rate_2,oa_xgot_rate_1)))
    
    h_pr <- h_pass_rate/mean(c(h_pass_rate,a_pass_rate))
    a_pr <- a_pass_rate/mean(c(h_pass_rate,a_pass_rate))
    h_shr <- h_shot_rate/mean(c(h_shot_rate,a_shot_rate))
    a_shr <- a_shot_rate/mean(c(h_shot_rate,a_shot_rate))
    h_sogr <- h_sog_rate/mean(c(h_sog_rate,a_sog_rate))
    a_sogr <- a_sog_rate/mean(c(h_sog_rate,a_sog_rate))
    h_xgr <- h_xg_rate/mean(c(h_xg_rate,a_xg_rate))
    a_xgr <- a_xg_rate/mean(c(h_xg_rate,a_xg_rate))
    h_xar <- h_xa_rate/mean(c(h_xa_rate,a_xa_rate))
    a_xar <- a_xa_rate/mean(c(h_xa_rate,a_xa_rate))
    h_xgotr <- h_xgot_rate/mean(c(h_xgot_rate,a_xgot_rate))
    a_xgotr <- a_xgot_rate/mean(c(h_xgot_rate,a_xgot_rate))
    
    lineup_rating_1 <- lineup_ratings(team1, team1_squad, lineup_stats, formation1, positions1,team1_player_data)
    lineup_rating_2 <- lineup_ratings(team2, team2_squad, lineup_stats, formation2, positions2,team2_player_data)
    
    h_lr <- lineup_rating_1/mean(c(lineup_rating_1,lineup_rating_2))
    a_lr <- lineup_rating_2/mean(c(lineup_rating_1,lineup_rating_2))
    
    pr1 <- mean(m_pr[1],h_lr,h_pr)
    sh1 <- mean(m_shr[1],h_lr,h_shr)
    sog1 <- mean(m_sogr[1],h_lr,h_sogr)
    xg1 <- mean(m_xgr[1],h_lr,h_xgr)
    xgot1 <- mean(m_xgotr[1],h_lr,h_xgotr)
    xa1 <- mean(m_xar[1],h_lr,h_xar)
    
    pr2 <- mean(m_pr[2],a_lr,a_pr)
    sh2 <- mean(m_shr[2],a_lr,a_shr)
    sog2 <- mean(m_sogr[2],a_lr,a_sogr)
    xg2 <- mean(m_xgr[2],a_lr,a_xgr)
    xgot2 <- mean(m_xgotr[2],a_lr,a_xgotr)
    xa2 <- mean(m_xar[2],a_lr,a_xar)
    
    game_list <- get_game_data_by_team(league_lines,fixtures[yy,])
    game_data <- game_list[[1]][[1]]
    fixtures_2 <- game_list[[2]]
    
    game_data$passes_1 <- game_data$passes_1*pr1
    game_data$passes_2 <- game_data$passes_2*pr2
    game_data$team1_shots <- game_data$team1_shots*sh1
    game_data$team2_shots <- game_data$team2_shots*sh2
    game_data$team1_sogs <- game_data$team1_sogs*sog1
    game_data$team2_sogs <- game_data$team2_sogs*sog2
    game_data$team1_xg <- game_data$team1_xg*xg1
    game_data$team2_xg <- game_data$team2_xg*xg2
    game_data$team1_xgot <- game_data$team1_xgot*xgot1
    game_data$team2_xgot <- game_data$team2_xgot*xgot2
    game_data$team1_xa <- game_data$team1_xa*xa1
    game_data$team2_xa <- game_data$team2_xa*xa2
    
    player_stats_game <- get_game_player_data(lineup_stats, formation_lines, fixtures_2, game_data, team1, team2)
    
    
    
    lines2 <- lines[lines$BARSTOOL_TEAM1==team1&lines$BARSTOOL_TEAM2==team2,]
    
    if(length(which(duplicated(lines2)))==0){
      
    }else{
      lines2 <- lines2[-which(duplicated(lines2)),]
    }
    
    goalie_ratings <- goalie_adjustments(lineup_stats, league_lines, fixtures[yy,])
    
    sub_ratings <- sub_adjustments(team1_subs,team2_subs,player_stats_game, team1,team2)
    
    predictions <- game_predict(league_lines, fixtures_2, lines2, player_stats_game = sub_ratings, league[1], goalie_ratings, date)
    
   
    
    
    
     if(prop_indicator!="none"){
       if(prop_indicator=="seriea" | prop_indicator=="dfl"){
         player_prop_list <- player_prop_lines(prop_indicator)
         prop_stats <- player_props_no_passes(player_prop_list,player_stats_game,paste0(l$ccode[l$id==id[zz]]," ",l$country[l$id==id[zz]]))
       }else{
         player_prop_list <- player_prop_lines(prop_indicator)
         prop_stats <- player_props_all(player_prop_list,player_stats_game,paste0(l$ccode[l$id==id[zz]]," ",l$country[l$id==id[zz]]))
       }
      
      
      
      
      prop_df <- prop_stats[[1]]
      player_stats_game <- prop_stats[[2]]
      
      
    }else{
      
    }
    
    home_attg <- predictions[[3]]
    away_attg <- predictions[[4]]
    
    
    print("____________Anytime Goalscorers______________________")
    
    tryCatch({
      gs_lines <- fanduel_goalscorers(sport_codes,sport_matt_id,leagues_df,league_id[zz])
      if(length(which(duplicated(gs_lines)))==0){
        
      }else{
        gs_lines <-  gs_lines[-which(duplicated(gs_lines)),] 
      }
      exgs <- data.frame(rbind(rbindlist(home_attg),rbindlist(away_attg)))
      exgs$Var1 <- as.character(exgs$Var1)
      sub_ratings <- left_join(sub_ratings,exgs, by = c("pageUrl2" = "Var1"))
      if(length(which(is.na(sub_ratings$team)))==0){
        
      }else{
        sub_ratings <-  sub_ratings[-which(is.na(sub_ratings$team)),] 
      }
      sub_ratings$Freq[which(is.na(sub_ratings$Freq))] <- 0
      sub_ratings$expected_attg <- (sub_ratings$Freq)/40000
      
      sub_ratings$expected_gs <- 0
      
      for(i in 1:length(sub_ratings$pageUrl2)){
        
        name <- gsub("-"," ",strsplit(sub_ratings$page_url_2[i],"/")[[1]][4])
        name
        fixed_name <- tolower(gs_lines$runnerName)[which(tolower(gs_lines$runnerName)==tolower(gs_lines$runnerName)[amatch(name, tolower(gs_lines$runnerName), maxDist = Inf)])]
        fixed_name
        if(length(1/gs_lines$decimalOdds[which(tolower(gs_lines$runnerName)==fixed_name)])==0){
          
        }else{
          sub_ratings$expected_gs[i] <- 1/gs_lines$decimalOdds[which(tolower(gs_lines$runnerName)==fixed_name)]
        }
        
        
        
       
        
      }
      sub_ratings$bet <- ifelse(sub_ratings$expected_attg>=(sub_ratings$expected_gs+.05),"*","")
      print(kable(sub_ratings[,c("pageUrl2","team","expected_attg","expected_gs","bet")]))
      key_player_stats <- sub_ratings[,c("confirmed","pageUrl2","positionRow","team",
                                               "expected_passes","expected_tackles","expected_sogs","expected_shots","expected_xg","expected_xgot","expected_xa", 
                                               "exp_min"       ,   "expected_attg"    ,     "expected_gs" )]
      key_player_stats[,c(5:14)] <- round(key_player_stats[,c(5:14)],4)
      
    },
    
    error=function(e) {
      message('No goalscorer lines available')
      key_player_stats <- sub_ratings[,c("confirmed","pageUrl2","positionRow","team",
                                               "expected_passes","expected_tackles","expected_sogs","expected_shots","expected_xg","expected_xgot","expected_xa",
                                               "exp_min"       )]
      key_player_stats[,c(5:12)] <- round(key_player_stats[,c(5:12)],4)
    })
    
    cat("____________________________________________________\n\n")
    print(paste0("Facts about ",team1,":"))
    print(paste0("Lineup: ", unique(key_player_stats$confirmed)))
    print(kable(key_player_stats[key_player_stats$team==team1,c(2,3,5,6,7,8,9,10,11,12)]))
    print(kable(manager_df[1,c(1,4,11,25)]))
    cat("____________________________________________________\n\n")
    
    print(paste0("Facts about ",team2,":"))
    print(paste0("Lineup: ", unique(key_player_stats$confirmed)))
    print(kable(key_player_stats[key_player_stats$team==team2,c(2,3,5,6,7,8,9,10,11,12)]))
    print(kable(manager_df[2,c(1,4,11,25)]))
    
    send_to_git(league[1],team1,team2)
    closeAllConnections()
  }
  
}


####### PRACTICE ########



















