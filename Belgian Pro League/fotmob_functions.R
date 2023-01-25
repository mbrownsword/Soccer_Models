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


get_player_data_all <- function(leagues, team_df){
  
  l <- fotmob_get_league_ids(cached = FALSE)
  l_list <- vector("list",1)
  
  
  for(c in leagues){
    
    tdf <- fotmob_get_league_matches(l$ccode[c],l$name[c], cached = FALSE)
    
    if(length(tdf$id)==0){
      next()
    }else{
      
    }
    
    p_list <- vector("list",length(tdf$id))
    for(a in 1:length(tdf$id)){
      if(tdf$status$utcTime[a]>Sys.Date()){
        next()
      }
      e <- fotmob_get_match_info(tdf$id[a])
      p <- fotmob_get_match_players(tdf$id[a])
      if(length(p)==0){
        next()
      }else{
        p_list[[a]] <- p
      }
      print(paste0("Done with game: ",e$home_team," v ",e$away_team," on ",e$match_date_utc_time))
    }
    
    p_afc <- rbindlist(p_list, fill = T)
    
    if(length(p_afc$match_id)==0){
      next()
    }else{
      
    }
    
    p_afc$stats_minutes_played <- as.numeric(p_afc$stats_minutes_played)
    p_afc$stats_total_shots <- as.numeric(p_afc$stats_total_shots)
    for(b in 1:length(p_afc$match_id)){
      p_afc$stats_tackles_won[b] <- as.numeric(strsplit(strsplit(p_afc$stats_tackles_won[b],"/")[[1]][2]," ")[[1]][1])
      p_afc$stats_accurate_passes[b] <- as.numeric(strsplit(strsplit(p_afc$stats_accurate_passes[b],"/")[[1]][2]," ")[[1]][1])
      p_afc$stats_shot_accuracy[b] <- as.numeric(strsplit(p_afc$stats_shot_accuracy[b],"/")[[1]][1])
    }
    p_afc$stats_shot_accuracy <- as.numeric(p_afc$stats_shot_accuracy)
    p_afc$stats_tackles_won <- as.numeric(p_afc$stats_tackles_won)
    p_afc$stats_goals <- as.numeric(p_afc$stats_goals)
    p_afc$stats_assists <- as.numeric(p_afc$stats_assists)
    p_afc$stats_accurate_passes <- as.numeric(p_afc$stats_accurate_passes)
    
    
    
    #if(grepl("World Cup Qualification",l$name[c])){
    f_teams <- read_xlsx("team_df2 3.xlsx")
    elo <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))
    
    for(d in 1:length(p_afc$team_id)){
      
      if(p_afc$is_home_team[d]==TRUE){
        e <- fotmob_get_match_info(p_afc$match_id[d])
        
        if(is.element(e$home_team,team_df$team_1)){
          e$home_team <- team_df$team_2[which(team_df$team_1==e$home_team)]
        }
        
        if(is.element(e$away_team,team_df$team_1)){
          e$away_team <- team_df$team_2[which(team_df$team_1==e$away_team)]
        }
        
        
        if(is.element(e$home_team,f_teams$team)){
          dt <- date(e$match_date_utc_time)
          
          if(length(which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team)))))==0){
            games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                           which(elo$team1==e$away_team),which(elo$team2==e$home_team)),]
          }else{
            games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                           which(elo$team1==e$away_team),which(elo$team2==e$home_team))[-which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                                                                                                            which(elo$team1==e$away_team),which(elo$team2==e$home_team))))],]
          }
          p_game <- games[games$date<=dt,]
          p_game <- p_game[c(which(p_game$team2==e$away_team),which(p_game$team1==e$away_team)),]
          p_game <- p_game[max(p_game$date)==p_game$date,]
          if(p_game$team2==e$away_team){
            p_afc$opp_rating[d] <- p_game$spi2
          }else{
            p_afc$opp_rating[d] <- p_game$spi1
          }
        }else{
          if(is.element(e$home_team,f_teams$team2)){
            e$home_team <- f_teams$team[which(e$home_team==f_teams$team2)][1]
            dt <- date(e$match_date_utc_time)
            if(length(which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team)))))==0){
              games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                             which(elo$team1==e$away_team),which(elo$team2==e$home_team)),]
            }else{
              games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                             which(elo$team1==e$away_team),which(elo$team2==e$home_team))[-which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                                                                                                              which(elo$team1==e$away_team),which(elo$team2==e$home_team))))],]
            }
            p_game <- games[games$date<=dt,]
            p_game <- p_game[c(which(p_game$team2==e$away_team),which(p_game$team1==e$away_team)),]
            p_game <- p_game[max(p_game$date)==p_game$date,]
            if(p_game$team2==e$away_team){
              p_afc$opp_rating[d] <- p_game$spi2
            }else{
              p_afc$opp_rating[d] <- p_game$spi1
            }
          }else{
            print(paste0("No team data for ",e$home_team))
            break()
          } 
        }
        
        
      }else{
        
        e <- fotmob_get_match_info(p_afc$match_id[d])
        if(is.element(e$home_team,team_df$team_1)){
          e$home_team <- team_df$team_2[which(team_df$team_1==e$home_team)]
        }
        
        if(is.element(e$away_team,team_df$team_1)){
          e$away_team <- team_df$team_2[which(team_df$team_1==e$away_team)]
        }
        if(is.element(e$away_team,f_teams$team)){
          dt <- date(e$match_date_utc_time)
          if(length(which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team)))))==0){
            games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                           which(elo$team1==e$away_team),which(elo$team2==e$home_team)),]
          }else{
            games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                           which(elo$team1==e$away_team),which(elo$team2==e$home_team))[-which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                                                                                                            which(elo$team1==e$away_team),which(elo$team2==e$home_team))))],]
          }
          p_game <- games[games$date<=dt,]
          p_game <- p_game[c(which(p_game$team2==e$home_team),which(p_game$team1==e$home_team)),]
          p_game <- p_game[max(p_game$date)==p_game$date,]
          if(p_game$team2==e$home_team){
            p_afc$opp_rating[d] <- p_game$spi2
          }else{
            p_afc$opp_rating[d] <- p_game$spi1
          }
        }else{
          if(is.element(e$away_team,f_teams$team2)){
            e$away_team <- f_teams$team[which(e$away_team==f_teams$team2)][1]
            dt <- date(e$match_date_utc_time)
            if(length(which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team)))))==0){
              games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                             which(elo$team1==e$away_team),which(elo$team2==e$home_team)),]
            }else{
              games <- elo[c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                             which(elo$team1==e$away_team),which(elo$team2==e$home_team))[-which(duplicated(c(which(elo$team1==e$home_team),which(elo$team2==e$away_team),
                                                                                                              which(elo$team1==e$away_team),which(elo$team2==e$home_team))))],]
            }
            p_game <- games[games$date<=dt,]
            p_game <- p_game[c(which(p_game$team2==e$home_team),which(p_game$team1==e$home_team)),]
            p_game <- p_game[max(p_game$date)==p_game$date,]
            if(p_game$team2==e$home_team){
              p_afc$opp_rating[d] <- p_game$spi2
            }else{
              p_afc$opp_rating[d] <- p_game$spi1
            }
          }else{
            print(paste0("No team data for ",e$away_team))
            break()
          } 
        }
        
      }
      print(d)
    }
    
    
    #}else{
    #  
    #}
    
    
    p_afc$league <- l$name[c]
    
    l_list[[c]] <- p_afc
    
    print(paste0("Done with ",l$name[c]))
    
  }
  
  wc_q_stat <- rbindlist(l_list, fill = T)
  
  #### Elo Calculations (International) #####
  
  for(f in 1:length(wc_q_stat$match_id)){
    wc_q_stat$team_rating[f] <- unique(wc_q_stat$opp_rating[wc_q_stat$match_id==wc_q_stat$match_id[f]])[(unique(wc_q_stat$opp_rating[wc_q_stat$match_id==wc_q_stat$match_id[f]]))!=wc_q_stat$opp_rating[f]]
    wc_q_stat$elo_dif[f] <-  wc_q_stat$team_rating[f]-wc_q_stat$opp_rating[f]
  }
  
  
  #### Rolled Up Data #####
  
  
  wc_q_stat$stats_expected_goals_x_g <- as.numeric(wc_q_stat$stats_expected_goals_x_g)
  wc_q_stat$stats_expected_goals_on_target_x_got <- as.numeric(wc_q_stat$stats_expected_goals_on_target_x_got)
  wc_q_stat$stats_expected_assists_x_a <- as.numeric(wc_q_stat$stats_expected_assists_x_a)
  wc_q_stat$stats_x_got_faced <- as.numeric(wc_q_stat$stats_x_got_faced)
  
  for(i in 1:length(team_df$team_1)){
    wc_q_stat$team_name[wc_q_stat$team_name==team_df$team_1[i]] <- team_df$team_2[i]
  }
  
  
  
  
  wc_q_stat$page_url_2 <- wc_q_stat$page_url
  for(i in 1:length(wc_q_stat$page_url)){
    
    wc_q_stat$page_url[i] <- paste0("/",strsplit(wc_q_stat$page_url_2[i],"/")[[1]][2],"/",strsplit(wc_q_stat$page_url_2[i],"/")[[1]][3],"/")
    
  }
  
  
  list(wc_q_stat,f_teams)
}


get_team_lines <- function(f1_stats){
  
  stat_game <- f1_stats %>%
    group_by(team_name, league, match_id, is_home_team) %>%
    summarise(games = sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T),
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T),
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T),
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T),
              avg_elo_dif = mean(elo_dif))
  
  
  stat_game$passes_against <- 0
  stat_game$tackles_against <- 0
  stat_game$shots_against <- 0
  stat_game$sogs_against <- 0
  stat_game$xg_against <- 0
  stat_game$xgot_against <- 0
  stat_game$xa_against <- 0
  
  for(i in 1:length(stat_game$team_name)){
    
    stat_game$passes_against[i] <- stat_game$passes[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$tackles_against[i] <- stat_game$tackles[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$shots_against[i] <- stat_game$shots[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$sogs_against[i] <- stat_game$shotsongoal[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$xg_against[i] <- stat_game$expected_goals[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$xgot_against[i] <- stat_game$expected_goals_ot[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    stat_game$xa_against[i] <- stat_game$expected_ast[stat_game$match_id==stat_game$match_id[i]&stat_game$team_name!=stat_game$team_name[i]]
    
  }
  
  if(length(which(stat_game$games<800))==0){
    
  }else{
    stat_game <- stat_game[-which(stat_game$games<800),]
  }
  
  
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
  
  xg_line_home <- lm(stat_game$expected_goals[!is.na(stat_game$expected_goals)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_goals)&stat_game$is_home_team==TRUE])
  xgot_line_home <- lm(stat_game$expected_goals_ot[!is.na(stat_game$expected_goals_ot)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_goals_ot)&stat_game$is_home_team==TRUE])
  xa_line_home <- lm(stat_game$expected_ast[!is.na(stat_game$expected_ast)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_ast)&stat_game$is_home_team==TRUE])
  
  xg_line_away <- lm(stat_game$expected_goals[!is.na(stat_game$expected_goals)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_goals)&stat_game$is_home_team==FALSE])
  xgot_line_away <- lm(stat_game$expected_goals_ot[!is.na(stat_game$expected_goals_ot)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_goals_ot)&stat_game$is_home_team==FALSE])
  xa_line_away <- lm(stat_game$expected_ast[!is.na(stat_game$expected_ast)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$expected_ast)&stat_game$is_home_team==FALSE])
  
  xg_against_line_home <- lm(stat_game$xg_against[!is.na(stat_game$xg_against)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$xg_against)&stat_game$is_home_team==TRUE])
  xgot_against_line_home <- lm(stat_game$xgot_against[!is.na(stat_game$xgot_against)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$xgot_against)&stat_game$is_home_team==TRUE])
  xa_against_line_home <- lm(stat_game$xa_against[!is.na(stat_game$xa_against)&stat_game$is_home_team==TRUE]~stat_game$avg_elo_dif[!is.na(stat_game$xa_against)&stat_game$is_home_team==TRUE])
  
  xg_against_line_away <- lm(stat_game$xg_against[!is.na(stat_game$xg_against)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$xg_against)&stat_game$is_home_team==FALSE])
  xgot_against_line_away <- lm(stat_game$xgot_against[!is.na(stat_game$xgot_against)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$xgot_against)&stat_game$is_home_team==FALSE])
  xa_against_line_away <- lm(stat_game$xa_against[!is.na(stat_game$xa_against)&stat_game$is_home_team==FALSE]~stat_game$avg_elo_dif[!is.na(stat_game$xa_against)&stat_game$is_home_team==FALSE])
  
  
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
  
  
  
  list(stat_game,
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
       xa_against_line_away)
  
}


get_game_data_by_team <- function(stat_game,pass_line_home,tkl_line_home,shots_line_home,sog_line_home,game_no, fixtures,
                                  pass_line_against_home,
                                  tkl_line_against_home,
                                  shots_line_against_home,
                                  sog_line_against_home,
                                  pass_line_against_away,
                                  tkl_line_against_away,
                                  shots_line_against_away,
                                  sog_line_against_away,pass_line_away,tkl_line_away,shots_line_away,sog_line_away,f_teams_f){
  
  
  fixtures$team1 <- fixtures$home$name
  fixtures$team2 <- fixtures$away$name
  
  fixtures$elo_dif_1 <- 0
  fixtures$elo_dif_2 <- 0
  
  tackle_range_list <- vector("list",1)
  pass_range_list <- vector("list",1)
  shots_range_list <-  vector("list",1)
  shots_on_goal_range_list <-  vector("list",1)
  xg_range_list <- vector("list",1)
  xgot_range_list <- vector("list",1)
  xa_range_list <- vector("list",1)
  
  for(g in 1:length(fixtures$round)){
    
    
    if(is.element(fixtures$team1[g],stat_game$team_name)){
      
    }else{
      fixtures$team1[g] <- f_teams_f$team2[which(fixtures$team1[g]==f_teams_f$team)][1]
    }
    
    if(is.element(fixtures$team2[g],stat_game$team_name)){
      
    }else{
      fixtures$team2[g] <- f_teams_f$team2[which(fixtures$team2[g]==f_teams_f$team)][1]
    }
    
    
    fixtures$elo_dif_1[g] <- as.numeric(fixtures$spi1[g]-fixtures$spi2[g])
    fixtures$elo_dif_2[g] <- as.numeric(fixtures$spi2[g]-fixtures$spi1[g])
    
    elo_dif_1 <- as.numeric(fixtures$spi1[g]-fixtures$spi2[g])
    elo_dif_2 <- as.numeric(fixtures$spi2[g]-fixtures$spi1[g])
    
    #Tackles
    
    team1_tackles <- (tkl_line_home$coefficients[[2]]*abs(elo_dif_1))+tkl_line_home$coefficients[[1]]
    team2_tackles_against <- (tkl_line_against_away$coefficients[[2]]*abs(elo_dif_2))+tkl_line_against_away$coefficients[[1]]
    
    team1_tackles <- mean(c(team1_tackles,team2_tackles_against))
    
    tackle_rate <- mean(stat_game$tkls_minus_xp)
    
    team1_tackle_rate <- mean(stat_game$tkls_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_tackle_rate_sd <- sd(stat_game$tkls_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    tackle_against_rate <- mean(stat_game$tkls_against_minus_xp)
    
    team2_tackle_against_rate <- mean(stat_game$tkls_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_tackle_against_rate_sd <- sd(stat_game$tkls_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_tackle <- team1_tackles+team1_tackle_rate-team1_tackle_rate_sd+tackle_rate+team2_tackle_against_rate-team2_tackle_against_rate_sd+tackle_against_rate
    team1_mid_tackle <- team1_tackles+team1_tackle_rate+tackle_rate+team2_tackle_against_rate+tackle_against_rate
    team1_best_tackle <- team1_tackles+team1_tackle_rate+team1_tackle_rate_sd+tackle_rate+team2_tackle_against_rate+team2_tackle_against_rate_sd+tackle_against_rate
    
    team2_tackles <- (tkl_line_away$coefficients[[2]]*abs(elo_dif_2))+tkl_line_away$coefficients[[1]]
    team1_tackles_against <- (tkl_line_against_home$coefficients[[2]]*abs(elo_dif_1))+tkl_line_against_home$coefficients[[1]]
    
    team2_tackles <- mean(c(team2_tackles,team1_tackles_against))
    
    team2_tackle_rate <- mean(stat_game$tkls_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_tackle_rate_sd <- sd(stat_game$tkls_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_tackle_against_rate <- mean(stat_game$tkls_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_tackle_against_rate_sd <- sd(stat_game$tkls_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    
    team2_worst_tackle <- team2_tackles+team2_tackle_rate-team2_tackle_rate_sd+tackle_rate+team1_tackle_against_rate-team1_tackle_against_rate_sd+tackle_against_rate
    team2_mid_tackle <- team2_tackles+team2_tackle_rate+tackle_rate+team1_tackle_against_rate+tackle_against_rate
    team2_best_tackle <- team2_tackles+team2_tackle_rate+team2_tackle_rate_sd+tackle_rate+team1_tackle_against_rate+team1_tackle_against_rate_sd+tackle_against_rate
    
    elos <- c(rep(fixtures$spi1[g],3),rep(fixtures$spi2[g],3))
    teams <- c(rep(fixtures$team1[g],3),rep(fixtures$team2[g],3))
    
    tackle_ranges <- c(team1_worst_tackle,team1_mid_tackle,team1_best_tackle,
                       team2_worst_tackle,team2_mid_tackle,team2_best_tackle)
    
    ggplot(data=data.frame(elos,tackle_ranges,teams), aes(x=tackle_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    tackle_range_list[[g]] <- data.frame(elos,tackle_ranges,teams)
    
    
    
    
    passes_rate <- mean(stat_game$pass_minus_xp)
    passes_against_rate <- mean(stat_game$pass_against_minus_xp)
    
    team1_passes <- (pass_line_home$coefficients[[2]]*elo_dif_1)+pass_line_home$coefficients[[1]]
    team2_passes_against <- (pass_line_against_away$coefficients[[2]]*abs(elo_dif_2))+pass_line_against_away$coefficients[[1]]
    
    team1_passes <- mean(c(team1_passes,team2_passes_against))
    
    team1_pass_rate <- mean(stat_game$pass_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_pass_rate_sd <- sd(stat_game$pass_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_pass_against_rate <- mean(stat_game$pass_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_pass_against_rate_sd <- sd(stat_game$pass_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_pass <- team1_passes+team1_pass_rate-team1_pass_rate_sd+passes_rate+team2_pass_against_rate-team2_pass_against_rate_sd+passes_against_rate
    team1_mid_pass <- team1_passes+team1_pass_rate+passes_rate+team2_pass_against_rate+passes_against_rate
    team1_best_pass <- team1_passes+team1_pass_rate+team1_pass_rate_sd+passes_rate+team2_pass_against_rate+team2_pass_against_rate_sd+passes_against_rate
    
    team2_passes <-(pass_line_away$coefficients[[2]]*elo_dif_2)+pass_line_away$coefficients[[1]]
    team1_passes_against <- (pass_line_against_home$coefficients[[2]]*abs(elo_dif_1))+pass_line_against_home$coefficients[[1]]
    
    team2_passes <- mean(c(team2_passes,team1_passes_against))
    
    team2_pass_rate <- mean(stat_game$pass_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_pass_rate_sd <- sd(stat_game$pass_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    
    team1_pass_against_rate <- mean(stat_game$pass_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_pass_against_rate_sd <- sd(stat_game$pass_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_worst_pass <- team2_passes+team2_pass_rate-team2_pass_rate_sd+passes_rate+team1_pass_against_rate-team1_pass_against_rate_sd+passes_against_rate
    team2_mid_pass <- team2_passes+team2_pass_rate+passes_rate+team1_pass_against_rate+passes_against_rate
    team2_best_pass <- team2_passes+team2_pass_rate+team2_pass_rate_sd+passes_rate+team1_pass_against_rate+team1_pass_against_rate_sd+passes_against_rate
    
    pass_ranges <- c(team1_worst_pass,team1_mid_pass,team1_best_pass,
                     team2_worst_pass,team2_mid_pass,team2_best_pass)
    
    ggplot(data=data.frame(elos,pass_ranges,teams), aes(x=pass_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    pass_range_list[[g]] <- data.frame(elos,pass_ranges,teams)
    
    shots_rate <- mean(stat_game$shots_minus_xp)
    shots_against_rate <- mean(stat_game$shots_against_minus_xp)
    
    team1_shots <- (shots_line_home$coefficients[[2]]*elo_dif_1)+shots_line_home$coefficients[[1]]
    team2_shots_against <- (shots_line_against_away$coefficients[[2]]*abs(elo_dif_2))+shots_line_against_away$coefficients[[1]]
    
    team1_shots <- mean(c(team1_shots,team2_shots_against))
    
    team1_shot_rate <- mean(stat_game$shots_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_shot_rate_sd <- sd(stat_game$shots_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_shot_against_rate <- mean(stat_game$shots_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_shot_against_rate_sd <- sd(stat_game$shots_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_shot <- team1_shots+team1_shot_rate-team1_shot_rate_sd+shots_rate+team2_shot_against_rate-team2_shot_against_rate_sd+shots_against_rate
    team1_mid_shot <- team1_shots+team1_shot_rate+shots_rate+team2_shot_against_rate+shots_against_rate
    team1_best_shot <- team1_shots+team1_shot_rate+team1_shot_rate_sd+shots_rate+team2_shot_against_rate+team2_shot_against_rate_sd+shots_against_rate
    
    team2_shots <-(shots_line_away$coefficients[[2]]*elo_dif_2)+shots_line_away$coefficients[[1]]
    team1_shots_against <- (shots_line_against_home$coefficients[[2]]*abs(elo_dif_1))+shots_line_against_home$coefficients[[1]]
    
    team2_shots <- mean(c(team2_shots,team1_shots_against))
    
    team2_shot_rate <- mean(stat_game$shots_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_shot_rate_sd <- sd(stat_game$shots_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_shot_against_rate <- mean(stat_game$shots_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_shot_against_rate_sd <- sd(stat_game$shots_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_worst_shot <- team2_shots+team2_shot_rate-team2_shot_rate_sd+shots_rate+team1_shot_against_rate-team1_shot_against_rate_sd+shots_against_rate
    team2_mid_shot <- team2_shots+team2_shot_rate+shots_rate+team1_shot_against_rate+shots_against_rate
    team2_best_shot <- team2_shots+team2_shot_rate+team2_shot_rate_sd+shots_rate+team1_shot_against_rate+team1_shot_against_rate_sd+shots_against_rate
    
    shot_ranges <- c(team1_worst_shot,team1_mid_shot,team1_best_shot,
                     team2_worst_shot,team2_mid_shot,team2_best_shot)
    
    ggplot(data=data.frame(elos,shot_ranges,teams), aes(x=shot_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    
    shots_range_list[[g]] <- data.frame(elos,shot_ranges,teams)
    
    sogs_rate <- mean(stat_game$sogs_minus_xp)
    sogs_against_rate <-  mean(stat_game$sogs_against_minus_xp)
    
    team1_sogs <- (sog_line_home$coefficients_home[[2]]*elo_dif_1)+sog_line_home$coefficients_home[[1]]
    team2_sogs_against <- (sog_line_against_away$coefficients[[2]]*abs(elo_dif_2))+sog_line_against_away$coefficients[[1]]
    
    team1_sogs <- mean(c(team1_sogs,team2_sogs_against))
    
    team1_sog_rate <- mean(stat_game$sogs_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_sog_rate_sd <- sd(stat_game$sogs_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_sog_against_rate <- mean(stat_game$sogs_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_sog_against_rate_sd <- sd(stat_game$sogs_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_sog <- team1_sogs+team1_sog_rate-team1_sog_rate_sd+sogs_rate+team2_sog_against_rate-team2_sog_against_rate_sd+sogs_against_rate
    team1_mid_sog <- team1_sogs+team1_sog_rate+shots_rate+team2_sog_against_rate+sogs_against_rate
    team1_best_sog <- team1_sogs+team1_sog_rate+team1_sog_rate_sd+sogs_rate+team2_sog_against_rate+team2_sog_against_rate_sd+sogs_against_rate
    
    team2_sogs <-(sog_line_away$coefficients[[2]]*elo_dif_2)+sog_line_away$coefficients[[1]]
    team1_sogs_against <- (sog_line_against_home$coefficients[[2]]*abs(elo_dif_1))+sog_line_against_home$coefficients[[1]]
    
    team2_sogs <- mean(c(team2_sogs,team1_sogs_against))
    
    team2_sog_rate <- mean(stat_game$sogs_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_sog_rate_sd <- sd(stat_game$sogs_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_sog_against_rate <- mean(stat_game$sogs_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_sog_against_rate_sd <- sd(stat_game$sogs_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    team2_worst_sog <- team2_sogs+team2_sog_rate-team2_sog_rate_sd+sogs_rate+team1_sog_against_rate-team1_sog_against_rate_sd+sogs_against_rate
    team2_mid_sog <- team2_sogs+team2_sog_rate+shots_rate+team1_sog_against_rate+sogs_against_rate
    team2_best_sog <- team2_sogs+team2_sog_rate+team2_sog_rate_sd+sogs_rate+team1_sog_against_rate+team1_sog_against_rate_sd+sogs_against_rate
    
    sog_ranges <- c(team1_worst_sog,team1_mid_sog,team1_best_sog,
                    team2_worst_sog,team2_mid_sog,team2_best_sog)
    
    ggplot(data=data.frame(elos,sog_ranges,teams), aes(x=sog_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    shots_on_goal_range_list[[g]] <- data.frame(elos,sog_ranges,teams)
    
    #Tackles
    
    team1_xg <- (xg_line_home$coefficients[[2]]*abs(elo_dif_1))+xg_line_home$coefficients[[1]]
    team2_xg_against <- (xg_against_line_away$coefficients[[2]]*abs(elo_dif_2))+xg_against_line_away$coefficients[[1]]
    
    team1_xg <- mean(c(team1_xg,team2_xg_against))
    
    xg_rate <- mean(stat_game$xg_minus_xp)
    
    team1_xg_rate <- mean(stat_game$xg_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xg_rate_sd <- sd(stat_game$xg_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    xg_against_rate <- mean(stat_game$xg_against_minus_xp)
    
    team2_xg_against_rate <- mean(stat_game$xg_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xg_against_rate_sd <- sd(stat_game$xg_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_xg <- team1_xg+team1_xg_rate-team1_xg_rate_sd+xg_rate+team2_xg_against_rate-team2_xg_against_rate_sd+xg_against_rate
    team1_mid_xg <- team1_xg+team1_xg_rate+xg_rate+team2_xg_against_rate+xg_against_rate
    team1_best_xg <- team1_xg+team1_xg_rate+team1_xg_rate_sd+xg_rate+team2_xg_against_rate+team2_xg_against_rate_sd+xg_against_rate
    
    team2_xg <- (xg_line_away$coefficients[[2]]*abs(elo_dif_2))+xg_line_away$coefficients[[1]]
    team1_xg_against <- (xg_against_line_home$coefficients[[2]]*abs(elo_dif_1))+xg_against_line_home$coefficients[[1]]
    
    team2_xg <- mean(c(team2_xg,team1_xg_against))
    
    team2_xg_rate <- mean(stat_game$xg_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xg_rate_sd <- sd(stat_game$xg_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_xg_against_rate <- mean(stat_game$xg_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xg_against_rate_sd <- sd(stat_game$xg_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    
    team2_worst_xg <- team2_xg+team2_xg_rate-team2_xg_rate_sd+xg_rate+team1_xg_against_rate-team1_xg_against_rate_sd+xg_against_rate
    team2_mid_xg <- team2_xg+team2_xg_rate+xg_rate+team1_xg_against_rate+xg_against_rate
    team2_best_xg <- team2_xg+team2_xg_rate+team2_xg_rate_sd+xg_rate+team1_xg_against_rate+team1_xg_against_rate_sd+xg_against_rate
    
    xg_ranges <- c(team1_worst_xg,team1_mid_xg,team1_best_xg,
                       team2_worst_xg,team2_mid_xg,team2_best_xg)
    
    ggplot(data=data.frame(elos,xg_ranges,teams), aes(x=xg_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    xg_range_list[[g]] <- data.frame(elos,xg_ranges,teams)
    
    
    team1_xgot <- (xgot_line_home$coefficients[[2]]*abs(elo_dif_1))+xgot_line_home$coefficients[[1]]
    team2_xgot_against <- (xgot_against_line_away$coefficients[[2]]*abs(elo_dif_2))+xgot_against_line_away$coefficients[[1]]
    
    team1_xgot <- mean(c(team1_xgot,team2_xgot_against))
    
    xgot_rate <- mean(stat_game$xgot_minus_xp)
    
    team1_xgot_rate <- mean(stat_game$xgot_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xgot_rate_sd <- sd(stat_game$xgot_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    xgot_against_rate <- mean(stat_game$xgot_against_minus_xp)
    
    team2_xgot_against_rate <- mean(stat_game$xgot_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xgot_against_rate_sd <- sd(stat_game$xgot_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_xgot <- team1_xgot+team1_xgot_rate-team1_xgot_rate_sd+xgot_rate+team2_xgot_against_rate-team2_xgot_against_rate_sd+xgot_against_rate
    team1_mid_xgot <- team1_xgot+team1_xgot_rate+xgot_rate+team2_xgot_against_rate+xgot_against_rate
    team1_best_xgot <- team1_xgot+team1_xgot_rate+team1_xgot_rate_sd+xgot_rate+team2_xgot_against_rate+team2_xgot_against_rate_sd+xgot_against_rate
    
    team2_xgot <- (xgot_line_away$coefficients[[2]]*abs(elo_dif_2))+xgot_line_away$coefficients[[1]]
    team1_xgot_against <- (xgot_against_line_home$coefficients[[2]]*abs(elo_dif_1))+xgot_against_line_home$coefficients[[1]]
    
    team2_xgot <- mean(c(team2_xgot,team1_xgot_against))
    
    team2_xgot_rate <- mean(stat_game$xgot_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xgot_rate_sd <- sd(stat_game$xgot_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_xgot_against_rate <- mean(stat_game$xgot_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xgot_against_rate_sd <- sd(stat_game$xgot_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    
    team2_worst_xgot <- team2_xgot+team2_xgot_rate-team2_xgot_rate_sd+xgot_rate+team1_xgot_against_rate-team1_xgot_against_rate_sd+xgot_against_rate
    team2_mid_xgot <- team2_xgot+team2_xgot_rate+xgot_rate+team1_xgot_against_rate+xgot_against_rate
    team2_best_xgot <- team2_xgot+team2_xgot_rate+team2_xgot_rate_sd+xgot_rate+team1_xgot_against_rate+team1_xgot_against_rate_sd+xgot_against_rate
    
    xgot_ranges <- c(team1_worst_xgot,team1_mid_xgot,team1_best_xgot,
                   team2_worst_xgot,team2_mid_xgot,team2_best_xgot)
    
    ggplot(data=data.frame(elos,xgot_ranges,teams), aes(x=xgot_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    xgot_range_list[[g]] <- data.frame(elos,xgot_ranges,teams)
    
    team1_xa <- (xa_line_home$coefficients[[2]]*abs(elo_dif_1))+xa_line_home$coefficients[[1]]
    team2_xa_against <- (xa_against_line_away$coefficients[[2]]*abs(elo_dif_2))+xa_against_line_away$coefficients[[1]]
    
    team1_xa <- mean(c(team1_xa,team2_xa_against))
    
    xa_rate <- mean(stat_game$xa_minus_xp)
    
    team1_xa_rate <- mean(stat_game$xa_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xa_rate_sd <- sd(stat_game$xa_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    xa_against_rate <- mean(stat_game$xa_against_minus_xp)
    
    team2_xa_against_rate <- mean(stat_game$xa_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xa_against_rate_sd <- sd(stat_game$xa_against_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_worst_xa <- team1_xa+team1_xa_rate-team1_xa_rate_sd+xa_rate+team2_xa_against_rate-team2_xa_against_rate_sd+xa_against_rate
    team1_mid_xa <- team1_xa+team1_xa_rate+xa_rate+team2_xa_against_rate+xa_against_rate
    team1_best_xa <- team1_xa+team1_xa_rate+team1_xa_rate_sd+xa_rate+team2_xa_against_rate+team2_xa_against_rate_sd+xa_against_rate
    
    team2_xa <- (xa_line_away$coefficients[[2]]*abs(elo_dif_2))+xa_line_away$coefficients[[1]]
    team1_xa_against <- (xa_against_line_home$coefficients[[2]]*abs(elo_dif_1))+xa_against_line_home$coefficients[[1]]
    
    team2_xa <- mean(c(team2_xa,team1_xa_against))
    
    team2_xa_rate <- mean(stat_game$xa_minus_xp[stat_game$team_name==fixtures$team2[g]])
    team2_xa_rate_sd <- sd(stat_game$xa_minus_xp[stat_game$team_name==fixtures$team2[g]])
    
    team1_xa_against_rate <- mean(stat_game$xa_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    team1_xa_against_rate_sd <- sd(stat_game$xa_against_minus_xp[stat_game$team_name==fixtures$team1[g]])
    
    
    team2_worst_xa <- team2_xa+team2_xa_rate-team2_xa_rate_sd+xa_rate+team1_xa_against_rate-team1_xa_against_rate_sd+xa_against_rate
    team2_mid_xa <- team2_xa+team2_xa_rate+xa_rate+team1_xa_against_rate+xa_against_rate
    team2_best_xa <- team2_xa+team2_xa_rate+team2_xa_rate_sd+xa_rate+team1_xa_against_rate+team1_xa_against_rate_sd+xa_against_rate
    
    xa_ranges <- c(team1_worst_xa,team1_mid_xa,team1_best_xa,
                     team2_worst_xa,team2_mid_xa,team2_best_xa)
    
    ggplot(data=data.frame(elos,xa_ranges,teams), aes(x=xa_ranges, y=elos, group=teams)) +
      geom_line(aes(linetype=teams))+
      geom_point()
    
    xa_range_list[[g]] <- data.frame(elos,xa_ranges,teams)
    
  }
  
  sog_ranges_all <- rbindlist(shots_on_goal_range_list)
  shots_ranges_all <- rbindlist(shots_range_list)
  pass_ranges_all <- rbindlist(pass_range_list)
  tackles_ranges_all <- rbindlist(tackle_range_list)
  xg_ranges_all <- rbindlist(xg_range_list)
  xgot_ranges_all <- rbindlist(xgot_range_list)
  xa_ranges_all <- rbindlist(xa_range_list)
  
  list(pass_ranges_all,
       tackles_ranges_all,
       shots_ranges_all,
       sog_ranges_all,
       fixtures,
       xg_ranges_all,
       xgot_ranges_all,
       xa_ranges_all)
  
}


get_position_data <- function(wc_q_stat){
  
  stat_game <- wc_q_stat %>%
    group_by(team_name, league, match_id, is_home_team) %>%
    summarise(games = sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T),
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T),
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T),
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T),
              avg_elo_dif = mean(elo_dif))
  
  
  
  if(length(which(stat_game$games<800))==0){
    match_ids_remove <- 0
  }else{
    match_ids_remove <- stat_game$match_id[which(stat_game$games<800)]
  }
  
  rm(stat_game)
  
  stat_game_position <- wc_q_stat %>%
    group_by(team_name, match_id, league, position_row, is_home_team) %>%
    summarise(players = length(stats_minutes_played[stats_minutes_played!=0]),
              games = sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T)/sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T)*90,
              avg_elo_dif = mean(elo_dif))
  
  stat_game_position$shotsongoal[is.nan(stat_game_position$shotsongoal)] <- 0
  stat_game_position$tackles[is.nan(stat_game_position$tackles)] <- 0
  stat_game_position$shots[is.nan(stat_game_position$shots)] <- 0
  stat_game_position$passes[is.nan(stat_game_position$passes)] <- 0
  
  stat_game_position$passes_against <- 0
  stat_game_position$tackles_against <- 0
  stat_game_position$shots_against <- 0
  stat_game_position$sogs_against <- 0
  stat_game_position$xg_against <- 0
  stat_game_position$xgot_against <- 0
  stat_game_position$xa_against <- 0
  
  for(i in 1:length(stat_game_position$team_name)){
    
    if(length(stat_game_position$passes[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$passes_against[i] <- stat_game_position$passes[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$passes_against[i] <- stat_game_position$passes[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    if(length(stat_game_position$tackles[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$tackles_against[i] <- stat_game_position$tackles[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$tackles_against[i] <- stat_game_position$tackles[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    if(length(stat_game_position$shots[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$shots_against[i] <- stat_game_position$shots[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$shots_against[i] <- stat_game_position$shots[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    if(length(stat_game_position$shotsongoal[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$sogs_against[i] <- stat_game_position$shotsongoal[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$sogs_against[i] <- stat_game_position$shotsongoal[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    if(length(stat_game_position$expected_goals[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$xg_against[i] <- stat_game_position$expected_goals[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$xg_against[i] <- stat_game_position$expected_goals[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    if(length(stat_game_position$expected_goals_ot[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$xgot_against[i] <- stat_game_position$expected_goals_ot[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$xgot_against[i] <- stat_game_position$expected_goals_ot[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    
    if(length(stat_game_position$expected_ast[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]])==0){
      
      stat_game_position$xa_against[i] <- stat_game_position$expected_ast[stat_game_position$position_row==(stat_game_position$position_row[i]-1)&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
    }else{
      
      stat_game_position$xa_against[i] <- stat_game_position$expected_ast[stat_game_position$position_row==stat_game_position$position_row[i]&stat_game_position$match_id==stat_game_position$match_id[i]&stat_game_position$team_name!=stat_game_position$team_name[i]]
      
      
    }
    
    
  }
  
  if(match_ids_remove[1]==0){
    
  }else{
    stat_game_position <-  stat_game_position[-which(stat_game_position$match_id %in% match_ids_remove),]
  }
  
  stat_game_position_home <- stat_game_position[which(stat_game_position$is_home_team==TRUE),]
  
  
  shots_line_1_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots)])
  shots_line_2_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots)])
  shots_line_3_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots)])
  shots_line_4_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots)])
  shots_line_5_home <- lm(stat_game_position_home$shots[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots)])
  shotsongoal_line_1_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shotsongoal)])
  shotsongoal_line_2_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shotsongoal)])
  shotsongoal_line_3_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shotsongoal)])
  shotsongoal_line_4_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shotsongoal)])
  shotsongoal_line_5_home <- lm(stat_game_position_home$shotsongoal[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shotsongoal)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shotsongoal)])
  passes_line_1_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes)])
  passes_line_2_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes)])
  passes_line_3_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes)])
  passes_line_4_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes)])
  passes_line_5_home <- lm(stat_game_position_home$passes[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes)])
  tackles_line_1_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles)]))
  tackles_line_2_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles)]))
  tackles_line_3_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles)]))
  tackles_line_4_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles)]))
  tackles_line_5_home <- lm(stat_game_position_home$tackles[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles)]))
  
  shots_against_line_1_home <- lm(stat_game_position_home$shots_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$shots_against)])
  shots_against_line_2_home <- lm(stat_game_position_home$shots_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$shots_against)])
  shots_against_line_3_home <- lm(stat_game_position_home$shots_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$shots_against)])
  shots_against_line_4_home <- lm(stat_game_position_home$shots_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$shots_against)])
  shots_against_line_5_home <- lm(stat_game_position_home$shots_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$shots_against)])
  sogs_against_line_1_home <- lm(stat_game_position_home$sogs_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$sogs_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$sogs_against)])
  sogs_against_line_2_home <- lm(stat_game_position_home$sogs_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$sogs_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$sogs_against)])
  sogs_against_line_3_home <- lm(stat_game_position_home$sogs_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$sogs_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$sogs_against)])
  sogs_against_line_4_home <- lm(stat_game_position_home$sogs_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$sogs_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$sogs_against)])
  sogs_against_line_5_home <- lm(stat_game_position_home$sogs_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$sogs_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$sogs_against)])
  passes_against_line_1_home <- lm(stat_game_position_home$passes_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$passes_against)])
  passes_against_line_2_home <- lm(stat_game_position_home$passes_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$passes_against)])
  passes_against_line_3_home <- lm(stat_game_position_home$passes_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$passes_against)])
  passes_against_line_4_home <- lm(stat_game_position_home$passes_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$passes_against)])
  passes_against_line_5_home <- lm(stat_game_position_home$passes_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$passes_against)])
  tackles_against_line_1_home <- lm(stat_game_position_home$tackles_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles_against)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$tackles_against)]))
  tackles_against_line_2_home <- lm(stat_game_position_home$tackles_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles_against)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$tackles_against)]))
  tackles_against_line_3_home <- lm(stat_game_position_home$tackles_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles_against)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$tackles_against)]))
  tackles_against_line_4_home <- lm(stat_game_position_home$tackles_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles_against)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$tackles_against)]))
  tackles_against_line_5_home <- lm(stat_game_position_home$tackles_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles_against)]~abs(stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$tackles_against)]))
  
  
  stat_game_position$expected_tkls[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (tackles_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+tackles_line_1_home$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (tackles_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+tackles_line_2_home$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (tackles_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+tackles_line_3_home$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (tackles_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+tackles_line_4_home$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (tackles_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+tackles_line_5_home$coefficients[[1]]
  stat_game_position$tkls_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$tackles[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_tkls[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_passes[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (passes_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+passes_line_1_home$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (passes_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+passes_line_2_home$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (passes_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+passes_line_3_home$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (passes_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+passes_line_4_home$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (passes_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+passes_line_5_home$coefficients[[1]]
  stat_game_position$pass_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$passes[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_passes[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_shots[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (shots_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+shots_line_1_home$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (shots_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+shots_line_2_home$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (shots_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+shots_line_3_home$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (shots_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+shots_line_4_home$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (shots_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+shots_line_5_home$coefficients[[1]]
  stat_game_position$shots_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$shots[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_shots[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_sogs[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (shotsongoal_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+shotsongoal_line_1_home$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (shotsongoal_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+shotsongoal_line_2_home$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (shotsongoal_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+shotsongoal_line_3_home$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (shotsongoal_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+shotsongoal_line_4_home$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (shotsongoal_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+shotsongoal_line_5_home$coefficients[[1]]
  stat_game_position$sogs_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$shotsongoal[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_sogs[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_tkls_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (tackles_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+tackles_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (tackles_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+tackles_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (tackles_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+tackles_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (tackles_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+tackles_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (tackles_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+tackles_against_line_5_home$coefficients[[1]]
  stat_game_position$tkls_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$tackles_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_tkls_against[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_passes_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (passes_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+passes_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (passes_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+passes_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (passes_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+passes_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (passes_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+passes_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (passes_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+passes_against_line_5_home$coefficients[[1]]
  stat_game_position$pass_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$passes_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_passes_against[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_shots_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (shots_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+shots_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (shots_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+shots_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (shots_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+shots_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (shots_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+shots_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (shots_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+shots_against_line_5_home$coefficients[[1]]
  stat_game_position$shots_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$shots_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_shots_against[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_sogs_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (sogs_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+sogs_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (sogs_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+sogs_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (sogs_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+sogs_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (sogs_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+sogs_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (sogs_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+sogs_against_line_5_home$coefficients[[1]]
  stat_game_position$sogs_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$sogs_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_sogs_against[stat_game_position$is_home_team==TRUE]
  
  
  stat_game_position_away <- stat_game_position[which(stat_game_position$is_home_team==FALSE),]
  
  
  shots_line_1_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots)])
  shots_line_2_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots)])
  shots_line_3_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots)])
  shots_line_4_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots)])
  shots_line_5_away <- lm(stat_game_position_away$shots[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots)])
  shotsongoal_line_1_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shotsongoal)])
  shotsongoal_line_2_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shotsongoal)])
  shotsongoal_line_3_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shotsongoal)])
  shotsongoal_line_4_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shotsongoal)])
  shotsongoal_line_5_away <- lm(stat_game_position_away$shotsongoal[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shotsongoal)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shotsongoal)])
  passes_line_1_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes)])
  passes_line_2_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes)])
  passes_line_3_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes)])
  passes_line_4_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes)])
  passes_line_5_away <- lm(stat_game_position_away$passes[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes)])
  tackles_line_1_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles)]))
  tackles_line_2_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles)]))
  tackles_line_3_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles)]))
  tackles_line_4_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles)]))
  tackles_line_5_away <- lm(stat_game_position_away$tackles[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles)]))
  
  shots_against_line_1_away <- lm(stat_game_position_away$shots_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$shots_against)])
  shots_against_line_2_away <- lm(stat_game_position_away$shots_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$shots_against)])
  shots_against_line_3_away <- lm(stat_game_position_away$shots_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$shots_against)])
  shots_against_line_4_away <- lm(stat_game_position_away$shots_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$shots_against)])
  shots_against_line_5_away <- lm(stat_game_position_away$shots_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$shots_against)])
  sogs_against_line_1_away <- lm(stat_game_position_away$sogs_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$sogs_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$sogs_against)])
  sogs_against_line_2_away <- lm(stat_game_position_away$sogs_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$sogs_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$sogs_against)])
  sogs_against_line_3_away <- lm(stat_game_position_away$sogs_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$sogs_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$sogs_against)])
  sogs_against_line_4_away <- lm(stat_game_position_away$sogs_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$sogs_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$sogs_against)])
  sogs_against_line_5_away <- lm(stat_game_position_away$sogs_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$sogs_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$sogs_against)])
  passes_against_line_1_away <- lm(stat_game_position_away$passes_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$passes_against)])
  passes_against_line_2_away <- lm(stat_game_position_away$passes_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$passes_against)])
  passes_against_line_3_away <- lm(stat_game_position_away$passes_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$passes_against)])
  passes_against_line_4_away <- lm(stat_game_position_away$passes_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$passes_against)])
  passes_against_line_5_away <- lm(stat_game_position_away$passes_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$passes_against)])
  tackles_against_line_1_away <- lm(stat_game_position_away$tackles_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles_against)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$tackles_against)]))
  tackles_against_line_2_away <- lm(stat_game_position_away$tackles_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles_against)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$tackles_against)]))
  tackles_against_line_3_away <- lm(stat_game_position_away$tackles_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles_against)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$tackles_against)]))
  tackles_against_line_4_away <- lm(stat_game_position_away$tackles_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles_against)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$tackles_against)]))
  tackles_against_line_5_away <- lm(stat_game_position_away$tackles_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles_against)]~abs(stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$tackles_against)]))
  
  
  stat_game_position$expected_tkls[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (tackles_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+tackles_line_1_away$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (tackles_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+tackles_line_2_away$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (tackles_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+tackles_line_3_away$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (tackles_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+tackles_line_4_away$coefficients[[1]]
  stat_game_position$expected_tkls[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (tackles_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+tackles_line_5_away$coefficients[[1]]
  stat_game_position$tkls_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$tackles[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_tkls[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_passes[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (passes_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+passes_line_1_away$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (passes_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+passes_line_2_away$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (passes_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+passes_line_3_away$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (passes_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+passes_line_4_away$coefficients[[1]]
  stat_game_position$expected_passes[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (passes_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+passes_line_5_away$coefficients[[1]]
  stat_game_position$pass_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$passes[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_passes[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_shots[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (shots_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+shots_line_1_away$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (shots_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+shots_line_2_away$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (shots_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+shots_line_3_away$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (shots_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+shots_line_4_away$coefficients[[1]]
  stat_game_position$expected_shots[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (shots_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+shots_line_5_away$coefficients[[1]]
  stat_game_position$shots_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$shots[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_shots[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_sogs[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (shotsongoal_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+shotsongoal_line_1_away$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (shotsongoal_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+shotsongoal_line_2_away$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (shotsongoal_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+shotsongoal_line_3_away$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (shotsongoal_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+shotsongoal_line_4_away$coefficients[[1]]
  stat_game_position$expected_sogs[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (shotsongoal_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+shotsongoal_line_5_away$coefficients[[1]]
  stat_game_position$sogs_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$shotsongoal[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_sogs[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_tkls_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (tackles_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+tackles_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (tackles_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+tackles_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (tackles_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+tackles_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (tackles_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+tackles_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_tkls_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (tackles_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+tackles_against_line_5_away$coefficients[[1]]
  stat_game_position$tkls_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$tackles_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_tkls_against[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_passes_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (passes_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+passes_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (passes_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+passes_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (passes_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+passes_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (passes_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+passes_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_passes_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (passes_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+passes_against_line_5_away$coefficients[[1]]
  stat_game_position$pass_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$passes_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_passes_against[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_shots_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (shots_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+shots_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (shots_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+shots_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (shots_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+shots_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (shots_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+shots_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_shots_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (shots_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+shots_against_line_5_away$coefficients[[1]]
  stat_game_position$shots_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$shots_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_shots_against[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_sogs_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (sogs_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+sogs_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (sogs_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+sogs_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (sogs_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+sogs_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (sogs_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+sogs_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_sogs_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (sogs_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+sogs_against_line_5_away$coefficients[[1]]
  stat_game_position$sogs_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$sogs_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_sogs_against[stat_game_position$is_home_team==FALSE]
  
  xg_line_1_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals)])
  xg_line_2_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals)])
  xg_line_3_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals)])
  xg_line_4_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals)])
  xg_line_5_home <- lm(stat_game_position_home$expected_goals[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals)])
  
  xgot_line_1_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_goals_ot)])
  xgot_line_2_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_goals_ot)])
  xgot_line_3_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_goals_ot)])
  xgot_line_4_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_goals_ot)])
  xgot_line_5_home <- lm(stat_game_position_home$expected_goals_ot[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals_ot)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_goals_ot)])
  
  xa_line_1_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$expected_ast)])
  xa_line_2_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$expected_ast)])
  xa_line_3_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$expected_ast)])
  xa_line_4_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$expected_ast)])
  xa_line_5_home <- lm(stat_game_position_home$expected_ast[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_ast)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$expected_ast)])
  
  xg_against_line_1_home <- lm(stat_game_position_home$xg_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xg_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xg_against)])
  xg_against_line_2_home <- lm(stat_game_position_home$xg_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xg_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xg_against)])
  xg_against_line_3_home <- lm(stat_game_position_home$xg_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xg_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xg_against)])
  xg_against_line_4_home <- lm(stat_game_position_home$xg_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xg_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xg_against)])
  xg_against_line_5_home <- lm(stat_game_position_home$xg_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xg_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xg_against)])
  
  xgot_against_line_1_home <- lm(stat_game_position_home$xgot_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xgot_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xgot_against)])
  xgot_against_line_2_home <- lm(stat_game_position_home$xgot_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xgot_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xgot_against)])
  xgot_against_line_3_home <- lm(stat_game_position_home$xgot_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xgot_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xgot_against)])
  xgot_against_line_4_home <- lm(stat_game_position_home$xgot_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xgot_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xgot_against)])
  xgot_against_line_5_home <- lm(stat_game_position_home$xgot_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xgot_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xgot_against)])
  
  xa_against_line_1_home <- lm(stat_game_position_home$xa_against[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xa_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==1&!is.na(stat_game_position_home$xa_against)])
  xa_against_line_2_home <- lm(stat_game_position_home$xa_against[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xa_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==2&!is.na(stat_game_position_home$xa_against)])
  xa_against_line_3_home <- lm(stat_game_position_home$xa_against[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xa_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==3&!is.na(stat_game_position_home$xa_against)])
  xa_against_line_4_home <- lm(stat_game_position_home$xa_against[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xa_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==4&!is.na(stat_game_position_home$xa_against)])
  xa_against_line_5_home <- lm(stat_game_position_home$xa_against[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xa_against)]~stat_game_position_home$avg_elo_dif[stat_game_position_home$position_row==5&!is.na(stat_game_position_home$xa_against)])
  
  stat_game_position$expected_xg[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xg_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xg_line_1_home$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xg_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xg_line_2_home$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xg_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xg_line_3_home$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xg_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xg_line_4_home$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xg_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xg_line_5_home$coefficients[[1]]
  stat_game_position$xg_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$expected_goals[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xg[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_xgot[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xgot_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xgot_line_1_home$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xgot_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xgot_line_2_home$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xgot_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xgot_line_3_home$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xgot_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xgot_line_4_home$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xgot_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xgot_line_5_home$coefficients[[1]]
  stat_game_position$xgot_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$expected_goals_ot[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xgot[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_xa[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xa_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xa_line_1_home$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xa_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xa_line_2_home$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xa_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xa_line_3_home$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xa_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xa_line_4_home$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xa_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xa_line_5_home$coefficients[[1]]
  stat_game_position$xa_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$expected_ast[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xa[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_xg_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xg_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xg_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xg_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xg_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xg_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xg_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xg_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xg_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xg_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xg_against_line_5_home$coefficients[[1]]
  stat_game_position$xg_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$xg_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xg_against[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_xgot_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xgot_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xgot_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xgot_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xgot_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xgot_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xgot_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xgot_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xgot_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xgot_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xgot_against_line_5_home$coefficients[[1]]
  stat_game_position$xgot_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$xgot_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xgot_against[stat_game_position$is_home_team==TRUE]
  
  stat_game_position$expected_xa_against[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE] <- (xa_against_line_1_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==TRUE])+xa_against_line_1_home$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE] <- (xa_against_line_2_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==TRUE])+xa_against_line_2_home$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE] <- (xa_against_line_3_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==TRUE])+xa_against_line_3_home$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE] <- (xa_against_line_4_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==TRUE])+xa_against_line_4_home$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE] <- (xa_against_line_5_home$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==TRUE])+xa_against_line_5_home$coefficients[[1]]
  stat_game_position$xa_against_minus_xp[stat_game_position$is_home_team==TRUE] <- stat_game_position$xa_against[stat_game_position$is_home_team==TRUE]-stat_game_position$expected_xa_against[stat_game_position$is_home_team==TRUE]
  
  xg_line_1_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals)])
  xg_line_2_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals)])
  xg_line_3_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals)])
  xg_line_4_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals)])
  xg_line_5_away <- lm(stat_game_position_away$expected_goals[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals)])
  
  xgot_line_1_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_goals_ot)])
  xgot_line_2_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_goals_ot)])
  xgot_line_3_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_goals_ot)])
  xgot_line_4_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_goals_ot)])
  xgot_line_5_away <- lm(stat_game_position_away$expected_goals_ot[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals_ot)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_goals_ot)])
  
  xa_line_1_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$expected_ast)])
  xa_line_2_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$expected_ast)])
  xa_line_3_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$expected_ast)])
  xa_line_4_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$expected_ast)])
  xa_line_5_away <- lm(stat_game_position_away$expected_ast[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_ast)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$expected_ast)])
  
  xg_against_line_1_away <- lm(stat_game_position_away$xg_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xg_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xg_against)])
  xg_against_line_2_away <- lm(stat_game_position_away$xg_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xg_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xg_against)])
  xg_against_line_3_away <- lm(stat_game_position_away$xg_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xg_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xg_against)])
  xg_against_line_4_away <- lm(stat_game_position_away$xg_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xg_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xg_against)])
  xg_against_line_5_away <- lm(stat_game_position_away$xg_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xg_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xg_against)])
  
  xgot_against_line_1_away <- lm(stat_game_position_away$xgot_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xgot_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xgot_against)])
  xgot_against_line_2_away <- lm(stat_game_position_away$xgot_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xgot_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xgot_against)])
  xgot_against_line_3_away <- lm(stat_game_position_away$xgot_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xgot_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xgot_against)])
  xgot_against_line_4_away <- lm(stat_game_position_away$xgot_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xgot_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xgot_against)])
  xgot_against_line_5_away <- lm(stat_game_position_away$xgot_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xgot_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xgot_against)])
  
  xa_against_line_1_away <- lm(stat_game_position_away$xa_against[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xa_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==1&!is.na(stat_game_position_away$xa_against)])
  xa_against_line_2_away <- lm(stat_game_position_away$xa_against[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xa_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==2&!is.na(stat_game_position_away$xa_against)])
  xa_against_line_3_away <- lm(stat_game_position_away$xa_against[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xa_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==3&!is.na(stat_game_position_away$xa_against)])
  xa_against_line_4_away <- lm(stat_game_position_away$xa_against[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xa_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==4&!is.na(stat_game_position_away$xa_against)])
  xa_against_line_5_away <- lm(stat_game_position_away$xa_against[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xa_against)]~stat_game_position_away$avg_elo_dif[stat_game_position_away$position_row==5&!is.na(stat_game_position_away$xa_against)])
  
  stat_game_position$expected_xg[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xg_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xg_line_1_away$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xg_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xg_line_2_away$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xg_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xg_line_3_away$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xg_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xg_line_4_away$coefficients[[1]]
  stat_game_position$expected_xg[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xg_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xg_line_5_away$coefficients[[1]]
  stat_game_position$xg_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$expected_goals[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xg[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_xgot[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xgot_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xgot_line_1_away$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xgot_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xgot_line_2_away$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xgot_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xgot_line_3_away$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xgot_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xgot_line_4_away$coefficients[[1]]
  stat_game_position$expected_xgot[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xgot_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xgot_line_5_away$coefficients[[1]]
  stat_game_position$xgot_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$expected_goals_ot[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xgot[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_xa[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xa_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xa_line_1_away$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xa_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xa_line_2_away$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xa_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xa_line_3_away$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xa_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xa_line_4_away$coefficients[[1]]
  stat_game_position$expected_xa[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xa_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xa_line_5_away$coefficients[[1]]
  stat_game_position$xa_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$expected_ast[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xa[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_xg_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xg_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xg_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xg_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xg_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xg_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xg_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xg_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xg_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_xg_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xg_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xg_against_line_5_away$coefficients[[1]]
  stat_game_position$xg_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$xg_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xg_against[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_xgot_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xgot_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xgot_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xgot_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xgot_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xgot_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xgot_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xgot_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xgot_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_xgot_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xgot_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xgot_against_line_5_away$coefficients[[1]]
  stat_game_position$xgot_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$xgot_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xgot_against[stat_game_position$is_home_team==FALSE]
  
  stat_game_position$expected_xa_against[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE] <- (xa_against_line_1_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==1&stat_game_position$is_home_team==FALSE])+xa_against_line_1_away$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE] <- (xa_against_line_2_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==2&stat_game_position$is_home_team==FALSE])+xa_against_line_2_away$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE] <- (xa_against_line_3_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==3&stat_game_position$is_home_team==FALSE])+xa_against_line_3_away$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE] <- (xa_against_line_4_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==4&stat_game_position$is_home_team==FALSE])+xa_against_line_4_away$coefficients[[1]]
  stat_game_position$expected_xa_against[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE] <- (xa_against_line_5_away$coefficients[[2]]*stat_game_position$avg_elo_dif[stat_game_position$position_row==5&stat_game_position$is_home_team==FALSE])+xa_against_line_5_away$coefficients[[1]]
  stat_game_position$xa_against_minus_xp[stat_game_position$is_home_team==FALSE] <- stat_game_position$xa_against[stat_game_position$is_home_team==FALSE]-stat_game_position$expected_xa_against[stat_game_position$is_home_team==FALSE]
  
  
  list(stat_game_position,
       tackles_line_1_home,
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
       tackles_against_line_1_home,
       tackles_against_line_2_home,
       tackles_against_line_3_home,
       tackles_against_line_4_home,
       tackles_against_line_5_home,
       passes_against_line_1_home,
       passes_against_line_2_home,
       passes_against_line_3_home,
       passes_against_line_4_home,
       passes_against_line_5_home,
       shots_against_line_1_home,
       shots_against_line_2_home,
       shots_against_line_3_home,
       shots_against_line_4_home,
       shots_against_line_5_home,
       sogs_against_line_1_home,
       sogs_against_line_2_home,
       sogs_against_line_3_home,
       sogs_against_line_4_home,
       sogs_against_line_5_home,
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
       tackles_against_line_1_away,
       tackles_against_line_2_away,
       tackles_against_line_3_away,
       tackles_against_line_4_away,
       tackles_against_line_5_away,
       passes_against_line_1_away,
       passes_against_line_2_away,
       passes_against_line_3_away,
       passes_against_line_4_away,
       passes_against_line_5_away,
       shots_against_line_1_away,
       shots_against_line_2_away,
       shots_against_line_3_away,
       shots_against_line_4_away,
       shots_against_line_5_away,
       sogs_against_line_1_away,
       sogs_against_line_2_away,
       sogs_against_line_3_away,
       sogs_against_line_4_away,
       sogs_against_line_5_away,
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
       xg_against_line_1_home,
       xg_against_line_2_home,
       xg_against_line_3_home,
       xg_against_line_4_home,
       xg_against_line_5_home,
       xgot_against_line_1_home,
       xgot_against_line_2_home,
       xgot_against_line_3_home,
       xgot_against_line_4_home,
       xgot_against_line_5_home,
       xa_against_line_1_home,
       xa_against_line_2_home,
       xa_against_line_3_home,
       xa_against_line_4_home,
       xa_against_line_5_home,
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
       xa_line_5_away,
       xg_against_line_1_away,
       xg_against_line_2_away,
       xg_against_line_3_away,
       xg_against_line_4_away,
       xg_against_line_5_away,
       xgot_against_line_1_away,
       xgot_against_line_2_away,
       xgot_against_line_3_away,
       xgot_against_line_4_away,
       xgot_against_line_5_away,
       xa_against_line_1_away,
       xa_against_line_2_away,
       xa_against_line_3_away,
       xa_against_line_4_away,
       xa_against_line_5_away
       
  )
  
  
}

get_player_data <- function(wc_q_stat){
  
  stat_game <- wc_q_stat %>%
    group_by(team_name, league, match_id, is_home_team) %>%
    summarise(games = sum(stats_minutes_played[stats_minutes_played!=0], na.rm = T),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T),
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T),
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T),
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T),
              avg_elo_dif = mean(elo_dif))
  
  
  
  if(length(which(stat_game$games<800))==0){
    match_ids_remove <- 0
  }else{
    match_ids_remove <- stat_game$match_id[which(stat_game$games<800)]
  }
  
  rm(stat_game)
  
  if(match_ids_remove[1]==0){
    
  }else{
    wc_q_stat <-  wc_q_stat[-which(wc_q_stat$match_id %in% match_ids_remove),]
  }
  
  stat_afc <- wc_q_stat %>%
    group_by(page_url, team_name, position_row) %>%
    summarise(page_url_2 = names(which.max(table(page_url_2))),
              minutes = sum(stats_minutes_played, na.rm = T),
              games = length(stats_minutes_played[stats_minutes_played!=0]),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T),
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T),
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T),
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T),
              expected_goals_against = sum(stats_x_got_faced, na.rm = T),
              avg_elo_dif = mean(elo_dif))
  
  stat_afc$sog_90min <- 90*stat_afc$shotsongoal/stat_afc$minutes
  stat_afc$shots_90min <- 90*stat_afc$shots/stat_afc$minutes
  stat_afc$tkl_90min <- 90*stat_afc$tackles/stat_afc$minutes
  stat_afc$pass_90min <- 90*stat_afc$passes/stat_afc$minutes
  stat_afc$xg_90min <- 90*stat_afc$expected_goals/stat_afc$minutes
  stat_afc$xgot_90min <- 90*stat_afc$expected_goals_ot/stat_afc$minutes
  stat_afc$xa_90min <- 90*stat_afc$expected_ast/stat_afc$minutes
  
  
  
  stat_afc
  
}


get_player_data_no_pos <- function(wc_q_stat){
  
  
  stat_afc <- wc_q_stat %>%
    group_by(page_url, team_name) %>%
    summarise(page_url_2 = names(which.max(table(page_url_2))),
              usual_position = names(which.max(table(position_row))),
              minutes = sum(stats_minutes_played, na.rm = T),
              games = length(stats_minutes_played[stats_minutes_played!=0]),
              stat_shots = sum(stats_minutes_played[!is.na(stats_total_shots)], na.rm = T),
              shots = sum(stats_total_shots, na.rm = T),
              stat_shotsongoal = sum(stats_minutes_played[!is.na(stats_shot_accuracy)], na.rm = T),
              shotsongoal = sum(stats_shot_accuracy, na.rm = T),
              stat_tackles = sum(stats_minutes_played[!is.na(stats_tackles_won)], na.rm = T),
              tackles = sum(stats_tackles_won, na.rm = T),
              stat_goals = sum(stats_minutes_played[!is.na(stats_goals)], na.rm = T),
              goals = sum(stats_goals, na.rm = T),
              stat_assists = sum(stats_minutes_played[!is.na(stats_assists)], na.rm = T),
              assists = sum(stats_assists, na.rm = T),
              stat_passes = sum(stats_minutes_played[!is.na(stats_accurate_passes)], na.rm = T),
              passes = sum(stats_accurate_passes, na.rm = T),
              expected_goals = sum(stats_expected_goals_x_g, na.rm = T),
              expected_goals_ot = sum(stats_expected_goals_on_target_x_got, na.rm = T),
              expected_ast = sum(stats_expected_assists_x_a, na.rm = T),
              expected_goals_against = sum(stats_x_got_faced, na.rm = T),
              avg_elo_dif = mean(elo_dif))
  
  stat_afc$sog_90min <- 90*stat_afc$shotsongoal/stat_afc$minutes
  stat_afc$shots_90min <- 90*stat_afc$shots/stat_afc$minutes
  stat_afc$tkl_90min <- 90*stat_afc$tackles/stat_afc$minutes
  stat_afc$pass_90min <- 90*stat_afc$passes/stat_afc$minutes
  stat_afc$xg_90min <- 90*stat_afc$expected_goals/stat_afc$minutes
  stat_afc$xgot_90min <- 90*stat_afc$expected_goals_ot/stat_afc$minutes
  stat_afc$xa_90min <- 90*stat_afc$expected_ast/stat_afc$minutes
  
  stat_afc
  
}

#f_teams_f <- e_teams_e

lineup_data <- function(date,stat_game_position,f_teams_f,league_name,ccode){
  
  
  
  l <- fotmob_get_league_ids(cached = FALSE)
  l <- data.frame(l)
  fixtures_2 <- fotmob_get_matches_by_date(date = date)
  fixtures_2 <- fixtures_2[which(grepl(league_name,fixtures_2$name)&fixtures_2$ccode==ccode),]
  fixtures_2$home_name <- fixtures_2$home_long_name
  fixtures_2$away_name <- fixtures_2$away_long_name
  game_no_2 <- c(1:length(fixtures_2$ccode))
  lineup_list <- vector(("list"),4)
  for(b in game_no_2){
    
    
    fotmob_get_match_info(fixtures_2$match_id[b])
    
    t <- read_json(paste0("https://www.fotmob.com/api/matchDetails?matchId=",fixtures_2$match_id[b],"&ccode3=USA&timezone=America%2FLos_Angeles&refresh=true&includeBuzzTab=false&acceptLanguage=en-US"))
    
    
    
    if(is.element(t$header$teams[[1]]$name,team_df$team_1)){
      t$header$teams[[1]]$name <- team_df$team_2[which(team_df$team_1==t$header$teams[[1]]$name)]
    }
    
    if(is.element(t$header$teams[[2]]$name,team_df$team_1)){
      t$header$teams[[2]]$name <- team_df$team_2[which(team_df$team_1==t$header$teams[[2]]$name)]
    }
    
    
    for(i in 1:length(team_df$team_1)){
      fixtures_2$home_name[fixtures_2$home_name==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$away_name[fixtures_2$away_name==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$home_long_name[fixtures_2$home_long_name==team_df$team_1[i]] <- team_df$team_2[i]
      fixtures_2$away_long_name[fixtures_2$away_long_name==team_df$team_1[i]] <- team_df$team_2[i]
    }
    
    
    
    if(length(t$content$lineup$lineup[[1]]$players)==0){
      
      
      if(t$content$lineup$simpleLineup==FALSE){
        
        
        games <- length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==0])
        
        players_0_1 <- 1      
        players_1_1 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==1],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==1]))))
        players_2_1 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==2],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==2]))))
        players_3_1 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==3],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==3]))))
        players_4_1 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==4],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==4]))))
        players_5_1 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==5],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[1]]$name&stat_game_position$position_row==5]))))
        
        games <- length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==0])
        
        
        players_0_2 <- 1      
        players_1_2 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==1],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==1]))))
        players_2_2 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==2],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==2]))))
        players_3_2 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==3],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==3]))))
        players_4_2 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==4],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==4]))))
        players_5_2 <- mean(c(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==5],
                              rep(0,games-length(stat_game_position$players[stat_game_position$team_name==t$header$teams[[2]]$name&stat_game_position$position_row==5]))))
        
        
        players_line1 <- round(c(players_0_1,
                                 players_1_1,
                                 players_2_1,
                                 players_3_1,
                                 players_4_1,
                                 players_5_1),0)
        
        df1 <- data.frame(position_row = c(0:(length(players_line1)-1)),players_line1)
        
        df1$dif <- df1$players_line1-c(players_0_1,
                                       players_1_1,
                                       players_2_1,
                                       players_3_1,
                                       players_4_1,
                                       players_5_1)
        
        if(sum(df1$players_line1)==12){
          df1$players_line1[which(df1$dif==max(df1$dif))[length(which(df1$dif==max(df1$dif)))]] <-  df1$players_line1[which(df1$dif==max(df1$dif))[length(which(df1$dif==max(df1$dif)))]]-1
        }
        
        if(sum(df1$players_line1)==10){
          df1$players_line1[which(-df1$dif==max(-df1$dif))[length(which(-df1$dif==max(-df1$dif)))]] <-  df1$players_line1[which(-df1$dif==max(-df1$dif))[length(which(-df1$dif==max(-df1$dif)))]]+1
        }
        
        
        players_line2 <- round(c(players_0_2,
                                 players_1_2,
                                 players_2_2,
                                 players_3_2,
                                 players_4_2,
                                 players_5_2),0)
        
        df2 <- data.frame(position_row = c(0:(length(players_line2)-1)),players_line2)
        
        df2$dif <- df2$players_line2-c(players_0_2,
                                       players_1_2,
                                       players_2_2,
                                       players_3_2,
                                       players_4_2,
                                       players_5_2)
        
        if(sum(df2$players_line2)==12){
          df2$players_line2[which(df2$dif==max(df2$dif))] <-  df2$players_line2[which(df2$dif==max(df2$dif))]-1
        }
        
        if(sum(df2$players_line2)==10){
          df2$players_line2[which(df2$dif==min(df2$dif))] <-  df2$players_line2[which(df2$dif==min(df2$dif))]+1
        }
        
        naplayers_1 <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[1]]
        naplayers_2 <- t$content$lineup$lineup[[1]]$nonAvailablePlayers[[2]]
        
        naplayers_1 <- lapply(naplayers_1, function(x) { x["name"] <- NULL; x })
        naplayers_1 <- lapply(naplayers_1, function(x) { x["naInfo"] <- NULL; x })
        naplayers_2 <- lapply(naplayers_2, function(x) { x["name"] <- NULL; x })
        naplayers_2 <- lapply(naplayers_2, function(x) { x["naInfo"] <- NULL; x })
        
        
        na_players_all <- data.frame(rbind(rbindlist(naplayers_1, fill = T),rbindlist(naplayers_2, fill = T), fill = T))
        
        if(length(which(  stat_afc$page_url %in% na_players_all$pageUrl ))==0){
          player_game_stats <- stat_afc
        }else{
          player_game_stats <- stat_afc[-which(  stat_afc$page_url %in% na_players_all$pageUrl ),]
        }
        
        
        starters_1 <- vector("list",length(df1$position_row))
        starters_2 <- vector("list",length(df1$position_row))
        for(i in 1:length(df1$position_row)){
          
          if(df1$players_line1[i]==0){
            
          }else{
            max_m <- player_game_stats$minutes[player_game_stats$position_row==df1$position_row[i]&player_game_stats$team_name==fixtures_2$home_long_name[b]][order(-player_game_stats$minutes[player_game_stats$position_row==df1$position_row[i]&player_game_stats$team_name==fixtures_2$home_long_name[b]])]
            
            max_m <- max_m[c(1:df1$players_line1[i])]
            
            for(j in 1:length(max_m)){
              starters_1[[i]]$player[j] <- player_game_stats$page_url[which(player_game_stats$minutes==max_m[j]&player_game_stats$position_row==df1$position_row[i]&player_game_stats$team_name==fixtures_2$home_long_name[b])]
              starters_1[[i]]$positionRow[j] <- df1$position_row[i]
              starters_1[[i]]$player_name[j] <- player_game_stats$page_url_2[which(player_game_stats$minutes==max_m[j]&player_game_stats$position_row==df1$position_row[i]&player_game_stats$team_name==fixtures_2$home_long_name[b])]
              
              player_game_stats <- player_game_stats[-which(player_game_stats$page_url==starters_1[[i]]$player[j]),]
            }
          }
          
          if(df2$players_line2[i]==0){
            
          }else{
            
            max_m <- player_game_stats$minutes[player_game_stats$position_row==df2$position_row[i]&player_game_stats$team_name==fixtures_2$away_long_name[b]][order(-player_game_stats$minutes[player_game_stats$position_row==df2$position_row[i]&player_game_stats$team_name==fixtures_2$away_long_name[b]])]
            
            max_m <- max_m[c(1:df2$players_line2[i])]
            
            for(j in 1:length(max_m)){
              starters_2[[i]]$player[j] <- player_game_stats$page_url[which(player_game_stats$minutes==max_m[j]&player_game_stats$position_row==df2$position_row[i]&player_game_stats$team_name==fixtures_2$away_long_name[b])]
              starters_2[[i]]$positionRow[j] <- df2$position_row[i]
              starters_2[[i]]$player_name[j] <- player_game_stats$page_url_2[which(player_game_stats$minutes==max_m[j]&player_game_stats$position_row==df2$position_row[i]&player_game_stats$team_name==fixtures_2$away_long_name[b])]
              
              player_game_stats <- player_game_stats[-which(player_game_stats$page_url==starters_2[[i]]$player[j]),]
              
            }
            
          }
          
          
          
          
        }
        
        
        lineup_1 <- rbindlist(starters_1)
        lineup_1$team <- t$header$teams[[1]]$name
        lineup_2 <- rbindlist(starters_2)
        lineup_2$team <- t$header$teams[[2]]$name
        
        lineup_list[[b]] <- bind_rows(lineup_1,lineup_2)
        lineup_list[[b]]$match_id <- t$content$matchFacts$matchId
        
        
      }else{
        
        
        lineup <- vector(("list"),length(t$content$lineup$lineup[[1]]$lineup$starting))
        for(a in 1:length(t$content$lineup$lineup[[1]]$lineup$starting)){
          t$content$lineup$lineup[[1]]$lineup$starting <- lapply(t$content$lineup$lineup[[1]]$lineup$starting, function(x) { x["name"] <- NULL; x })
          lineup[[a]] <- rbindlist(t$content$lineup$lineup[[1]]$lineup$starting, fill = T)
        }
        
        lineup_df <- rbindlist(lineup, fill = T)
        lineup_df$team <- fixtures_2$home_name[b]
        lineup_df$
        
        lineup_2 <- vector(("list"),length(t$content$lineup$lineup[[2]]$lineup$starting))
        for(a in 1:length(t$content$lineup$lineup[[2]]$lineup$starting)){
          t$content$lineup$lineup[[2]]$lineup$starting <- lapply(t$content$lineup$lineup[[2]]$lineup$starting, function(x) { x["name"] <- NULL; x })
          lineup[[a]] <- rbindlist(t$content$lineup$lineup[[2]]$lineup$starting, fill = T)
        }
        
        lineup_df_2 <- rbindlist(lineup_2, fill = T)
        lineup_df_2$team <- fixtures_2$away_name[b]
        
        lineup_list[[b]] <- bind_rows(lineup_df,lineup_df_2)
        lineup_list[[b]]$match_id <- t$content$matchFacts$matchId
        if(length(which(duplicated(lineup_list[[b]][,4])))==0){
          
        }else{
          lineup_list[[b]] <- lineup_list[[b]][-which(duplicated(lineup_list[[b]][,4])),]
        }
        
        lineup_list[[b]]$pageUrl2 <- lineup_list[[b]]$pageUrl
        for(i in 1:length(lineup_list[[b]]$pageUrl)){
          
          lineup_list[[b]]$pageUrl[i] <- paste0("/",strsplit(lineup_list[[b]]$pageUrl2[i],"/")[[1]][2],"/",strsplit(lineup_list[[b]]$pageUrl2[i],"/")[[1]][3],"/")
          
        }
        
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
      
      lineup_df$team <- fixtures_2$home_name[b]
      
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
      lineup_df_2$team <- fixtures_2$away_name[b]
      
      lineup_list[[b]] <- bind_rows(lineup_df,lineup_df_2)
      lineup_list[[b]]$match_id <- t$content$matchFacts$matchId
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
  
  day_lineups <- rbindlist(lineup_list, fill = T)
  
  
  
  day_lineups 
  
  
}

get_game_player_data <- function(day_lineups,shots_ranges_all,sog_ranges_all,pass_ranges_all,tackles_ranges_all,
                                 stat_afc, stat_game_position, 
                                 tackles_line_1_home,
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
                                 tackles_against_line_1_home,
                                 tackles_against_line_2_home,
                                 tackles_against_line_3_home,
                                 tackles_against_line_4_home,
                                 tackles_against_line_5_home,
                                 passes_against_line_1_home,
                                 passes_against_line_2_home,
                                 passes_against_line_3_home,
                                 passes_against_line_4_home,
                                 passes_against_line_5_home,
                                 shots_against_line_1_home,
                                 shots_against_line_2_home,
                                 shots_against_line_3_home,
                                 shots_against_line_4_home,
                                 shots_against_line_5_home,
                                 sogs_against_line_1_home,
                                 sogs_against_line_2_home,
                                 sogs_against_line_3_home,
                                 sogs_against_line_4_home,
                                 sogs_against_line_5_home, 
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
                                 tackles_against_line_1_away,
                                 tackles_against_line_2_away,
                                 tackles_against_line_3_away,
                                 tackles_against_line_4_away,
                                 tackles_against_line_5_away,
                                 passes_against_line_1_away,
                                 passes_against_line_2_away,
                                 passes_against_line_3_away,
                                 passes_against_line_4_away,
                                 passes_against_line_5_away,
                                 shots_against_line_1_away,
                                 shots_against_line_2_away,
                                 shots_against_line_3_away,
                                 shots_against_line_4_away,
                                 shots_against_line_5_away,
                                 sogs_against_line_1_away,
                                 sogs_against_line_2_away,
                                 sogs_against_line_3_away,
                                 sogs_against_line_4_away,
                                 sogs_against_line_5_away,
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
                                 xg_against_line_1_home,
                                 xg_against_line_2_home,
                                 xg_against_line_3_home,
                                 xg_against_line_4_home,
                                 xg_against_line_5_home,
                                 xgot_against_line_1_home,
                                 xgot_against_line_2_home,
                                 xgot_against_line_3_home,
                                 xgot_against_line_4_home,
                                 xgot_against_line_5_home,
                                 xa_against_line_1_home,
                                 xa_against_line_2_home,
                                 xa_against_line_3_home,
                                 xa_against_line_4_home,
                                 xa_against_line_5_home,
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
                                 xa_line_5_away,
                                 xg_against_line_1_away,
                                 xg_against_line_2_away,
                                 xg_against_line_3_away,
                                 xg_against_line_4_away,
                                 xg_against_line_5_away,
                                 xgot_against_line_1_away,
                                 xgot_against_line_2_away,
                                 xgot_against_line_3_away,
                                 xgot_against_line_4_away,
                                 xgot_against_line_5_away,
                                 xa_against_line_1_away,
                                 xa_against_line_2_away,
                                 xa_against_line_3_away,
                                 xa_against_line_4_away,
                                 xa_against_line_5_away,
                                 fixtures,
                                 e_teams){
  
  
  
  day_lineups$player <- day_lineups$pageUrl
  day_lineups$player_name <- day_lineups$pageUrl2
  
  day_lineups$shxpos <- 0
  day_lineups$sogxpos<- 0
  day_lineups$passxpos<- 0
  day_lineups$tklxpos<- 0
  day_lineups$xgxpos<- 0
  day_lineups$xgotxpos<- 0
  day_lineups$xaxpos<- 0
  day_lineups$shot_range_low <- 0
  day_lineups$shot_range_mid <- 0
  day_lineups$shot_range_high <- 0
  day_lineups$sog_range_low <- 0
  day_lineups$sog_range_mid <- 0
  day_lineups$sog_range_high <- 0
  day_lineups$pass_range_low <- 0
  day_lineups$pass_range_mid <- 0
  day_lineups$pass_range_high <- 0
  day_lineups$tackle_range_low <- 0
  day_lineups$tackle_range_mid <- 0
  day_lineups$tackle_range_high <- 0
  day_lineups$xg_range_low <- 0
  day_lineups$xg_range_mid <- 0
  day_lineups$xg_range_high <- 0
  day_lineups$xgot_range_low <- 0
  day_lineups$xgot_range_mid <- 0
  day_lineups$xgot_range_high <- 0
  day_lineups$xa_range_low <- 0
  day_lineups$xa_range_mid <- 0
  day_lineups$xa_range_high <- 0
  for(h in 1:length(day_lineups$player)){
    
    day_lineups$shot_range_low[h] <- shots_ranges_all$shot_ranges[shots_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$shot_range_mid[h] <- shots_ranges_all$shot_ranges[shots_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$shot_range_high[h] <- shots_ranges_all$shot_ranges[shots_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$sog_range_low[h] <- sog_ranges_all$sog_ranges[sog_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$sog_range_mid[h] <- sog_ranges_all$sog_ranges[sog_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$sog_range_high[h] <- sog_ranges_all$sog_ranges[sog_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$pass_range_low[h] <- pass_ranges_all$pass_ranges[pass_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$pass_range_mid[h] <- pass_ranges_all$pass_ranges[pass_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$pass_range_high[h] <- pass_ranges_all$pass_ranges[pass_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$tackle_range_low[h] <- tackles_ranges_all$tackle_ranges[tackles_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$tackle_range_mid[h] <- tackles_ranges_all$tackle_ranges[tackles_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$tackle_range_high[h] <- tackles_ranges_all$tackle_ranges[tackles_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$xg_range_low[h] <- xg_ranges_all$xg_ranges[xg_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$xg_range_mid[h] <- xg_ranges_all$xg_ranges[xg_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$xg_range_high[h] <- xg_ranges_all$xg_ranges[xg_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$xgot_range_low[h] <- xgot_ranges_all$xgot_ranges[xgot_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$xgot_range_mid[h] <- xgot_ranges_all$xgot_ranges[xgot_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$xgot_range_high[h] <- xgot_ranges_all$xgot_ranges[xgot_ranges_all$teams==day_lineups$team[h]][3]
    day_lineups$xa_range_low[h] <- xa_ranges_all$xa_ranges[xa_ranges_all$teams==day_lineups$team[h]][1]
    day_lineups$xa_range_mid[h] <- xa_ranges_all$xa_ranges[xa_ranges_all$teams==day_lineups$team[h]][2]
    day_lineups$xa_range_high[h] <- xa_ranges_all$xa_ranges[xa_ranges_all$teams==day_lineups$team[h]][3]
    
    if(length(stat_afc$stat_shots[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row])==0){
      
      day_lineups$minutes_shots[h] <- stat_afc$stat_shots[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_sogs[h] <- stat_afc$stat_shotsongoal[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_passes[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_tackles[h] <- stat_afc$stat_tackles[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_xg[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_xgot[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$minutes_xa[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      
      day_lineups$sp90[h] <- stat_afc$shots_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$sogp90[h] <- stat_afc$sog_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$pp90[h] <- stat_afc$pass_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$tp90[h] <- stat_afc$tkl_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$xgp90[h] <- stat_afc$xg_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$xgotp90[h] <- stat_afc$xgot_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      day_lineups$xap90[h] <- stat_afc$xa_90min[stat_afc$page_url==day_lineups$pageUrl[h]&max(stat_afc$minutes[stat_afc$page_url==day_lineups$pageUrl[h]])==stat_afc$minutes]
      
    }else{
      
      day_lineups$minutes_shots[h] <- stat_afc$stat_shots[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_sogs[h] <- stat_afc$stat_shotsongoal[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_passes[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_tackles[h] <- stat_afc$stat_tackles[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_xg[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_xgot[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$minutes_xa[h] <- stat_afc$stat_passes[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      
      
      day_lineups$sp90[h] <- stat_afc$shots_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$sogp90[h] <- stat_afc$sog_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$pp90[h] <- stat_afc$pass_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$tp90[h] <- stat_afc$tkl_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$xgp90[h] <- stat_afc$xg_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$xgotp90[h] <- stat_afc$xgot_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      day_lineups$xap90[h] <- stat_afc$xa_90min[stat_afc$page_url==day_lineups$pageUrl[h]&day_lineups$positionRow[h]==stat_afc$position_row]
      
      
    }
    
    if(day_lineups$positionRow[h]==0){
      
      next()
      
    }else{
      
      
      if(day_lineups$positionRow[h]==1){
        
        if(any(grepl(day_lineups$team[h],fixtures$team1))){
          
          sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(sh_pos_rate)){
            sh_pos_rate <- 0
          }
          
          if(is.nan(sh_against_pos_rate)){
            sh_against_pos_rate <- 0
          }
          
          
          day_lineups$shxpos[h] <- mean(c(shots_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_line_1_home$coefficients[[1]],
                                          shots_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_against_line_1_away$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
          
          sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(sog_pos_rate)){
            sog_pos_rate <- 0
          }
          
          if(is.nan(sog_against_pos_rate)){
            sog_against_pos_rate <- 0
          }
          
          day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shotsongoal_line_1_home$coefficients[[1]],
                                           sogs_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+sogs_against_line_1_away$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
          
          
          pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(pass_pos_rate)){
            pass_pos_rate <- 0
          }
          
          if(is.nan(pass_against_pos_rate)){
            pass_against_pos_rate <- 0
          }
          
          day_lineups$passxpos[h] <- mean(c(passes_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_line_1_home$coefficients[[1]],
                                            passes_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_against_line_1_away$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
          
          
          tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(tkl_pos_rate)){
            tkl_pos_rate <- 0
          }
          
          if(is.nan(tkl_against_pos_rate)){
            tkl_against_pos_rate <- 0
          }
          
          day_lineups$tklxpos[h] <- mean(c(tackles_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_line_1_home$coefficients[[1]],
                                           tackles_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_against_line_1_away$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
          
          xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xg_pos_rate)){
            xg_pos_rate <- 0
          }
          
          if(is.nan(xg_against_pos_rate)){
            xg_against_pos_rate <- 0
          }
          
          day_lineups$xgxpos[h] <- mean(c(xg_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_line_1_home$coefficients[[1]],
                                           xg_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_against_line_1_away$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
          
          xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xgot_pos_rate)){
            xgot_pos_rate <- 0
          }
          
          if(is.nan(xgot_against_pos_rate)){
            xgot_against_pos_rate <- 0
          }
          
          day_lineups$xgotxpos[h] <- mean(c(xgot_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_line_1_home$coefficients[[1]],
                                          xgot_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_against_line_1_away$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
          
          xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
          xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xa_pos_rate)){
            xa_pos_rate <- 0
          }
          
          if(is.nan(xa_against_pos_rate)){
            xa_against_pos_rate <- 0
          }
          
          day_lineups$xaxpos[h] <- mean(c(xa_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_line_1_home$coefficients[[1]],
                                            xa_against_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_against_line_1_away$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
          
          
          
        }else{
          
          sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = FALSE)
          sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
          
          if(is.nan(sh_pos_rate)){
            sh_pos_rate <- 0
          }
          
          if(is.nan(sh_against_pos_rate)){
            sh_against_pos_rate <- 0
          }
          
          day_lineups$shxpos[h] <- mean(c(shots_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_line_1_away$coefficients[[1]],
                                          shots_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_against_line_1_home$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
          
          sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = FALSE)
          sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
          
          if(is.nan(sog_pos_rate)){
            sog_pos_rate <- 0
          }
          
          if(is.nan(sog_against_pos_rate)){
            sog_against_pos_rate <- 0
          }
          
          day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shotsongoal_line_1_away$coefficients[[1]],
                                           sogs_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+sogs_against_line_1_home$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
          
          
          pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = FALSE)
          pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
          
          if(is.nan(pass_pos_rate)){
            pass_pos_rate <- 0
          }
          
          if(is.nan(pass_against_pos_rate)){
            pass_against_pos_rate <- 0
          }
          
          day_lineups$passxpos[h] <- mean(c(passes_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_line_1_away$coefficients[[1]],
                                            passes_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_against_line_1_home$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
          
          
          tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = FALSE)
          tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
          
          if(is.nan(tkl_pos_rate)){
            tkl_pos_rate <- 0
          }
          
          if(is.nan(tkl_against_pos_rate)){
            tkl_against_pos_rate <- 0
          }
          
          day_lineups$tklxpos[h] <- mean(c(tackles_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_line_1_away$coefficients[[1]],
                                           tackles_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_against_line_1_home$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
          
          xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
          xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xg_pos_rate)){
            xg_pos_rate <- 0
          }
          
          if(is.nan(xg_against_pos_rate)){
            xg_against_pos_rate <- 0
          }
          
          day_lineups$xgxpos[h] <- mean(c(xg_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_line_1_away$coefficients[[1]],
                                          xg_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_against_line_1_home$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
          
          xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
          xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xgot_pos_rate)){
            xgot_pos_rate <- 0
          }
          
          if(is.nan(xgot_against_pos_rate)){
            xgot_against_pos_rate <- 0
          }
          
          day_lineups$xgotxpos[h] <- mean(c(xgot_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_line_1_away$coefficients[[1]],
                                            xgot_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_against_line_1_home$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
          
          xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
          xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
          
          if(is.nan(xa_pos_rate)){
            xa_pos_rate <- 0
          }
          
          if(is.nan(xa_against_pos_rate)){
            xa_against_pos_rate <- 0
          }
          
          day_lineups$xaxpos[h] <- mean(c(xa_line_1_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_line_1_away$coefficients[[1]],
                                          xa_against_line_1_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_against_line_1_home$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
          
          
          
          
        }
        
        
      }else{
        
        
        if(day_lineups$positionRow[h]==2){
          
          if(any(grepl(day_lineups$team[h],fixtures$team1))){
            
            sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(sh_pos_rate)){
              sh_pos_rate <- 0
            }
            
            if(is.nan(sh_against_pos_rate)){
              sh_against_pos_rate <- 0
            }
            
            
            day_lineups$shxpos[h] <- mean(c(shots_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_line_2_home$coefficients[[1]],
                                            shots_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_against_line_2_away$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
            
            sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(sog_pos_rate)){
              sog_pos_rate <- 0
            }
            
            if(is.nan(sog_against_pos_rate)){
              sog_against_pos_rate <- 0
            }
            
            day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shotsongoal_line_2_home$coefficients[[1]],
                                             sogs_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+sogs_against_line_2_away$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
            
            
            pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(pass_pos_rate)){
              pass_pos_rate <- 0
            }
            
            if(is.nan(pass_against_pos_rate)){
              pass_against_pos_rate <- 0
            }
            
            day_lineups$passxpos[h] <- mean(c(passes_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_line_2_home$coefficients[[1]],
                                              passes_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_against_line_2_away$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
            
            
            tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(tkl_pos_rate)){
              tkl_pos_rate <- 0
            }
            
            if(is.nan(tkl_against_pos_rate)){
              tkl_against_pos_rate <- 0
            }
            
            day_lineups$tklxpos[h] <- mean(c(tackles_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_line_2_home$coefficients[[1]],
                                             tackles_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_against_line_2_away$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
            
            xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xg_pos_rate)){
              xg_pos_rate <- 0
            }
            
            if(is.nan(xg_against_pos_rate)){
              xg_against_pos_rate <- 0
            }
            
            day_lineups$xgxpos[h] <- mean(c(xg_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_line_2_home$coefficients[[1]],
                                            xg_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_against_line_2_away$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
            
            xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xgot_pos_rate)){
              xgot_pos_rate <- 0
            }
            
            if(is.nan(xgot_against_pos_rate)){
              xgot_against_pos_rate <- 0
            }
            
            day_lineups$xgotxpos[h] <- mean(c(xgot_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_line_2_home$coefficients[[1]],
                                              xgot_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_against_line_2_away$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
            
            xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
            xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xa_pos_rate)){
              xa_pos_rate <- 0
            }
            
            if(is.nan(xa_against_pos_rate)){
              xa_against_pos_rate <- 0
            }
            
            day_lineups$xaxpos[h] <- mean(c(xa_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_line_2_home$coefficients[[1]],
                                            xa_against_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_against_line_2_away$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
            
            
            
          }else{
            
            sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
            
            if(is.nan(sh_pos_rate)){
              sh_pos_rate <- 0
            }
            
            if(is.nan(sh_against_pos_rate)){
              sh_against_pos_rate <- 0
            }
            
            day_lineups$shxpos[h] <- mean(c(shots_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_line_2_away$coefficients[[1]],
                                            shots_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_against_line_2_home$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
            
            sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
            
            if(is.nan(sog_pos_rate)){
              sog_pos_rate <- 0
            }
            
            if(is.nan(sog_against_pos_rate)){
              sog_against_pos_rate <- 0
            }
            
            day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shotsongoal_line_2_away$coefficients[[1]],
                                             sogs_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+sogs_against_line_2_home$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
            
            
            pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
            
            if(is.nan(pass_pos_rate)){
              pass_pos_rate <- 0
            }
            
            if(is.nan(pass_against_pos_rate)){
              pass_against_pos_rate <- 0
            }
            
            day_lineups$passxpos[h] <- mean(c(passes_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_line_2_away$coefficients[[1]],
                                              passes_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_against_line_2_home$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
            
            
            tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
            
            if(is.nan(tkl_pos_rate)){
              tkl_pos_rate <- 0
            }
            
            if(is.nan(tkl_against_pos_rate)){
              tkl_against_pos_rate <- 0
            }
            
            day_lineups$tklxpos[h] <- mean(c(tackles_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_line_2_away$coefficients[[1]],
                                             tackles_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_against_line_2_home$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
            
            xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xg_pos_rate)){
              xg_pos_rate <- 0
            }
            
            if(is.nan(xg_against_pos_rate)){
              xg_against_pos_rate <- 0
            }
            
            day_lineups$xgxpos[h] <- mean(c(xg_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_line_2_away$coefficients[[1]],
                                            xg_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_against_line_2_home$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
            
            xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xgot_pos_rate)){
              xgot_pos_rate <- 0
            }
            
            if(is.nan(xgot_against_pos_rate)){
              xgot_against_pos_rate <- 0
            }
            
            day_lineups$xgotxpos[h] <- mean(c(xgot_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_line_2_away$coefficients[[1]],
                                              xgot_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_against_line_2_home$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
            
            xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
            xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
            
            if(is.nan(xa_pos_rate)){
              xa_pos_rate <- 0
            }
            
            if(is.nan(xa_against_pos_rate)){
              xa_against_pos_rate <- 0
            }
            
            day_lineups$xaxpos[h] <- mean(c(xa_line_2_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_line_2_away$coefficients[[1]],
                                            xa_against_line_2_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_against_line_2_home$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
            
            
            
            
          }
          
          
        }else{
          
          
          if(day_lineups$positionRow[h]==3){
            
            if(any(grepl(day_lineups$team[h],fixtures$team1))){
              
              sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(sh_pos_rate)){
                sh_pos_rate <- 0
              }
              
              if(is.nan(sh_against_pos_rate)){
                sh_against_pos_rate <- 0
              }
              
              
              day_lineups$shxpos[h] <- mean(c(shots_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_line_3_home$coefficients[[1]],
                                              shots_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_against_line_3_away$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
              
              sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(sog_pos_rate)){
                sog_pos_rate <- 0
              }
              
              if(is.nan(sog_against_pos_rate)){
                sog_against_pos_rate <- 0
              }
              
              day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shotsongoal_line_3_home$coefficients[[1]],
                                               sogs_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+sogs_against_line_3_away$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
              
              
              pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(pass_pos_rate)){
                pass_pos_rate <- 0
              }
              
              if(is.nan(pass_against_pos_rate)){
                pass_against_pos_rate <- 0
              }
              
              day_lineups$passxpos[h] <- mean(c(passes_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_line_3_home$coefficients[[1]],
                                                passes_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_against_line_3_away$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
              
              
              tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(tkl_pos_rate)){
                tkl_pos_rate <- 0
              }
              
              if(is.nan(tkl_against_pos_rate)){
                tkl_against_pos_rate <- 0
              }
              
              day_lineups$tklxpos[h] <- mean(c(tackles_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_line_3_home$coefficients[[1]],
                                               tackles_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_against_line_3_away$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
              
              xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xg_pos_rate)){
                xg_pos_rate <- 0
              }
              
              if(is.nan(xg_against_pos_rate)){
                xg_against_pos_rate <- 0
              }
              
              day_lineups$xgxpos[h] <- mean(c(xg_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_line_3_home$coefficients[[1]],
                                              xg_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_against_line_3_away$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
              
              xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xgot_pos_rate)){
                xgot_pos_rate <- 0
              }
              
              if(is.nan(xgot_against_pos_rate)){
                xgot_against_pos_rate <- 0
              }
              
              day_lineups$xgotxpos[h] <- mean(c(xgot_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_line_3_home$coefficients[[1]],
                                                xgot_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_against_line_3_away$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
              
              xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
              xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xa_pos_rate)){
                xa_pos_rate <- 0
              }
              
              if(is.nan(xa_against_pos_rate)){
                xa_against_pos_rate <- 0
              }
              
              day_lineups$xaxpos[h] <- mean(c(xa_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_line_3_home$coefficients[[1]],
                                              xa_against_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_against_line_3_away$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
              
              
              
            }else{
              
              sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
              
              if(is.nan(sh_pos_rate)){
                sh_pos_rate <- 0
              }
              
              if(is.nan(sh_against_pos_rate)){
                sh_against_pos_rate <- 0
              }
              
              day_lineups$shxpos[h] <- mean(c(shots_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_line_3_away$coefficients[[1]],
                                              shots_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_against_line_3_home$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
              
              sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
              
              if(is.nan(sog_pos_rate)){
                sog_pos_rate <- 0
              }
              
              if(is.nan(sog_against_pos_rate)){
                sog_against_pos_rate <- 0
              }
              
              day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shotsongoal_line_3_away$coefficients[[1]],
                                               sogs_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+sogs_against_line_3_home$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
              
              
              pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
              
              if(is.nan(pass_pos_rate)){
                pass_pos_rate <- 0
              }
              
              if(is.nan(pass_against_pos_rate)){
                pass_against_pos_rate <- 0
              }
              
              day_lineups$passxpos[h] <- mean(c(passes_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_line_3_away$coefficients[[1]],
                                                passes_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_against_line_3_home$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
              
              
              tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
              
              if(is.nan(tkl_pos_rate)){
                tkl_pos_rate <- 0
              }
              
              if(is.nan(tkl_against_pos_rate)){
                tkl_against_pos_rate <- 0
              }
              
              day_lineups$tklxpos[h] <- mean(c(tackles_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_line_3_away$coefficients[[1]],
                                               tackles_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_against_line_3_home$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
              
              
              xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xg_pos_rate)){
                xg_pos_rate <- 0
              }
              
              if(is.nan(xg_against_pos_rate)){
                xg_against_pos_rate <- 0
              }
              
              day_lineups$xgxpos[h] <- mean(c(xg_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_line_3_away$coefficients[[1]],
                                              xg_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_against_line_3_home$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
              
              xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xgot_pos_rate)){
                xgot_pos_rate <- 0
              }
              
              if(is.nan(xgot_against_pos_rate)){
                xgot_against_pos_rate <- 0
              }
              
              day_lineups$xgotxpos[h] <- mean(c(xgot_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_line_3_away$coefficients[[1]],
                                                xgot_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_against_line_3_home$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
              
              xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
              xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
              
              if(is.nan(xa_pos_rate)){
                xa_pos_rate <- 0
              }
              
              if(is.nan(xa_against_pos_rate)){
                xa_against_pos_rate <- 0
              }
              
              day_lineups$xaxpos[h] <- mean(c(xa_line_3_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_line_3_away$coefficients[[1]],
                                              xa_against_line_3_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_against_line_3_home$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
              
              
              
            }
            
            
          }else{
            
            
            if(day_lineups$positionRow[h]==4){
              
              if(any(grepl(day_lineups$team[h],fixtures$team1))){
                
                sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(sh_pos_rate)){
                  sh_pos_rate <- 0
                }
                
                if(is.nan(sh_against_pos_rate)){
                  sh_against_pos_rate <- 0
                }
                
                
                day_lineups$shxpos[h] <- mean(c(shots_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_line_4_home$coefficients[[1]],
                                                shots_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_against_line_4_away$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
                
                sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(sog_pos_rate)){
                  sog_pos_rate <- 0
                }
                
                if(is.nan(sog_against_pos_rate)){
                  sog_against_pos_rate <- 0
                }
                
                day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shotsongoal_line_4_home$coefficients[[1]],
                                                 sogs_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+sogs_against_line_4_away$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
                
                
                pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(pass_pos_rate)){
                  pass_pos_rate <- 0
                }
                
                if(is.nan(pass_against_pos_rate)){
                  pass_against_pos_rate <- 0
                }
                
                day_lineups$passxpos[h] <- mean(c(passes_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_line_4_home$coefficients[[1]],
                                                  passes_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_against_line_4_away$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
                
                
                tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(tkl_pos_rate)){
                  tkl_pos_rate <- 0
                }
                
                if(is.nan(tkl_against_pos_rate)){
                  tkl_against_pos_rate <- 0
                }
                
                day_lineups$tklxpos[h] <- mean(c(tackles_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_line_4_home$coefficients[[1]],
                                                 tackles_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_against_line_4_away$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
                
                
                xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xg_pos_rate)){
                  xg_pos_rate <- 0
                }
                
                if(is.nan(xg_against_pos_rate)){
                  xg_against_pos_rate <- 0
                }
                
                day_lineups$xgxpos[h] <- mean(c(xg_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_line_4_home$coefficients[[1]],
                                                xg_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_against_line_4_away$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
                
                xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xgot_pos_rate)){
                  xgot_pos_rate <- 0
                }
                
                if(is.nan(xgot_against_pos_rate)){
                  xgot_against_pos_rate <- 0
                }
                
                day_lineups$xgotxpos[h] <- mean(c(xgot_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_line_4_home$coefficients[[1]],
                                                  xgot_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_against_line_4_away$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
                
                xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xa_pos_rate)){
                  xa_pos_rate <- 0
                }
                
                if(is.nan(xa_against_pos_rate)){
                  xa_against_pos_rate <- 0
                }
                
                day_lineups$xaxpos[h] <- mean(c(xa_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_line_4_home$coefficients[[1]],
                                                xa_against_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_against_line_4_away$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
                
                
              }else{
                
                sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                
                if(is.nan(sh_pos_rate)){
                  sh_pos_rate <- 0
                }
                
                if(is.nan(sh_against_pos_rate)){
                  sh_against_pos_rate <- 0
                }
                
                day_lineups$shxpos[h] <- mean(c(shots_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_line_4_away$coefficients[[1]],
                                                shots_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_against_line_4_home$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
                
                sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                
                if(is.nan(sog_pos_rate)){
                  sog_pos_rate <- 0
                }
                
                if(is.nan(sog_against_pos_rate)){
                  sog_against_pos_rate <- 0
                }
                
                day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shotsongoal_line_4_away$coefficients[[1]],
                                                 sogs_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+sogs_against_line_4_home$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
                
                
                pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                
                if(is.nan(pass_pos_rate)){
                  pass_pos_rate <- 0
                }
                
                if(is.nan(pass_against_pos_rate)){
                  pass_against_pos_rate <- 0
                }
                
                day_lineups$passxpos[h] <- mean(c(passes_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_line_4_away$coefficients[[1]],
                                                  passes_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_against_line_4_home$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
                
                
                tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                
                if(is.nan(tkl_pos_rate)){
                  tkl_pos_rate <- 0
                }
                
                if(is.nan(tkl_against_pos_rate)){
                  tkl_against_pos_rate <- 0
                }
                
                day_lineups$tklxpos[h] <- mean(c(tackles_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_line_4_away$coefficients[[1]],
                                                 tackles_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_against_line_4_home$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
                
                
                xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xg_pos_rate)){
                  xg_pos_rate <- 0
                }
                
                if(is.nan(xg_against_pos_rate)){
                  xg_against_pos_rate <- 0
                }
                
                day_lineups$xgxpos[h] <- mean(c(xg_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_line_4_away$coefficients[[1]],
                                                xg_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_against_line_4_home$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
                
                xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xgot_pos_rate)){
                  xgot_pos_rate <- 0
                }
                
                if(is.nan(xgot_against_pos_rate)){
                  xgot_against_pos_rate <- 0
                }
                
                day_lineups$xgotxpos[h] <- mean(c(xgot_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_line_4_away$coefficients[[1]],
                                                  xgot_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_against_line_4_home$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
                
                xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                
                if(is.nan(xa_pos_rate)){
                  xa_pos_rate <- 0
                }
                
                if(is.nan(xa_against_pos_rate)){
                  xa_against_pos_rate <- 0
                }
                
                day_lineups$xaxpos[h] <- mean(c(xa_line_4_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_line_4_away$coefficients[[1]],
                                                xa_against_line_4_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_against_line_4_home$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
                
                
                
              }
              
              
              
              
              
              
            }else{
              
              
              if(day_lineups$positionRow[h]==5){
                
                if(any(grepl(day_lineups$team[h],fixtures$team1))){
                  
                  sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(sh_pos_rate)){
                    sh_pos_rate <- 0
                  }
                  
                  if(is.nan(sh_against_pos_rate)){
                    sh_against_pos_rate <- 0
                  }
                  
                  
                  day_lineups$shxpos[h] <- mean(c(shots_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_line_5_home$coefficients[[1]],
                                                  shots_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_against_line_5_away$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
                  
                  sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(sog_pos_rate)){
                    sog_pos_rate <- 0
                  }
                  
                  if(is.nan(sog_against_pos_rate)){
                    sog_against_pos_rate <- 0
                  }
                  
                  day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shotsongoal_line_5_home$coefficients[[1]],
                                                   sogs_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+sogs_against_line_5_away$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
                  
                  
                  pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(pass_pos_rate)){
                    pass_pos_rate <- 0
                  }
                  
                  if(is.nan(pass_against_pos_rate)){
                    pass_against_pos_rate <- 0
                  }
                  
                  day_lineups$passxpos[h] <- mean(c(passes_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_line_5_home$coefficients[[1]],
                                                    passes_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_against_line_5_away$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
                  
                  
                  tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(tkl_pos_rate)){
                    tkl_pos_rate <- 0
                  }
                  
                  if(is.nan(tkl_against_pos_rate)){
                    tkl_against_pos_rate <- 0
                  }
                  
                  day_lineups$tklxpos[h] <- mean(c(tackles_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_line_5_home$coefficients[[1]],
                                                   tackles_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_against_line_5_away$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
                  
                  xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xg_pos_rate)){
                    xg_pos_rate <- 0
                  }
                  
                  if(is.nan(xg_against_pos_rate)){
                    xg_against_pos_rate <- 0
                  }
                  
                  day_lineups$xgxpos[h] <- mean(c(xg_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_line_5_home$coefficients[[1]],
                                                  xg_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_against_line_5_away$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
                  
                  xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xgot_pos_rate)){
                    xgot_pos_rate <- 0
                  }
                  
                  if(is.nan(xgot_against_pos_rate)){
                    xgot_against_pos_rate <- 0
                  }
                  
                  day_lineups$xgotxpos[h] <- mean(c(xgot_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_line_5_home$coefficients[[1]],
                                                    xgot_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_against_line_5_away$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
                  
                  xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE], na.rm = TRUE)
                  xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xa_pos_rate)){
                    xa_pos_rate <- 0
                  }
                  
                  if(is.nan(xa_against_pos_rate)){
                    xa_against_pos_rate <- 0
                  }
                  
                  day_lineups$xaxpos[h] <- mean(c(xa_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_line_5_home$coefficients[[1]],
                                                  xa_against_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_against_line_5_away$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
                  
                  
                  
                }else{
                  
                  sh_pos_rate <- mean(stat_game_position$shots_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  sh_against_pos_rate <- mean(stat_game_position$shots_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                  
                  if(is.nan(sh_pos_rate)){
                    sh_pos_rate <- 0
                  }
                  
                  if(is.nan(sh_against_pos_rate)){
                    sh_against_pos_rate <- 0
                  }
                  
                  day_lineups$shxpos[h] <- mean(c(shots_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shots_line_5_away$coefficients[[1]],
                                                  shots_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+shots_against_line_5_home$coefficients[[1]]))+sh_pos_rate+sh_against_pos_rate
                  
                  sog_pos_rate <- mean(stat_game_position$sogs_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  sog_against_pos_rate <- mean(stat_game_position$sogs_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                  
                  if(is.nan(sog_pos_rate)){
                    sog_pos_rate <- 0
                  }
                  
                  if(is.nan(sog_against_pos_rate)){
                    sog_against_pos_rate <- 0
                  }
                  
                  day_lineups$sogxpos[h] <- mean(c(shotsongoal_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+shotsongoal_line_5_away$coefficients[[1]],
                                                   sogs_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+sogs_against_line_5_home$coefficients[[1]]))+sog_pos_rate+sog_against_pos_rate
                  
                  
                  pass_pos_rate <- mean(stat_game_position$pass_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  pass_against_pos_rate <- mean(stat_game_position$pass_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                  
                  if(is.nan(pass_pos_rate)){
                    pass_pos_rate <- 0
                  }
                  
                  if(is.nan(pass_against_pos_rate)){
                    pass_against_pos_rate <- 0
                  }
                  
                  day_lineups$passxpos[h] <- mean(c(passes_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+passes_line_5_away$coefficients[[1]],
                                                    passes_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+passes_against_line_5_home$coefficients[[1]]))+pass_pos_rate+pass_against_pos_rate
                  
                  
                  tkl_pos_rate <- mean(stat_game_position$tkls_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  tkl_against_pos_rate <- mean(stat_game_position$tkls_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==TRUE)], na.rm = TRUE)
                  
                  if(is.nan(tkl_pos_rate)){
                    tkl_pos_rate <- 0
                  }
                  
                  if(is.nan(tkl_against_pos_rate)){
                    tkl_against_pos_rate <- 0
                  }
                  
                  day_lineups$tklxpos[h] <- mean(c(tackles_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+tackles_line_5_away$coefficients[[1]],
                                                   tackles_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+tackles_against_line_5_home$coefficients[[1]]))+tkl_pos_rate+tkl_against_pos_rate
                  
                  xg_pos_rate <- mean(stat_game_position$xg_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  xg_against_pos_rate <- mean(stat_game_position$xg_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xg_pos_rate)){
                    xg_pos_rate <- 0
                  }
                  
                  if(is.nan(xg_against_pos_rate)){
                    xg_against_pos_rate <- 0
                  }
                  
                  day_lineups$xgxpos[h] <- mean(c(xg_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xg_line_5_away$coefficients[[1]],
                                                  xg_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xg_against_line_5_home$coefficients[[1]]))+xg_pos_rate+xg_against_pos_rate
                  
                  xgot_pos_rate <- mean(stat_game_position$xgot_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  xgot_against_pos_rate <- mean(stat_game_position$xgot_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xgot_pos_rate)){
                    xgot_pos_rate <- 0
                  }
                  
                  if(is.nan(xgot_against_pos_rate)){
                    xgot_against_pos_rate <- 0
                  }
                  
                  day_lineups$xgotxpos[h] <- mean(c(xgot_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xgot_line_5_away$coefficients[[1]],
                                                    xgot_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xgot_against_line_5_home$coefficients[[1]]))+xgot_pos_rate+xgot_against_pos_rate
                  
                  xa_pos_rate <- mean(stat_game_position$xa_minus_xp[stat_game_position$team_name==day_lineups$team[h]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE], na.rm = TRUE)
                  xa_against_pos_rate <- mean(stat_game_position$xa_against_minus_xp[which(stat_game_position$team_name==day_lineups$team[day_lineups$match_id==day_lineups$match_id[h]&day_lineups$team!=day_lineups$team[h]]&stat_game_position$position_row==day_lineups$positionRow[h]&stat_game_position$is_home_team==FALSE)], na.rm = TRUE)
                  
                  if(is.nan(xa_pos_rate)){
                    xa_pos_rate <- 0
                  }
                  
                  if(is.nan(xa_against_pos_rate)){
                    xa_against_pos_rate <- 0
                  }
                  
                  day_lineups$xaxpos[h] <- mean(c(xa_line_5_away$coefficients[[2]]*fixtures$elo_dif_2[which(fixtures$team2==day_lineups$team[h])]+xa_line_5_away$coefficients[[1]],
                                                  xa_against_line_5_home$coefficients[[2]]*fixtures$elo_dif_1[which(fixtures$team1==day_lineups$team[h])]+xa_against_line_5_home$coefficients[[1]]))+xa_pos_rate+xa_against_pos_rate
                  
                  
                  
                  
                }
                
                
              }else{
                
                
                
                
                
              } 
              
              
            } 
            
            
          } 
          
          
        } 
        
        
      }
      
      
    }
    
    
    
    
  }
  
  day_lineups$passxpos[day_lineups$positionRow==0] <- ifelse(day_lineups$minutes_passes[day_lineups$positionRow==0]>=180,day_lineups$pp90[day_lineups$positionRow==0],
                                                             15)
  
  day_lineups$shxpos[day_lineups$shxpos<0] <- 0
  day_lineups$sogxpos[day_lineups$sogxpos<0] <- 0
  day_lineups$passxpos[day_lineups$passxpos<0] <-0
  day_lineups$tklxpos[day_lineups$tklxpos<0] <- 0
  day_lineups$xgxpos[day_lineups$xgxpos<0] <- 0
  day_lineups$xgotxpos[day_lineups$xgotxpos<0] <-0
  day_lineups$xaxpos[day_lineups$xaxpos<0] <- 0
  
  day_lineups$sp90[is.nan(day_lineups$sp90)] <- 0
  day_lineups$sogp90[is.nan(day_lineups$sogp90)] <- 0
  day_lineups$pp90[is.nan(day_lineups$pp90)] <-0
  day_lineups$tp90[is.nan(day_lineups$tp90)] <- 0
  day_lineups$xgp90[is.nan(day_lineups$xgp90)] <- 0
  day_lineups$xgotp90[is.nan(day_lineups$xgotp90)] <-0
  day_lineups$xap90[is.nan(day_lineups$xap90)] <- 0
  
  day_lineups$expected_tackles <- 0
  day_lineups$expected_passes <- 0
  day_lineups$expected_shots <- 0
  day_lineups$expected_sogs <- 0
  day_lineups$expected_xg <- 0
  day_lineups$expected_xgot <- 0
  day_lineups$expected_xa <- 0
  
  day_lineups$expected_tackles_low <- 0
  day_lineups$expected_passes_low <- 0
  day_lineups$expected_shots_low <- 0
  day_lineups$expected_sogs_low <- 0
  day_lineups$expected_xg_low <- 0
  day_lineups$expected_xgot_low <- 0
  day_lineups$expected_xa_low <- 0
  
  day_lineups$expected_tackles_high <- 0
  day_lineups$expected_passes_high <- 0
  day_lineups$expected_shots_high <- 0
  day_lineups$expected_sogs_high <- 0
  day_lineups$expected_xg_high  <- 0
  day_lineups$expected_xgot_high  <- 0
  day_lineups$expected_xa_high  <- 0
  
  day_lineups$pweight <- 0
  day_lineups$tweight <- 0
  day_lineups$sweight <- 0
  day_lineups$sogweight <- 0
  day_lineups$xgweight <- 0
  day_lineups$xgotweight <- 0
  day_lineups$xaweight <- 0
  
  
  day_lineups$player_pass_share <- 0
  day_lineups$player_tkl_share <- 0
  day_lineups$player_shot_share <- 0
  day_lineups$player_sog_share <- 0
  day_lineups$player_xg_share <- 0
  day_lineups$player_xgot_share <- 0
  day_lineups$player_xa_share <- 0
  
  for(k in 1:length(day_lineups$player)){
    
    if(day_lineups$positionRow[k]==0){
      
      next()
      
    }else{
      
      day_lineups$pweight[k] <- day_lineups$minutes_passes[k]/sum(day_lineups$minutes_passes[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$tweight[k] <- day_lineups$minutes_tackles[k]/sum(day_lineups$minutes_tackles[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$sweight[k] <- day_lineups$minutes_shots[k]/sum(day_lineups$minutes_shots[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$sogweight[k] <- day_lineups$minutes_sogs[k]/sum(day_lineups$minutes_sogs[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xgweight[k] <- day_lineups$minutes_xg[k]/sum(day_lineups$minutes_xg[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xgotweight[k] <- day_lineups$minutes_xgot[k]/sum(day_lineups$minutes_xgot[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xaweight[k] <- day_lineups$minutes_xa[k]/sum(day_lineups$minutes_xa[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      
      
      day_lineups$player_pass_share[k] <- day_lineups$pp90[k]/sum(day_lineups$pp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_tkl_share[k] <- day_lineups$tp90[k]/sum(day_lineups$tp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_shot_share[k] <- day_lineups$sp90[k]/sum(day_lineups$sp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_sog_share[k] <- day_lineups$sogp90[k]/sum(day_lineups$sogp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xg_share[k] <- day_lineups$xgp90[k]/sum(day_lineups$xgp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xgot_share[k] <- day_lineups$xgotp90[k]/sum(day_lineups$xgotp90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xa_share[k] <- day_lineups$xap90[k]/sum(day_lineups$xap90[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      
    }
    
    
  }
  
  for(l in 1:length(day_lineups$player)){
    
    if(day_lineups$positionRow[l]==0){
      
      next()
      
    }else{
      
      day_lineups$pweight[l] <- ifelse(is.nan(day_lineups$pweight[l]),1,
                                       ifelse(day_lineups$pweight[l]==0,min(day_lineups$pweight[day_lineups$pweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$pweight[l]))
      day_lineups$tweight[l] <- ifelse(is.nan(day_lineups$tweight[l]),1,
                                       ifelse(day_lineups$tweight[l]==0,min(day_lineups$tweight[day_lineups$tweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$tweight[l]))
      day_lineups$sweight[l] <- ifelse(is.nan(day_lineups$sweight[l]),1,
                                       ifelse(day_lineups$sweight[l]==0,min(day_lineups$sweight[day_lineups$sweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$sweight[l]))
      day_lineups$sogweight[l] <- ifelse(is.nan(day_lineups$sogweight[l]),1,
                                         ifelse(day_lineups$sogweight[l]==0,min(day_lineups$sogweight[day_lineups$sogweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$sogweight[l]))
      
      day_lineups$xgweight[l] <- ifelse(is.nan(day_lineups$xgweight[l]),1,
                                       ifelse(day_lineups$xgweight[l]==0,min(day_lineups$xgweight[day_lineups$xgweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$xgweight[l]))
      day_lineups$xgotweight[l] <- ifelse(is.nan(day_lineups$xgotweight[l]),1,
                                       ifelse(day_lineups$xgotweight[l]==0,min(day_lineups$xgotweight[day_lineups$xgotweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$xgotweight[l]))
      day_lineups$xaweight[l] <- ifelse(is.nan(day_lineups$xaweight[l]),1,
                                         ifelse(day_lineups$xaweight[l]==0,min(day_lineups$xaweight[day_lineups$xaweight!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$xaweight[l]))
      
      
      day_lineups$player_pass_share[l] <- ifelse(is.nan(day_lineups$player_pass_share[l]),1,
                                                 ifelse(day_lineups$player_pass_share[l]==0,min(day_lineups$player_pass_share[day_lineups$player_pass_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_pass_share[l]))
      day_lineups$player_tkl_share[l] <- ifelse(is.nan(day_lineups$player_tkl_share[l]),1,
                                                ifelse(day_lineups$player_tkl_share[l]==0,min(day_lineups$player_tkl_share[day_lineups$player_tkl_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_tkl_share[l]))
      day_lineups$player_shot_share[l] <- ifelse(is.nan(day_lineups$player_shot_share[l]),1,
                                                 ifelse(day_lineups$player_shot_share[l]==0,min(day_lineups$player_shot_share[day_lineups$player_shot_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_shot_share[l]))
      day_lineups$player_sog_share[l] <- ifelse(is.nan(day_lineups$player_sog_share[l]),1,
                                                ifelse(day_lineups$player_sog_share[l]==0,min(day_lineups$player_sog_share[day_lineups$player_sog_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_sog_share[l]))
      day_lineups$player_xg_share[l] <- ifelse(is.nan(day_lineups$player_xg_share[l]),1,
                                                ifelse(day_lineups$player_xg_share[l]==0,min(day_lineups$player_xg_share[day_lineups$player_xg_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_xg_share[l]))
      day_lineups$player_xgot_share[l] <- ifelse(is.nan(day_lineups$player_xgot_share[l]),1,
                                                 ifelse(day_lineups$player_xgot_share[l]==0,min(day_lineups$player_xgot_share[day_lineups$player_xgot_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_xgot_share[l]))
      day_lineups$player_xa_share[l] <- ifelse(is.nan(day_lineups$player_xa_share[l]),1,
                                                ifelse(day_lineups$player_xa_share[l]==0,min(day_lineups$player_xa_share[day_lineups$player_xa_share!=0&day_lineups$team==day_lineups$team[l]&day_lineups$positionRow==day_lineups$positionRow[l]]),day_lineups$player_xa_share[l]))
      
    }
    
    
  }
  
  day_lineups$pweight_fin <- 0
  day_lineups$tweight_fin <- 0
  day_lineups$sweight_fin <- 0
  day_lineups$sogweight_fin <- 0
  day_lineups$xgweight_fin <- 0
  day_lineups$xgotweight_fin <- 0
  day_lineups$xaweight_fin <- 0
  
  day_lineups$player_pass_share_fin <- 0
  day_lineups$player_tkl_share_fin <- 0
  day_lineups$player_shot_share_fin <- 0
  day_lineups$player_sog_share_fin <- 0
  day_lineups$player_xg_share_fin <- 0
  day_lineups$player_xgot_share_fin <- 0
  day_lineups$player_xa_share_fin <- 0
  
  for(k in 1:length(day_lineups$player)){
    
    if(day_lineups$positionRow[k]==0){
      
      next()
      
    }else{
      
      day_lineups$pweight_fin[k] <- day_lineups$pweight[k]/sum(day_lineups$pweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$tweight_fin[k] <- day_lineups$tweight[k]/sum(day_lineups$tweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$sweight_fin[k] <- day_lineups$sweight[k]/sum(day_lineups$sweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$sogweight_fin[k] <- day_lineups$sogweight[k]/sum(day_lineups$sogweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xgweight_fin[k] <- day_lineups$xgweight[k]/sum(day_lineups$xgweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xgotweight_fin[k] <- day_lineups$xgotweight[k]/sum(day_lineups$xgotweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$xaweight_fin[k] <- day_lineups$xaweight[k]/sum(day_lineups$xaweight[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      
      
      day_lineups$player_pass_share_fin[k] <- day_lineups$player_pass_share[k]/sum(day_lineups$player_pass_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_tkl_share_fin[k] <- day_lineups$player_tkl_share[k]/sum(day_lineups$player_tkl_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_shot_share_fin[k] <- day_lineups$player_shot_share[k]/sum(day_lineups$player_shot_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_sog_share_fin[k] <- day_lineups$player_sog_share[k]/sum(day_lineups$player_sog_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xg_share_fin[k] <- day_lineups$player_xg_share[k]/sum(day_lineups$player_xg_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xgot_share_fin[k] <- day_lineups$player_xgot_share[k]/sum(day_lineups$player_xgot_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      day_lineups$player_xa_share_fin[k] <- day_lineups$player_xa_share[k]/sum(day_lineups$player_xa_share[day_lineups$team==day_lineups$team[k]&day_lineups$positionRow==day_lineups$positionRow[k]])
      
    }
    
    
  }
  
  
  for(j in 1:length(day_lineups$player)){
    
    
    if(day_lineups$positionRow[j]==0){
      
      tot_passes <- sum(day_lineups$passxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_passes < day_lineups$pass_range_low[j]){
        
        tot_passes_adj <- day_lineups$pass_range_low[j]
        tot_passes_adj_low <- tot_passes
        tot_passes_adj_high <- day_lineups$pass_range_high[j]
        
      }else{
        
        if(tot_passes > day_lineups$pass_range_high[j]){
          
          tot_passes_adj <- day_lineups$pass_range_high[j]
          
          tot_passes_adj_low <- day_lineups$pass_range_high[j]
          tot_passes_adj_high <- tot_passes
        }else{
          
          
          tot_passes_adj <- tot_passes
          
          tot_passes_adj_low <- day_lineups$pass_range_low[j]
          tot_passes_adj_high <- day_lineups$pass_range_high[j]
        }
        
        
      }
      
      day_lineups$expected_passes[j] <- day_lineups$passxpos[j]*(tot_passes_adj/tot_passes)
      day_lineups$expected_passes_low[j] <- day_lineups$passxpos[j]*(tot_passes_adj_low/tot_passes)
      day_lineups$expected_passes_high[j] <- day_lineups$passxpos[j]*(tot_passes_adj_high/tot_passes)
      
    }else{
      
      
      
      tot_passes <- sum(day_lineups$passxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_passes < day_lineups$pass_range_low[j]){
        
        tot_passes_adj <- day_lineups$pass_range_low[j]
        tot_passes_adj_low <- tot_passes
        tot_passes_adj_high <- day_lineups$pass_range_high[j]
        
      }else{
        
        if(tot_passes > day_lineups$pass_range_high[j]){
          
          tot_passes_adj <- day_lineups$pass_range_high[j]
          
          tot_passes_adj_low <- day_lineups$pass_range_high[j]
          tot_passes_adj_high <- tot_passes
        }else{
          
          
          tot_passes_adj <- tot_passes
          
          tot_passes_adj_low <- day_lineups$pass_range_low[j]
          tot_passes_adj_high <- day_lineups$pass_range_high[j]
        }
        
        
      }
      
      pos_passes <- sum(day_lineups$passxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_pass_weight <- sum(day_lineups$pweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_passes <- (pos_passes*day_lineups$player_pass_share_fin[j])
      
      day_lineups$expected_passes[j] <- player_passes*(tot_passes_adj/tot_passes)
      day_lineups$expected_passes_low[j] <- player_passes*(tot_passes_adj_low/tot_passes)
      day_lineups$expected_passes_high[j] <- player_passes*(tot_passes_adj_high/tot_passes)
      
      
      tot_tackles <- sum(day_lineups$tklxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_tackles < day_lineups$tackle_range_low[j]){
        
        tot_tackles_adj <- day_lineups$tackle_range_low[j]
        tot_tackles_adj_low <- tot_tackles
        tot_tackles_adj_high <- day_lineups$tackle_range_high[j]
        
      }else{
        
        if(tot_tackles > day_lineups$tackle_range_high[j]){
          
          tot_tackles_adj <- day_lineups$tackle_range_high[j]
          tot_tackles_adj_low <- day_lineups$tackle_range_low[j]
          tot_tackles_adj_high <- tot_tackles
          
        }else{
          
          
          tot_tackles_adj <- tot_tackles
          
          tot_tackles_adj_low <- day_lineups$tackle_range_low[j]
          tot_tackles_adj_high <- day_lineups$tackle_range_high[j]
        }
        
        
      }
      
      pos_tackles <- sum(day_lineups$tklxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_tackle_weight <- sum(day_lineups$tweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_tackles <- (pos_tackles*day_lineups$player_tkl_share_fin[j])
      
      day_lineups$expected_tackles[j] <- player_tackles*(tot_tackles_adj/tot_tackles)
      
      day_lineups$expected_tackles_low[j] <- player_tackles*(tot_tackles_adj_low/tot_tackles)
      day_lineups$expected_tackles_high[j] <- player_tackles*(tot_tackles_adj_high/tot_tackles)
      
      
      
      tot_shots <- sum(day_lineups$shxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_shots < day_lineups$shot_range_low[j]){
        
        tot_shots_adj <- day_lineups$shot_range_low[j]
        tot_shots_adj_low <- tot_shots
        tot_shots_adj_high <- day_lineups$shot_range_high[j]
        
      }else{
        
        if(tot_shots > day_lineups$shot_range_high[j]){
          
          tot_shots_adj <- day_lineups$shot_range_high[j]
          tot_shots_adj_low <- day_lineups$shot_range_low[j]
          tot_shots_adj_high <- tot_shots
          
        }else{
          
          
          tot_shots_adj <- tot_shots
          tot_shots_adj_low <- day_lineups$shot_range_low[j]
          tot_shots_adj_high <- day_lineups$shot_range_high[j]
          
        }
        
        
      }
      
      pos_shots <- sum(day_lineups$shxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_shots_weight <- sum(day_lineups$sweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_shots <- (pos_shots*day_lineups$player_shot_share_fin[j])
      
      day_lineups$expected_shots[j] <- player_shots*(tot_shots_adj/tot_shots)
      
      day_lineups$expected_shots_low[j] <- player_shots*(tot_shots_adj_low/tot_shots)
      day_lineups$expected_shots_high[j] <- player_shots*(tot_shots_adj_high/tot_shots)
      
      tot_sogs <- sum(day_lineups$sogxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_sogs < day_lineups$sog_range_low[j]){
        
        tot_sogs_adj <- day_lineups$sog_range_low[j]
        tot_sogs_adj_low <- tot_sogs
        tot_sogs_adj_high <- day_lineups$sog_range_high[j]
        
      }else{
        
        if(tot_sogs > day_lineups$sog_range_high[j]){
          
          tot_sogs_adj <- day_lineups$sog_range_high[j]
          tot_sogs_adj_low <- day_lineups$sog_range_low[j]
          tot_sogs_adj_high <- tot_sogs
          
        }else{
          
          
          tot_sogs_adj <- tot_sogs
          tot_sogs_adj_low <- day_lineups$sog_range_low[j]
          tot_sogs_adj_high <- day_lineups$sog_range_high[j]
          
        }
        
        
      }
      
      pos_sogs <- sum(day_lineups$sogxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_sogs_weight <- sum(day_lineups$sogweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_sogs <- (pos_sogs*day_lineups$player_sog_share_fin[j])
      
      day_lineups$expected_sogs[j] <- player_sogs*(tot_sogs_adj/tot_sogs)
      day_lineups$expected_sogs_low[j] <- player_sogs*(tot_sogs_adj_low/tot_sogs)
      day_lineups$expected_sogs_high[j] <- player_sogs*(tot_sogs_adj_high/tot_sogs)
      
      tot_xg <- sum(day_lineups$xgxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_xg < day_lineups$xg_range_low[j]){
        
        tot_xg_adj <- day_lineups$xg_range_low[j]
        tot_xg_adj_low <- tot_xg
        tot_xg_adj_high <- day_lineups$xg_range_high[j]
        
      }else{
        
        if(tot_xg > day_lineups$xg_range_high[j]){
          
          tot_xg_adj <- day_lineups$xg_range_high[j]
          tot_xg_adj_low <- day_lineups$xg_range_low[j]
          tot_xg_adj_high <- tot_xg
          
        }else{
          
          
          tot_xg_adj <- tot_xg
          tot_xg_adj_low <- day_lineups$xg_range_low[j]
          tot_xg_adj_high <- day_lineups$xg_range_high[j]
          
        }
        
        
      }
      
      pos_xg <- sum(day_lineups$xgxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_xg_weight <- sum(day_lineups$xgweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_xg <- (pos_xg*day_lineups$player_xg_share_fin[j])
      
      day_lineups$expected_xg[j] <- player_xg*(tot_xg_adj/tot_xg)
      day_lineups$expected_xg_low[j] <- player_xg*(tot_xg_adj_low/tot_xg)
      day_lineups$expected_xg_high[j] <- player_xg*(tot_xg_adj_high/tot_xg)
      
      tot_xgot <- sum(day_lineups$xgotxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_xgot < day_lineups$xgot_range_low[j]){
        
        tot_xgot_adj <- day_lineups$xgot_range_low[j]
        tot_xgot_adj_low <- tot_xgot
        tot_xgot_adj_high <- day_lineups$xgot_range_high[j]
        
      }else{
        
        if(tot_xgot > day_lineups$xgot_range_high[j]){
          
          tot_xgot_adj <- day_lineups$xgot_range_high[j]
          tot_xgot_adj_low <- day_lineups$xgot_range_low[j]
          tot_xgot_adj_high <- tot_xgot
          
        }else{
          
          
          tot_xgot_adj <- tot_xgot
          tot_xgot_adj_low <- day_lineups$xgot_range_low[j]
          tot_xgot_adj_high <- day_lineups$xgot_range_high[j]
          
        }
        
        
      }
      
      pos_xgot <- sum(day_lineups$xgotxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_xgot_weight <- sum(day_lineups$xgotweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_xgot <- (pos_xgot*day_lineups$player_xgot_share_fin[j])
      
      day_lineups$expected_xgot[j] <- player_xgot*(tot_xgot_adj/tot_xgot)
      day_lineups$expected_xgot_low[j] <- player_xgot*(tot_xgot_adj_low/tot_xgot)
      day_lineups$expected_xgot_high[j] <- player_xgot*(tot_xgot_adj_high/tot_xgot)
      
      tot_xa <- sum(day_lineups$xaxpos[day_lineups$team==day_lineups$team[j]])
      
      if(tot_xa < day_lineups$xa_range_low[j]){
        
        tot_xa_adj <- day_lineups$xa_range_low[j]
        tot_xa_adj_low <- tot_xa
        tot_xa_adj_high <- day_lineups$xa_range_high[j]
        
      }else{
        
        if(tot_xa > day_lineups$xa_range_high[j]){
          
          tot_xa_adj <- day_lineups$xa_range_high[j]
          tot_xa_adj_low <- day_lineups$xa_range_low[j]
          tot_xa_adj_high <- tot_xa
          
        }else{
          
          
          tot_xa_adj <- tot_xa
          tot_xa_adj_low <- day_lineups$xa_range_low[j]
          tot_xa_adj_high <- day_lineups$xa_range_high[j]
          
        }
        
        
      }
      
      pos_xa <- sum(day_lineups$xaxpos[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      sum_xa_weight <- sum(day_lineups$xaweight_fin[day_lineups$team==day_lineups$team[j]&day_lineups$positionRow==day_lineups$positionRow[j]])
      
      player_xa <- (pos_xa*day_lineups$player_xa_share_fin[j])
      
      day_lineups$expected_xa[j] <- player_xa*(tot_xa_adj/tot_xa)
      day_lineups$expected_xa_low[j] <- player_xa*(tot_xa_adj_low/tot_xa)
      day_lineups$expected_xa_high[j] <- player_xa*(tot_xa_adj_high/tot_xa)
      
    }
    
    
  }
  
  day_lineups <- data.frame(day_lineups)
  
  if(is.element("player_name",colnames(day_lineups))){
    view_expected <- data.frame(day_lineups[,c("player_name","pageUrl","positionRow","team","expected_passes","expected_tackles","expected_shots","expected_sogs","expected_xg","expected_xgot","expected_xa",
                                               "pass_lineup_rating","tackle_lineup_rating","shot_lineup_rating","sog_lineup_rating","xg_lineup_rating","xgot_lineup_rating","xa_lineup_rating",
                                               "expected_passes_low","expected_tackles_low","expected_shots_low","expected_sogs_low","expected_xg_low","expected_xgot_low","expected_xa_low",
                                               "expected_passes_high","expected_tackles_high","expected_shots_high","expected_sogs_high","expected_xg_high","expected_xgot_high","expected_xa_high",
                                               "pweight","tweight","sweight","sogweight","xgweight","xgotweight","xaweight",
                                               "shot_range_low",    "shot_range_mid" ,   "shot_range_high" , 
                                               "sog_range_low" ,    "sog_range_mid"  , "sog_range_high"  , 
                                               "pass_range_low"  ,  "pass_range_mid"  ,  "pass_range_high" , 
                                               "tackle_range_low" , "tackle_range_mid" , "tackle_range_high",
                                               "xg_range_low" ,    "xg_range_mid"  , "xg_range_high"  , 
                                               "xgot_range_low" ,    "xgot_range_mid"  , "xgot_range_high"  , 
                                               "xa_range_low" ,    "xa_range_mid"  , "xa_range_high" )])
    
  }else{
    view_expected <- data.frame(day_lineups[,c("pageUrl","positionRow","team","expected_passes","expected_tackles","expected_shots","expected_sogs","expected_xg","expected_xgot","expected_xa",
                                               "pass_lineup_rating","tackle_lineup_rating","shot_lineup_rating","sog_lineup_rating","xg_lineup_rating","xgot_lineup_rating","xa_lineup_rating",
                                               "expected_passes_low","expected_tackles_low","expected_shots_low","expected_sogs_low","expected_xg_low","expected_xgot_low","expected_xa_low",
                                               "expected_passes_high","expected_tackles_high","expected_shots_high","expected_sogs_high","expected_xg_high","expected_xgot_high","expected_xa_high",
                                               "pweight","tweight","sweight","sogweight","xgweight","xgotweight","xaweight",
                                               "shot_range_low",    "shot_range_mid" ,   "shot_range_high" , 
                                               "sog_range_low" ,    "sog_range_mid"  , "sog_range_high"  , 
                                               "pass_range_low"  ,  "pass_range_mid"  ,  "pass_range_high" , 
                                               "tackle_range_low" , "tackle_range_mid" , "tackle_range_high",
                                               "xg_range_low" ,    "xg_range_mid"  , "xg_range_high"  , 
                                               "xgot_range_low" ,    "xgot_range_mid"  , "xgot_range_high"  , 
                                               "xa_range_low" ,    "xa_range_mid"  , "xa_range_high" )])
    
  }
  
  
  
  
  
}





player_prop_lines <- function(){
  
  games <- read_html("https://bv2.digitalsportstech.com/api/sgmGames?sb=ait&league=epl", encoding = "latin1")
  games_s <- games %>%
    html_nodes("body") %>%
    html_text()
  write(games_s, "output.json")
  games <-  read_json("output.json")
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
    options(encoding = "utf8")
    tackles_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Tackles"), encoding = "latin1")
    tackles_lines <- tackles_lines %>%
      html_nodes("body") %>%
      html_text()
    
    write(tackles_lines, "output.json")
    tackles_lines <-  read_json("output.json")
    passes_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Passes"), encoding = "latin1")
    passes_lines <- passes_lines %>%
      html_nodes("body") %>%
      html_text()
    write(passes_lines, "output.json")
    passes_lines <-  read_json("output.json")
    shots_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Shots"), encoding = "latin1")
    shots_lines <- shots_lines %>%
      html_nodes("body") %>%
      html_text()
    write(shots_lines, "output.json")
    shots_lines <-  read_json("output.json")
    sogs_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Shots%20on%20Goal"), encoding = "latin1")
    sogs_lines <- sogs_lines %>%
      html_nodes("body") %>%
      html_text()
    write(sogs_lines, "output.json")
    sogs_lines <-  read_json("output.json")
    goals_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Goals"), encoding = "latin1")
    goals_lines <- goals_lines %>%
      html_nodes("body") %>%
      html_text()
    write(goals_lines, "output.json")
    goals_lines <-  read_json("output.json")
    ast_lines <- read_html(paste0("https://bv2.digitalsportstech.com/api/dfm/marketsBySs?sb=ait&gameId=",games[[k]]$providers[[1]]$id,"&statistic=Assists"), encoding = "latin1")
    ast_lines <- ast_lines %>%
      html_nodes("body") %>%
      html_text()
    write(ast_lines, "output.json")
    ast_lines <-  read_json("output.json")
    
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
  all_player_passes_lines <- rbindlist(player_passes_lines)
  all_player_shots_lines <- rbindlist(player_shots_lines)
  all_player_sogs_lines <- rbindlist(player_sogs_lines)
  all_player_goals_lines <- rbindlist(player_goals_lines)
  all_player_ast_lines <- rbindlist(player_ast_lines)
  
  all_player_tackles_lines$prob_tackle <- round(1/all_player_tackles_lines$odds,4)
  all_player_passes_lines$prob_passes <- round(1/all_player_passes_lines$odds,4)
  all_player_shots_lines$prob_shots <- round(1/all_player_shots_lines$odds,4)
  all_player_sogs_lines$prob_sogs <- round(1/all_player_sogs_lines$odds,4)
  all_player_goals_lines$prob_goals <- round(1/all_player_goals_lines$odds,4)
  all_player_ast_lines$prob_ast <- round(1/all_player_ast_lines$odds,4)
  
  player_prop_list <- list(all_player_tackles_lines,
                           all_player_passes_lines,
                           all_player_shots_lines,
                           all_player_sogs_lines,
                           all_player_goals_lines,
                           all_player_ast_lines)
  
}

