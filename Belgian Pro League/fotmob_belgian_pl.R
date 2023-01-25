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

########## Variables ########

l <- fotmob_get_league_ids(cached = FALSE)

leagues <- c(92)
league_name <- "First Division A"
ccode <- "BEL"
team_df <- data.frame(team_1 = c( "Gent"   ,"Sporting Charleroi" , "Kortrijk"  ,  "Zulte Waregem", "St.Truiden" , "Union St.Gilloise","Royal Antwerp"   ,"Oostende", "Westerlo" ),
                      team_2 = c("KAA Gent","Sporting de Charleroi" ,"KV Kortrijk" ,  "SV Zulte Waregem" ,"St. Truidense" ,"Union Saint Gilloise","Antwerp","KV Oostende", "KVC Westerlo" ))
write.csv(team_df,paste0("team_codes",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))

fixtures <- fotmob_get_league_matches(ccode,league_name, cached = FALSE)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]
f_teams <- read_xlsx("team_df2 3.xlsx")




######### Historical Data ##########

e1_stat <- get_player_data_all(leagues, team_df)
e1_stats <- e1_stat[[1]]
e1_stats_s <- apply(e1_stats,2,as.character)
write.csv(e1_stats_s,paste0("data_",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))

e_teams_e <- e1_stat[[2]]
write.csv(e_teams_e,paste0("teams_",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))
######### Stats by Team #########

e1_team_lines <- get_team_lines(e1_stats)
stat_game <- e1_team_lines[[1]]
pass_line_home <- e1_team_lines[[2]]
tkl_line_home <- e1_team_lines[[3]]
shots_line_home <- e1_team_lines[[4]]
sog_line_home <- e1_team_lines[[5]]
pass_line_against_home <- e1_team_lines[[6]]
tkl_line_against_home <- e1_team_lines[[7]]
shots_line_against_home <- e1_team_lines[[8]]
sog_line_against_home <- e1_team_lines[[9]]
pass_line_away <- e1_team_lines[[10]]
tkl_line_away <- e1_team_lines[[11]]
shots_line_away <- e1_team_lines[[12]]
sog_line_away <- e1_team_lines[[13]]
pass_line_against_away <- e1_team_lines[[14]]
tkl_line_against_away <- e1_team_lines[[15]]
shots_line_against_away <- e1_team_lines[[16]]
sog_line_against_away <- e1_team_lines[[17]]
xg_line_home<- e1_team_lines[[18]]
xg_line_away<- e1_team_lines[[19]]
xgot_line_home<- e1_team_lines[[20]]
xgot_line_away<- e1_team_lines[[21]]
xa_line_home<- e1_team_lines[[22]]
xa_line_away<- e1_team_lines[[23]]
xg_against_line_home<- e1_team_lines[[24]]
xg_against_line_away<- e1_team_lines[[25]]
xgot_against_line_home<- e1_team_lines[[26]]
xgot_against_line_away<- e1_team_lines[[27]]
xa_against_line_home<- e1_team_lines[[28]]
xa_against_line_away<- e1_team_lines[[29]]

######### Stats by Team by Position ######


position_data <- get_position_data(e1_stats)
stat_game_position <- position_data[[1]]
tackles_line_1_home <- position_data[[2]]
tackles_line_2_home <- position_data[[3]]
tackles_line_3_home <- position_data[[4]]
tackles_line_4_home <- position_data[[5]]
tackles_line_5_home <- position_data[[6]]
passes_line_1_home <- position_data[[7]]
passes_line_2_home <- position_data[[8]]
passes_line_3_home <- position_data[[9]]
passes_line_4_home <- position_data[[10]]
passes_line_5_home <- position_data[[11]]
shots_line_1_home <- position_data[[12]]
shots_line_2_home <- position_data[[13]]
shots_line_3_home <- position_data[[14]]
shots_line_4_home <- position_data[[15]]
shots_line_5_home <- position_data[[16]]
shotsongoal_line_1_home <- position_data[[17]]
shotsongoal_line_2_home <- position_data[[18]]
shotsongoal_line_3_home <- position_data[[19]]
shotsongoal_line_4_home <- position_data[[20]]
shotsongoal_line_5_home <- position_data[[21]]
tackles_against_line_1_home <- position_data[[22]]
tackles_against_line_2_home <- position_data[[23]]
tackles_against_line_3_home <- position_data[[24]]
tackles_against_line_4_home <- position_data[[25]]
tackles_against_line_5_home <- position_data[[26]]
passes_against_line_1_home <- position_data[[27]]
passes_against_line_2_home <- position_data[[28]]
passes_against_line_3_home <- position_data[[29]]
passes_against_line_4_home <- position_data[[30]]
passes_against_line_5_home <- position_data[[31]]
shots_against_line_1_home <- position_data[[32]]
shots_against_line_2_home <- position_data[[33]]
shots_against_line_3_home <- position_data[[34]]
shots_against_line_4_home <- position_data[[35]]
shots_against_line_5_home <- position_data[[36]]
sogs_against_line_1_home <- position_data[[37]]
sogs_against_line_2_home <- position_data[[38]]
sogs_against_line_3_home <- position_data[[39]]
sogs_against_line_4_home <- position_data[[40]]
sogs_against_line_5_home <- position_data[[41]]
tackles_line_1_away <- position_data[[42]]
tackles_line_2_away <- position_data[[43]]
tackles_line_3_away <- position_data[[44]]
tackles_line_4_away <- position_data[[45]]
tackles_line_5_away <- position_data[[46]]
passes_line_1_away <- position_data[[47]]
passes_line_2_away <- position_data[[48]]
passes_line_3_away <- position_data[[49]]
passes_line_4_away <- position_data[[50]]
passes_line_5_away <- position_data[[51]]
shots_line_1_away <- position_data[[52]]
shots_line_2_away <- position_data[[53]]
shots_line_3_away <- position_data[[54]]
shots_line_4_away <- position_data[[55]]
shots_line_5_away <- position_data[[56]]
shotsongoal_line_1_away <- position_data[[57]]
shotsongoal_line_2_away <- position_data[[58]]
shotsongoal_line_3_away <- position_data[[59]]
shotsongoal_line_4_away <- position_data[[60]]
shotsongoal_line_5_away <- position_data[[61]]
tackles_against_line_1_away <- position_data[[62]]
tackles_against_line_2_away <- position_data[[63]]
tackles_against_line_3_away <- position_data[[64]]
tackles_against_line_4_away <- position_data[[65]]
tackles_against_line_5_away <- position_data[[66]]
passes_against_line_1_away <- position_data[[67]]
passes_against_line_2_away <- position_data[[68]]
passes_against_line_3_away <- position_data[[69]]
passes_against_line_4_away <- position_data[[70]]
passes_against_line_5_away <- position_data[[71]]
shots_against_line_1_away <- position_data[[72]]
shots_against_line_2_away <- position_data[[73]]
shots_against_line_3_away <- position_data[[74]]
shots_against_line_4_away <- position_data[[75]]
shots_against_line_5_away <- position_data[[76]]
sogs_against_line_1_away <- position_data[[77]]
sogs_against_line_2_away <- position_data[[78]]
sogs_against_line_3_away <- position_data[[79]]
sogs_against_line_4_away <- position_data[[80]]
sogs_against_line_5_away <- position_data[[81]]
xg_line_1_home <- position_data[[82]]
xg_line_2_home <- position_data[[83]]
xg_line_3_home <- position_data[[84]]
xg_line_4_home <- position_data[[85]]
xg_line_5_home <- position_data[[86]]
xgot_line_1_home <- position_data[[87]]
xgot_line_2_home <- position_data[[88]]
xgot_line_3_home <- position_data[[89]]
xgot_line_4_home <- position_data[[90]]
xgot_line_5_home <- position_data[[91]]
xa_line_1_home <- position_data[[92]]
xa_line_2_home <- position_data[[93]]
xa_line_3_home <- position_data[[94]]
xa_line_4_home <- position_data[[95]]
xa_line_5_home <- position_data[[96]]
xg_against_line_1_home <- position_data[[97]]
xg_against_line_2_home <- position_data[[98]]
xg_against_line_3_home <- position_data[[99]]
xg_against_line_4_home <- position_data[[100]]
xg_against_line_5_home <- position_data[[101]]
xgot_against_line_1_home <- position_data[[102]]
xgot_against_line_2_home <- position_data[[103]]
xgot_against_line_3_home <- position_data[[104]]
xgot_against_line_4_home <- position_data[[105]]
xgot_against_line_5_home <- position_data[[106]]
xa_against_line_1_home <- position_data[[107]]
xa_against_line_2_home <- position_data[[108]]
xa_against_line_3_home <- position_data[[109]]
xa_against_line_4_home <- position_data[[110]]
xa_against_line_5_home <- position_data[[111]]
xg_line_1_away <- position_data[[112]]
xg_line_2_away <- position_data[[113]]
xg_line_3_away <- position_data[[114]]
xg_line_4_away <- position_data[[115]]
xg_line_5_away <- position_data[[116]]
xgot_line_1_away <- position_data[[117]]
xgot_line_2_away <- position_data[[118]]
xgot_line_3_away <- position_data[[119]]
xgot_line_4_away <- position_data[[120]]
xgot_line_5_away <- position_data[[121]]
xa_line_1_away <- position_data[[122]]
xa_line_2_away <- position_data[[123]]
xa_line_3_away <- position_data[[124]]
xa_line_4_away <- position_data[[125]]
xa_line_5_away <- position_data[[126]]
xg_against_line_1_away <- position_data[[127]]
xg_against_line_2_away <- position_data[[128]]
xg_against_line_3_away <- position_data[[129]]
xg_against_line_4_away <- position_data[[130]]
xg_against_line_5_away <- position_data[[131]]
xgot_against_line_1_away <- position_data[[132]]
xgot_against_line_2_away <- position_data[[133]]
xgot_against_line_3_away <- position_data[[134]]
xgot_against_line_4_away <- position_data[[135]]
xgot_against_line_5_away <- position_data[[136]]
xa_against_line_1_away <- position_data[[137]]
xa_against_line_2_away <- position_data[[138]]
xa_against_line_3_away <- position_data[[139]]
xa_against_line_4_away <- position_data[[140]]
xa_against_line_5_away <- position_data[[141]]
######### Stats by Player by Position #########


stat_afc <- get_player_data(e1_stats)

######### Stats by Player#########


#stat_player_afc <- get_player_data_no_pos(e1_stats)

####### Match Lineups ########

day_lineups <- lineup_data(date, stat_game_position, e_teams_e, league_name, ccode)

day_lineups$pageUrl <- ifelse((day_lineups$pageUrl=="/NA/NA/"),
                              day_lineups$player,
                              day_lineups$pageUrl)

day_lineups$pageUrl <- ifelse(is.na(day_lineups$pageUrl),
                               day_lineups$player,
                               day_lineups$pageUrl)

day_lineups$pageUrl2 <- ifelse(is.na(day_lineups$pageUrl2),
                              day_lineups$player_name,
                              day_lineups$pageUrl2)

day_lineups$player_name <- ifelse(is.na(day_lineups$player_name),
                               day_lineups$pageUrl2,
                               day_lineups$player_name)

stat_afc <- stat_afc[order(-stat_afc$minutes),]

day_lineups$pass_share <- 0
day_lineups$tackle_share <- 0
day_lineups$shot_share <- 0
day_lineups$sog_share <- 0 
day_lineups$xg_share <- 0
day_lineups$xgot_share <- 0
day_lineups$xa_share <- 0 



for(i in 1:length(day_lineups$match_id)){
  
  if(length(stat_afc$pass_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]])==0){
    max_sum_passes <- sum(unlist(stat_afc$pass_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_tackles <- sum(unlist(stat_afc$tkl_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_shots <- sum(unlist(stat_afc$shots_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_sogs <- sum(unlist(stat_afc$sog_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xg <- sum(unlist(stat_afc$xg_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xgot <- sum(unlist(stat_afc$xgot_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xa <- sum(unlist(stat_afc$xa_90min[stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    
  }else{
    max_sum_passes <- sum(unlist(stat_afc$pass_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_tackles <- sum(unlist(stat_afc$tkl_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_shots <- sum(unlist(stat_afc$shots_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_sogs <- sum(unlist(stat_afc$sog_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xg <- sum(unlist(stat_afc$xg_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xgot <- sum(unlist(stat_afc$xgot_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    max_sum_xa <- sum(unlist(stat_afc$xa_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]))
    
  }
  
  
  if(day_lineups$positionRow[i]==0){
    
    
    
  }else{
    
    if(length(which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130))==0){
      
      
      if(max_sum_passes==0){
        day_lineups$pass_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$pass_share[i] <- ifelse(mean(stat_afc$pass_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$pass_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                            0,
                                            (mean(stat_afc$pass_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$pass_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_passes)
        
      }
      if(max_sum_tackles==0){
        day_lineups$tackle_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$tackle_share[i] <- ifelse(mean(stat_afc$tkl_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$tkl_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                              0,
                                              (mean(stat_afc$tkl_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$tkl_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130]/max_sum_tackles, na.rm = T)))/max_sum_passes)
        
      }
      if(max_sum_shots==0){
        day_lineups$shot_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$shot_share[i] <- ifelse(mean(stat_afc$shots_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$shots_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                            0,
                                            (mean(stat_afc$shots_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$shots_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_shots)
        
      }
      if(max_sum_sogs==0){
        day_lineups$sog_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$sog_share[i] <- ifelse(mean(stat_afc$sog_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$sog_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                           0,
                                           (mean(stat_afc$sog_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$sog_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_sogs)
      }
      
      if(max_sum_xg==0){
        day_lineups$xg_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xg_share[i] <- ifelse(mean(stat_afc$xg_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xg_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                           0,
                                           (mean(stat_afc$xg_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xg_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_xg)
      }
      if(max_sum_xgot==0){
        day_lineups$xgot_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xgot_share[i] <- ifelse(mean(stat_afc$xgot_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xgot_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                           0,
                                           (mean(stat_afc$xgot_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xgot_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_xgot)
      }
      if(max_sum_xa==0){
        day_lineups$xa_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xa_share[i] <- ifelse(mean(stat_afc$xa_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xa_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T))<0,
                                           0,
                                           (mean(stat_afc$xa_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)-(0.5*sd(stat_afc$xa_90min[stat_afc$position_row==day_lineups$positionRow[i]&stat_afc$minutes>=130], na.rm = T)))/max_sum_xa)
      }
      
      
    }else{
      
      if(max_sum_passes==0){
        day_lineups$pass_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$pass_share[i] <- stat_afc$pass_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_passes
      }
      if(max_sum_tackles==0){
        day_lineups$tackle_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$tackle_share[i] <- stat_afc$tkl_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_tackles
      }
      if(max_sum_shots==0){
        day_lineups$shot_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$shot_share[i] <- stat_afc$shots_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_shots
      }
      if(max_sum_sogs==0){
        day_lineups$sog_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$sog_share[i] <- stat_afc$sog_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_sogs 
      }
      if(max_sum_xg==0){
        day_lineups$xg_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xg_share[i] <- stat_afc$xg_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_xg 
      }
      if(max_sum_xgot==0){
        day_lineups$xgot_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xgot_share[i] <- stat_afc$xgot_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_xgot 
      }
      if(max_sum_xa==0){
        day_lineups$xa_share[i] <- 1/length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team==day_lineups$team[i]))
      }else{
        day_lineups$xa_share[i] <- stat_afc$xa_90min[which(day_lineups$pageUrl[i]==stat_afc$page_url&day_lineups$positionRow[i]==stat_afc$position_row&stat_afc$minutes>=130)]/max_sum_xa 
      }
      
      
      
    }
    
  }
  
  
}

day_lineups$pass_lineup_rating <- 0
day_lineups$tackle_lineup_rating <- 0
day_lineups$shot_lineup_rating <- 0
day_lineups$sog_lineup_rating <- 0
day_lineups$xg_lineup_rating <- 0
day_lineups$xgot_lineup_rating <- 0
day_lineups$xa_lineup_rating <- 0

for(j in 1:length(day_lineups$match_id)){
  
  psr <- sum(day_lineups$pass_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  tsr <- sum(day_lineups$tackle_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  ssr <- sum(day_lineups$shot_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  sogsr <- sum(day_lineups$sog_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  xgsr <- sum(day_lineups$xg_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  xgotsr <- sum(day_lineups$xgot_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  xasr <- sum(day_lineups$xa_share[day_lineups$team==day_lineups$team[j]])/(length(unique(day_lineups$positionRow[day_lineups$team==day_lineups$team[j]]))-1)
  
  o_psr <- sum(day_lineups$pass_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_tsr <- sum(day_lineups$tackle_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_ssr <- sum(day_lineups$shot_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_sogsr <- sum(day_lineups$sog_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_xgsr <- sum(day_lineups$xg_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_xgotsr <- sum(day_lineups$xgot_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  o_xasr <- sum(day_lineups$xa_share[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]])/(length(unique(day_lineups$positionRow[day_lineups$team==unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])[which(unique(day_lineups$team[day_lineups$match_id==day_lineups$match_id[j]])!=day_lineups$team[j])]]))-1)
  
  day_lineups$pass_lineup_rating[j] <- mean(c(psr,(2-o_psr)))
  day_lineups$tackle_lineup_rating[j] <- mean(c(tsr,(2-o_tsr)))
  day_lineups$shot_lineup_rating[j] <- mean(c(ssr,(2-o_ssr)))
  day_lineups$sog_lineup_rating[j] <- mean(c(sogsr,(2-o_sogsr)))
  day_lineups$xg_lineup_rating[j] <- mean(c(tsr,(2-o_tsr)))
  day_lineups$xgot_lineup_rating[j] <- mean(c(ssr,(2-o_ssr)))
  day_lineups$xa_lineup_rating[j] <- mean(c(sogsr,(2-o_sogsr)))
  
  
}


day_lineups$pass_lineup_rating <- ifelse(day_lineups$pass_lineup_rating>1.33,1.33,ifelse(day_lineups$pass_lineup_rating<.67,.67,day_lineups$pass_lineup_rating))
day_lineups$tackle_lineup_rating <- ifelse(day_lineups$tackle_lineup_rating>1.33,1.33,ifelse(day_lineups$tackle_lineup_rating<.67,.67,day_lineups$tackle_lineup_rating))
day_lineups$shot_lineup_rating <- ifelse(day_lineups$shot_lineup_rating>1.33,1.33,ifelse(day_lineups$shot_lineup_rating<.67,.67,day_lineups$shot_lineup_rating))
day_lineups$sog_lineup_rating <- ifelse(day_lineups$sog_lineup_rating>1.33,1.33,ifelse(day_lineups$sog_lineup_rating<.67,.67,day_lineups$sog_lineup_rating))
day_lineups$xg_lineup_rating <- ifelse(day_lineups$xg_lineup_rating>1.33,1.33,ifelse(day_lineups$xg_lineup_rating<.67,.67,day_lineups$xg_lineup_rating))
day_lineups$xgot_lineup_rating <- ifelse(day_lineups$xgot_lineup_rating>1.33,1.33,ifelse(day_lineups$xgot_lineup_rating<.67,.67,day_lineups$xgot_lineup_rating))
day_lineups$xa_lineup_rating <- ifelse(day_lineups$xa_lineup_rating>1.33,1.33,ifelse(day_lineups$xa_lineup_rating<.67,.67,day_lineups$xa_lineup_rating))

####### Fotmob Fixtures w/ SPI #######

fixtures <- fotmob_get_league_matches(ccode,league_name, cached = F)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]

fixtures_f <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))

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


for(i in 1:length(fixtures$round)){
  
  if(is.element(fixtures$home$name[i],f_teams$team2)){
    fixtures$home$name[i] <- f_teams$team[which(f_teams$team2==fixtures$home$name[i])][1]
  }
  
  if(is.element(fixtures$home$name[i],team_df$team_1)){
    fixtures$home$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$home$name[i])]
  }
  
  
  fixtures_f2 <- fixtures_f[c(which(fixtures_f$team1==fixtures$home$name[i]),which(fixtures_f$team2==fixtures$home$name[i])),]
  fixtures_f2 <- fixtures_f2[fixtures_f2$date>=Sys.Date(),]
  fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
  fixtures_f2 <- fixtures_f2[1,]
  
  
  
  
  fixtures$spi1[i] <- ifelse(length(fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])==0,
                             fixtures_f2$spi2[which(fixtures$home$name[i]==fixtures_f2$team2)],
                             fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])
  
  if(is.element(fixtures$away$name[i],f_teams$team2)){
    fixtures$away$name[i] <- f_teams$team[which(f_teams$team2==fixtures$away$name[i])][1]
  }
  
  if(is.element(fixtures$away$name[i],team_df$team_1)){
    fixtures$away$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$away$name[i])]
  }
  
  fixtures_f2 <- fixtures_f[c(which(fixtures_f$team2==fixtures$away$name[i]),
                              which(fixtures_f$team1==fixtures$away$name[i])),]
  fixtures_f2 <- fixtures_f2[fixtures_f2$date>=Sys.Date(),]
  fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
  fixtures_f2 <- fixtures_f2[1,]
  
  
  
  
  
  fixtures$spi2[i] <- ifelse(length(fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])==0,
                             fixtures_f2$spi1[which(fixtures$away$name[i]==fixtures_f2$team1)],
                             fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])
  
}

fixtures_ff <- fixtures

fixtures <- fotmob_get_league_matches(ccode,league_name, cached = F)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]

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


for(i in 1:length(fixtures$round)){
  fixtures$spi1[i] <- fixtures_ff$spi1[which(fixtures$away$shortName[i]==fixtures_ff$away$shortName)]
  fixtures$spi2[i] <- fixtures_ff$spi2[which(fixtures$away$shortName[i]==fixtures_ff$away$shortName)]
  if(is.element(fixtures$home$name[i],team_df$team_1)){
    fixtures$home$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$home$name[i])]
  }
  
  if(is.element(fixtures$away$name[i],team_df$team_1)){
    fixtures$away$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$away$name[i])]
  }
}



####### Match Data by Team #######



game_data_by_team <- get_game_data_by_team(stat_game,pass_line_home,tkl_line_home,shots_line_home,sog_line_home,game_no, fixtures,
                                           pass_line_against_home,
                                           tkl_line_against_home,
                                           shots_line_against_home,
                                           sog_line_against_home,
                                           pass_line_against_away,
                                           tkl_line_against_away,
                                           shots_line_against_away,
                                           sog_line_against_away,pass_line_away,tkl_line_away,shots_line_away,sog_line_away,e_teams_e)
pass_ranges_all <- game_data_by_team[[1]]
tackles_ranges_all <- game_data_by_team[[2]]
shots_ranges_all <- game_data_by_team[[3]]
sog_ranges_all <- game_data_by_team[[4]]
fixtures <- game_data_by_team[[5]]
xg_ranges_all <- game_data_by_team[[6]]
xgot_ranges_all <- game_data_by_team[[7]]
xa_ranges_all <- game_data_by_team[[8]]
sog_ranges_all$sog_ranges <- ifelse(sog_ranges_all$sog_ranges>0,
                                    sog_ranges_all$sog_ranges,0)
shots_ranges_all$shot_ranges <- ifelse(shots_ranges_all$shot_ranges>0,
                                       shots_ranges_all$shot_ranges,0)
tackles_ranges_all$tackle_ranges <- ifelse(tackles_ranges_all$tackle_ranges>0,
                                           tackles_ranges_all$tackle_ranges,0)
xg_ranges_all$xg_ranges <- ifelse(xg_ranges_all$xg_ranges>0,
                                   xg_ranges_all$xg_ranges,0)
xgot_ranges_all$xgot_ranges <- ifelse(xgot_ranges_all$xgot_ranges>0,
                                      xgot_ranges_all$xgot_ranges,0)
xa_ranges_all$xa_ranges <- ifelse(xa_ranges_all$xa_ranges>0,
                                      xa_ranges_all$xa_ranges,0)


########## Player Expected Stats By Match ###########


player_stats_game <-  get_game_player_data(day_lineups,shots_ranges_all,sog_ranges_all,pass_ranges_all,tackles_ranges_all,
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
                                           e_teams)


player_stats_game$expected_passes <- player_stats_game$expected_passes*player_stats_game$pass_lineup_rating
player_stats_game$expected_passes_low <- player_stats_game$expected_passes_low*player_stats_game$pass_lineup_rating
player_stats_game$expected_passes_high <- player_stats_game$expected_passes_high*player_stats_game$pass_lineup_rating

player_stats_game$expected_tackles <- player_stats_game$expected_tackles*player_stats_game$tackle_lineup_rating
player_stats_game$expected_tackles_low <- player_stats_game$expected_tackles_low*player_stats_game$tackle_lineup_rating
player_stats_game$expected_tackles_high <- player_stats_game$expected_tackles_high*player_stats_game$tackle_lineup_rating

player_stats_game$expected_shots <- player_stats_game$expected_shots*player_stats_game$shot_lineup_rating
player_stats_game$expected_shots_low <- player_stats_game$expected_shots_low*player_stats_game$shot_lineup_rating
player_stats_game$expected_shots_high <- player_stats_game$expected_shots_high*player_stats_game$shot_lineup_rating

player_stats_game$expected_sogs <- player_stats_game$expected_sogs*player_stats_game$sog_lineup_rating
player_stats_game$expected_sogs_low <- player_stats_game$expected_sogs_low*player_stats_game$sog_lineup_rating
player_stats_game$expected_sogs_high <- player_stats_game$expected_sogs_high*player_stats_game$sog_lineup_rating

player_stats_game$expected_xg <- player_stats_game$expected_xg*player_stats_game$xg_lineup_rating
player_stats_game$expected_xg_low <- player_stats_game$expected_xg_low*player_stats_game$xg_lineup_rating
player_stats_game$expected_xg_high <- player_stats_game$expected_xg_high*player_stats_game$xg_lineup_rating

player_stats_game$expected_xgot <- player_stats_game$expected_xgot*player_stats_game$xgot_lineup_rating
player_stats_game$expected_xgot_low <- player_stats_game$expected_xgot_low*player_stats_game$xgot_lineup_rating
player_stats_game$expected_xgot_high <- player_stats_game$expected_xgot_high*player_stats_game$xgot_lineup_rating

player_stats_game$expected_xa <- player_stats_game$expected_xa*player_stats_game$xa_lineup_rating
player_stats_game$expected_xa_low <- player_stats_game$expected_xa_low*player_stats_game$xa_lineup_rating
player_stats_game$expected_xa_high <- player_stats_game$expected_xa_high*player_stats_game$xa_lineup_rating

player_stats_game$goal_exp_rate <- 0
player_stats_game$ast_exp_rate <- 0
e1_stats$stats_goals <- as.numeric(e1_stats$stats_goals)
e1_stats$stats_assists <- as.numeric(e1_stats$stats_assists)

mean_gxp_rate <- mean( e1_stats$stats_goals[!is.na(e1_stats$stats_expected_goals_x_g)]-e1_stats$stats_expected_goals_x_g[!is.na(e1_stats$stats_expected_goals_x_g)], na.rm = T)
sd_gxp_rate <- sd( e1_stats$stats_goals[!is.na(e1_stats$stats_expected_goals_x_g)]-e1_stats$stats_expected_goals_x_g[!is.na(e1_stats$stats_expected_goals_x_g)])

mean_axp_rate <- mean( e1_stats$stats_assists[-c(which(is.na(e1_stats$stats_expected_assists_x_a)),which(is.na(e1_stats$stats_assists)))]-e1_stats$stats_expected_assists_x_a[-c(which(is.na(e1_stats$stats_expected_assists_x_a)),which(is.na(e1_stats$stats_assists)))], na.rm = T)
sd_axp_rate <- sd( e1_stats$stats_assists[-c(which(is.na(e1_stats$stats_expected_assists_x_a)),which(is.na(e1_stats$stats_assists)))]-e1_stats$stats_expected_assists_x_a[-c(which(is.na(e1_stats$stats_expected_assists_x_a)),which(is.na(e1_stats$stats_assists)))])

for(a in 1:length(player_stats_game$player_name)){
  player_stats_game$goal_exp_rate[a] <- mean( e1_stats$stats_goals[which(player_stats_game$pageUrl[a]==e1_stats$page_url)]-
                                                e1_stats$stats_expected_goals_x_g[which(player_stats_game$pageUrl[a]==e1_stats$page_url)],na.rm=TRUE)
  player_stats_game$ast_exp_rate[a] <- mean( e1_stats$stats_assists[which(player_stats_game$pageUrl[a]==e1_stats$page_url)]-
                                               e1_stats$stats_expected_assists_x_a[which(player_stats_game$pageUrl[a]==e1_stats$page_url)],na.rm=TRUE)
  player_stats_game$goal_exp_rate[a] <- ifelse(player_stats_game$goal_exp_rate[a]>(mean_gxp_rate+sd_gxp_rate),
                                               (mean_gxp_rate+sd_gxp_rate),
                                               ifelse(player_stats_game$goal_exp_rate[a]<(mean_gxp_rate-sd_gxp_rate),
                                                      (mean_gxp_rate-sd_gxp_rate),
                                                      player_stats_game$goal_exp_rate[a]))
  player_stats_game$ast_exp_rate[a] <- ifelse(player_stats_game$ast_exp_rate[a]>(mean_axp_rate+sd_axp_rate),
                                              (mean_axp_rate+sd_axp_rate),
                                              ifelse(player_stats_game$ast_exp_rate[a]<(mean_axp_rate-sd_axp_rate),
                                                     (mean_axp_rate-sd_axp_rate),
                                                     player_stats_game$ast_exp_rate[a]))
  
}
player_stats_game$goal_exp_rate[is.na(player_stats_game$goal_exp_rate)] <- 0
player_stats_game$ast_exp_rate[is.na(player_stats_game$ast_exp_rate)] <- 0

player_stats_game$goal_exp <- ifelse((player_stats_game$goal_exp_rate+player_stats_game$expected_xg)<0,0,(player_stats_game$goal_exp_rate+player_stats_game$expected_xg))
player_stats_game$ast_exp <-  ifelse((player_stats_game$ast_exp_rate+player_stats_game$expected_xa)<0,0,(player_stats_game$ast_exp_rate+player_stats_game$expected_xa))

####### Game Prediction ########






setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/Belgian Pro League")

source("running_belgian_pl_lines.R")
whsa
unique(c(unique(whsa$BARSTOOL_TEAM1),unique(whsa$BARSTOOL_TEAM2)))
unique(day_lineups$team)

team_df2 <-  data.frame(team_1 = c("Standard de Liège"   ,"Sint-Truiden" ,"AS Eupen" ,"Westerlo","Seraing United"  ,
                                   "Royal Antwerp FC","AA Gent" , "KRC Genk","Zulte Waregem" , "Union Saint-Gilloise" ,"Kortrijk" ),
                        team_2 = c("Standard Liege" , "St. Truidense"  ,"Eupen" ,"KVC Westerlo"  ,  "RFC Seraing"   ,  
                                   "Antwerp", "KAA Gent" , "Genk" ,"SV Zulte Waregem" ,"Union Saint Gilloise"  ,"KV Kortrijk"       ))
write.csv(team_df2,paste0("team_barstool",ccode,"_",league_name,"_",gsub("-","",date[1]),".csv"))
for(i in 1:length(team_df2$team_1)){
  
  whsa$BARSTOOL_TEAM1[whsa$BARSTOOL_TEAM1==team_df2$team_1[i]] <- team_df2$team_2[i]
  whsa$BARSTOOL_TEAM2[whsa$BARSTOOL_TEAM2==team_df2$team_1[i]] <- team_df2$team_2[i]
  
}


test1 <- stat_game$passes[!is.na(stat_game$passes)]
test1_1 <- stat_game$shots[!is.na(stat_game$shots)]
test1_2 <- stat_game$shotsongoal[!is.na(stat_game$shotsongoal)]
test2 <- stat_game$expected_goals[!is.na(stat_game$expected_goals)]
test3 <- stat_game$is_mc[!is.na(stat_game$passes)]

ggplot(data.frame(test1,test2),aes(x=test1_1,
                                   y=test1_2,
                                   col=test3))+geom_point()+geom_smooth(method='lm', formula= y~x)

xg_exp <- summary(lm(test2~test1+test1_2))
wpct_1 <- 0
wpct_X <- 0
wpct_2 <- 0
m_totals <- 0
setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/Belgian Pro League")
sink(paste0(league_name,"_",ccode,"_game_log_",gsub("-","",date[1]),".txt"), split = TRUE)
card <- ""
q <- 1
for(j in 1:length(fixtures$round)){
  
  game_end_check <- fixtures$round[any(player_stats_game$team==fixtures$home$name[j])&any(player_stats_game$team==fixtures$away$name[j])][1]
  
  if(is.na(game_end_check)){
    next()
  }
  if(fixtures$status$finished[j]==TRUE){
    next()
  }
  print("-----Model Predictions Below-----")
  
  print("--Pass/Shot Model--")
  
  #Pass/Model
  
  h_score <- xg_exp$coefficients[[1]]+(xg_exp$coefficients[[2]]*sum(player_stats_game$expected_passes[player_stats_game$team==fixtures$home$name[j]]))+
    (xg_exp$coefficients[[3]]*sum(player_stats_game$expected_sogs[player_stats_game$team==fixtures$home$name[j]]))
  
  a_score <- xg_exp$coefficients[[1]]+(xg_exp$coefficients[[2]]*sum(player_stats_game$expected_passes[player_stats_game$team==fixtures$away$name[j]]))+
    (xg_exp$coefficients[[3]]*sum(player_stats_game$expected_sogs[player_stats_game$team==fixtures$away$name[j]]))
  
  print(paste0("The pass/shot predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The pass/shot predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[1] <- team_one_win_pct
  wpct_X[1] <- team_one_draw_pct
  wpct_2[1] <- team_two_win_pct
  m_totals[1] <- h_score+a_score
  
  print(paste0(fixtures$home$name[j]," pass/shot Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The pass/shot Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," pass/shot Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  
  print("--XG Model--")
  
  #XG Model
  
  h_score <- sum(player_stats_game$expected_xg[player_stats_game$team==fixtures$home$name[j]])
  a_score <- sum(player_stats_game$expected_xg[player_stats_game$team==fixtures$away$name[j]])
  
  if(is.nan(h_score)){
    next()
  }
  if(is.nan(a_score)){
    next()
  }
  
  print(paste0("The xg model predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The xg model predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[2] <- team_one_win_pct
  wpct_X[2] <- team_one_draw_pct
  wpct_2[2] <- team_two_win_pct
  m_totals[2] <- h_score+a_score
  
  print(paste0(fixtures$home$name[j]," xg model Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The xg model Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," xg model Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  print("--XGoT Model--")
  
  #XGoT Model
  
  h_score <- sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$home$name[j]])
  a_score <- sum(player_stats_game$expected_xgot[player_stats_game$team==fixtures$away$name[j]])
  
  print(paste0("The xgot model predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The xgot model predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[3] <- team_one_win_pct
  wpct_X[3] <- team_one_draw_pct
  wpct_2[3] <- team_two_win_pct
  m_totals[3] <- h_score+a_score
  
  print(paste0(fixtures$home$name[j]," xgot model Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The xgot model Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," xgot model Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  print("--XA Model--")
  
  #XA Model
  
  h_conv_rate <- mean(stat_game$expected_goals[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE])
  a_conv_rate <- mean(stat_game$expected_goals[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE])
  
  h_score <- sum(player_stats_game$expected_xa[player_stats_game$team==fixtures$home$name[j]])+h_conv_rate
  a_score <- sum(player_stats_game$expected_xa[player_stats_game$team==fixtures$away$name[j]])+a_conv_rate
  
  if(is.nan(h_score)){
    next()
  }
  if(is.nan(a_score)){
    next()
  }
  
  print(paste0("The xa model predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The xa model predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[4] <- team_one_win_pct
  wpct_X[4] <- team_one_draw_pct
  wpct_2[4] <- team_two_win_pct
  m_totals[4] <- h_score+a_score
  
  print(paste0(fixtures$home$name[j]," xa model Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The xa model Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," xa model Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  print("--XGExp Model--")
  
  #XG Model
  
  h_score <- sum(player_stats_game$goal_exp[player_stats_game$team==fixtures$home$name[j]])
  a_score <- sum(player_stats_game$goal_exp[player_stats_game$team==fixtures$away$name[j]])
  
  print(paste0("The XGExp model predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The XGExp model predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[5] <- team_one_win_pct
  wpct_X[5] <- team_one_draw_pct
  wpct_2[5] <- team_two_win_pct
  m_totals[5] <- h_score+a_score
  
  print(paste0(fixtures$home$name[j]," XGExp model Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The XGExp model Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," XGExp model Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  print("--XAExp Model--")
  
  #XA Model
  
  h_conv_rate <- mean(stat_game$expected_goals[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==TRUE])
  a_conv_rate <- mean(stat_game$expected_goals[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE]-stat_game$expected_ast[stat_game$expected_ast!=0&stat_game$is_home_team==FALSE])
  
  h_score <- sum(player_stats_game$ast_exp[player_stats_game$team==fixtures$home$name[j]])+h_conv_rate
  a_score <- sum(player_stats_game$ast_exp[player_stats_game$team==fixtures$away$name[j]])+a_conv_rate
  
  print(paste0("The XAExp model predicted score is ",fixtures$home$name[j]," ",round(h_score,4), " to ",fixtures$away$name[j]," ",round(a_score,4)))
  print(paste0("The XAExp model predicted total is ",round(h_score,4)+round(a_score,4)))
  
  
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
  for (i in 1:n_sims) {
    
    team_one_goals <- rpois(1, lambda = team_one_eg)
    team_two_goals <- rpois(1, lambda = team_two_eg)
    
    
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
  }
  team_one_win_pct <- team_one_wins / n_sims
  team_one_draw_pct <- team_one_draws / n_sims
  team_two_win_pct <- team_two_wins / n_sims
  team_two_draw_pct <- team_two_draws / n_sims
  
  wpct_1[6] <- team_one_win_pct
  wpct_X[6] <- team_one_draw_pct
  wpct_2[6] <- team_two_win_pct
  m_totals[6] <- h_score+a_score
  
  spct_1 <- wpct_1+wpct_X
  spct_2 <- wpct_2+wpct_X
  
  print(paste0(fixtures$home$name[j]," XAExp model Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The XAExp model Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," XAExp model Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  print("--Mean 6 Models--")
  
  print(paste0("Our modelled likelihood of ",fixtures$home$name[j]," winning this match is ",
               round((mean(wpct_1)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2)," percent, with a minimum chance of ",
               round(min(wpct_1) * 100, 2),  " percent, and a maximum chance of ",round(max(wpct_1) * 100, 2)," percent."))
  print(paste0("Our modelled likelihood of a draw occurring in this match is ",
               round((mean(wpct_X)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2),
               " percent, with a minimum chance of ",
               round(min(wpct_X) * 100, 2),  " percent, and a maximum chance of ",round(max(wpct_X) * 100, 2)," percent."))
  print(paste0("Our modelled likelihood of ",fixtures$away$name[j]," winning this match is ",
               round((mean(wpct_2)/sum(c(mean(wpct_1),mean(wpct_X),mean(wpct_2))))*100, 2),
               " percent, with a minimum chance of ",
               round(min(wpct_2) * 100, 2),  " percent, and a maximum chance of ",round(max(wpct_2) * 100, 2)," percent."))
  
  print(paste0("Our modelled likelihood of ",fixtures$home$name[j]," winning this match OR DRAW is ",
               round((mean(spct_1)/sum(c(mean(spct_1),mean(spct_2))))*100, 2),
               " percent, with a minimum chance of ",
               round((min(spct_1)/sum(c(min(spct_1),max(spct_2)))) * 100, 2),  " percent, and a maximum chance of ",
               round((max(spct_1)/sum(c(max(spct_1),min(spct_2)))) * 100, 2)," percent."))
  print(paste0("Our modelled likelihood of ",fixtures$away$name[j]," winning this match OR DRAW is ",
               round((mean(spct_2)/sum(c(mean(spct_1),mean(spct_2))))*100, 2),
               " percent, with a minimum chance of ",
               round((min(spct_2)/sum(c(min(spct_2),max(spct_1)))) * 100, 2),  " percent, and a maximum chance of ",
               round((max(spct_2)/sum(c(max(spct_2),min(spct_1)))) * 100, 2)," percent."))
  
  
  mean_p_total <- mean(m_totals)
  
  
  
  mu <- mean_p_total
  x <- as.numeric(whsa$BARSTOOL_TOTAL[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1])
  probability <- ppois(x, lambda = mu, lower.tail = FALSE)
  
  # Print the probability
  probability
  
  mu <- max(m_totals)
  x <- as.numeric(whsa$BARSTOOL_TOTAL[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1])
  probability1 <- ppois(x, lambda = mu, lower.tail = FALSE)
  
  # Print the probability
  probability1
  
  mu <- min(m_totals)
  x <- as.numeric(whsa$BARSTOOL_TOTAL[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1])
  probability2 <- ppois(x, lambda = mu, lower.tail = FALSE)
  
  # Print the probability
  probability2
  
  print(paste0("The modelled likelihood of hitting the over ", x," is ",round(probability*100,2), " percent, with a high likelihood of ",
               round(probability1*100,2)," and a low likelihood of ",round(probability2*100,2)," percent."))
  
  print("--Sportsbook Odds--")
  
  h_odds <- whsa$BARSTOOL_TEAM1_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  a_odds <- whsa$BARSTOOL_TEAM2_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  d_odds <- whsa$BARSTOOL_X_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  
  h_spread_odds <- whsa$BARSTOOL_TEAM1_SPREAD_LINE[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  a_spread_odds <- whsa$BARSTOOL_TEAM2_SPREAD_LINE[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  
  total <- whsa$BARSTOOL_TOTAL[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  total_line_over <- whsa$BARSTOOL_OVER_LINE[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  
  print(paste0(fixtures$home$name[j]," Barstool Odds are: ", round(1/as.numeric(h_odds) * 100, 2)))
  print(paste0("The Barstool DRAW Odds are is: ", round(1/as.numeric(d_odds) * 100, 2)))
  print(paste0(fixtures$away$name[j]," Barstool Odds are: ", round(1/as.numeric(a_odds) * 100, 2)))
  
  print(paste0(fixtures$home$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(h_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(a_odds) * 100, 2))),3)))
  print(paste0("The Adjusted DRAW Barstool Odds are: ", round(100*round(1/as.numeric(d_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                             round(1/as.numeric(d_odds) * 100, 2),
                                                                                                             round(1/as.numeric(a_odds) * 100, 2))),3)))
  print(paste0(fixtures$away$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(a_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(a_odds) * 100, 2))),3)))
  
  print(paste0(fixtures$home$name[j],"'s Barstool Odds to cover the ",
               whsa$BARSTOOL_TEAM1_SPREAD[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1],
               " spread are: ", round(1/as.numeric(h_spread_odds) * 100, 2)))
  print(paste0(fixtures$away$name[j],"'s Barstool Odds to cover the ",
               whsa$BARSTOOL_TEAM2_SPREAD[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1],
               " spread are: ", round(1/as.numeric(a_spread_odds) * 100, 2)))
  
  
  print(paste0("Barstool's line for the total is ", total, " at ",total_line_over,", which is a ",round(1/as.numeric(total_line_over)*100,2)," percent chance of the over ",x, " hitting."))
  
  print("-----------------------------------")
  
  if((min(wpct_1)*100)>(1/as.numeric(h_odds) * 100)){
    card[q] <- fixtures$home$name[j]
    q <- q+1
  }
  
  if((min(wpct_X)*100)>(1/as.numeric(d_odds) * 100)){
    card[q] <- paste0("draw ",fixtures$home$name[j],"/",fixtures$away$name[j])
    q <- q+1
  }
  
  if((min(wpct_2)*100)>(1/as.numeric(a_odds) * 100)){
    card[q] <- fixtures$away$name[j]
    q <- q+1
  }
  
  if((probability1*100)<(1/as.numeric(total_line_over)*100)){
    card[q] <- paste0("under ",fixtures$home$name[j],"/",fixtures$away$name[j])
    q <- q+1
  }
  
  if((probability2*100)>(1/as.numeric(total_line_over)*100)){
    card[q] <- paste0("over ",fixtures$home$name[j],"/",fixtures$away$name[j])
    q <- q+1
  }
  if((min(spct_1) * 100)>(1/as.numeric(h_spread_odds) * 100)&whsa$BARSTOOL_TEAM1_SPREAD[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]>0){
    card[q] <- paste0(fixtures$home$name[j]," ATS")
    q <- q+1
  }
  if((min(spct_2) * 100)>(1/as.numeric(a_spread_odds) * 100)&whsa$BARSTOOL_TEAM2_SPREAD[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]>0){
    card[q] <- paste0(fixtures$away$name[j]," ATS")
    q <- q+1
  }
}
card
