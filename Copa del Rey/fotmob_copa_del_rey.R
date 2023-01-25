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

setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/Copa Del Rey")
source("fotmob_functions.R")


########## Variables ########

l <- fotmob_get_league_ids(cached = FALSE)
View(l)

leagues <- c(334,335,339)
league_name <- "Copa del Rey"
ccode <- "ESP"
team_df <- data.frame(team_1 = c("Sevilla","Girona","Athletic Club","CD Mirandes","Sporting Gijon","Deportivo Alaves","Burgos CF","Malaga","Cartagena","Ponferradina" ),
                      team_2 = c("Sevilla FC","Girona FC","Athletic Bilbao","Mirandes","Sporting GijÃ³n","AlavÃ©s","Burgos","MÃ¡laga","FC Cartagena","SD Ponferradina" ))

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
e_teams_e <- e1_stat[[2]]

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

stat_afc <- stat_afc[order(-stat_afc$minutes),]

day_lineups$pass_share <- 0
day_lineups$tackle_share <- 0
day_lineups$shot_share <- 0
day_lineups$sog_share <- 0 
day_lineups$xg_share <- 0
day_lineups$xgot_share <- 0
day_lineups$xa_share <- 0 



for(i in 1:length(day_lineups$match_id)){
  
  max_sum_passes <- stat_afc$pass_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_tackles <- stat_afc$tkl_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_shots <- stat_afc$shots_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_sogs <- stat_afc$sog_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_xg <- stat_afc$xg_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_xgot <- stat_afc$xgot_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  max_sum_xa <- stat_afc$xa_90min[stat_afc$team_name==day_lineups$team[i]&stat_afc$position_row==day_lineups$positionRow[i]][c(1:length(which(day_lineups$positionRow==day_lineups$positionRow[i]&day_lineups$team[i]==day_lineups$team)))]
  
  max_sum_passes <- sum(unlist(max_sum_passes[!is.nan(max_sum_passes)]))
  max_sum_tackles <- sum(unlist(max_sum_tackles[!is.nan(max_sum_tackles)]))
  max_sum_shots <- sum(unlist(max_sum_shots[!is.nan(max_sum_shots)]))
  max_sum_sogs <- sum(unlist(max_sum_sogs[!is.nan(max_sum_sogs)]))
  max_sum_xg <- sum(unlist(max_sum_xg[!is.nan(max_sum_xg)]))
  max_sum_xgot <- sum(unlist(max_sum_xgot[!is.nan(max_sum_xgot)]))
  max_sum_xa <- sum(unlist(max_sum_xa[!is.nan(max_sum_xa)]))
  
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


####### Fotmob Fixtures w/ SPI #######
l <- fotmob_get_league_ids(cached = FALSE)
fixtures <- fotmob_get_league_matches(ccode,league_name, cached = FALSE)
fixtures <- fixtures[fixtures$status$utcTime>=Sys.Date(),]
date <- unique(as.Date(fixtures$status$utcTime))
date <- date[order(date)]
date <- date[date<(date[1]+3)]
fixtures <- fixtures[as.Date(fixtures$status$utcTime)<(date[1]+3),]

fixtures_f <-  read.csv(paste0("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"))
fixtures_f <- fixtures_f[fixtures_f$date>as.Date("2022-07-01"),]
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

fixtures$spi1 <- 0
fixtures$spi2 <- 0
for(i in 1:length(fixtures$round)){
  
  if(is.element(fixtures$home$name[i],f_teams$team2)){
    fixtures$home$name[i] <- f_teams$team[which(f_teams$team2==fixtures$home$name[i])][1]
  }
  
  if(is.element(fixtures$home$name[i],team_df$team_1)){
    fixtures$home$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$home$name[i])]
  }
  
  if(is.element(fixtures$away$name[i],f_teams$team2)){
    fixtures$away$name[i] <- f_teams$team[which(f_teams$team2==fixtures$away$name[i])][1]
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
  fixtures_f2 <- fixtures_f2[fixtures_f2$date>=Sys.Date(),]
  fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
  fixtures_f2 <- fixtures_f2[1,]
  
  
  
  
  fixtures$spi1[i] <- ifelse(length(fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])==0,
                             fixtures_f2$spi2[which(fixtures$home$name[i]==fixtures_f2$team2)],
                             fixtures_f2$spi1[which(fixtures$home$name[i]==fixtures_f2$team1)])
  
  
  
  fixtures_f2 <- fixtures_f[c(which(fixtures_f$team2==fixtures$away$name[i]),
                              which(fixtures_f$team1==fixtures$away$name[i])),]
  fixtures_f2 <- fixtures_f2[fixtures_f2$date>=Sys.Date(),]
  fixtures_f2 <- fixtures_f2[order(fixtures_f2$date),]
  fixtures_f2 <- fixtures_f2[1,]
  
  
  
  
  
  fixtures$spi2[i] <- ifelse(length(fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])==0,
                             fixtures_f2$spi1[which(fixtures$away$name[i]==fixtures_f2$team1)],
                             fixtures_f2$spi2[which(fixtures$away$name[i]==fixtures_f2$team2)])
  
}

fixtures_ff <- fixtures[fixtures$spi1!=0,]

fixtures <- fotmob_get_league_matches(ccode,league_name, cached = FALSE)
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

fixtures$spi1 <- 0
fixtures$spi2 <- 0
for(i in 1:length(fixtures$round)){
  if(length(fixtures_ff$spi1[which(fixtures$away$shortName[i]==fixtures_ff$away$shortName)])==0){
    next()
  }
  if(length(fixtures_ff$spi1[which(fixtures$home$shortName[i]==fixtures_ff$home$shortName)])==0){
    next()
  }
  fixtures$spi1[i] <- fixtures_ff$spi1[which(fixtures$away$shortName[i]==fixtures_ff$away$shortName)]
  fixtures$spi2[i] <- fixtures_ff$spi2[which(fixtures$away$shortName[i]==fixtures_ff$away$shortName)]
  if(is.element(fixtures$home$name[i],team_df$team_1)){
    fixtures$home$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$home$name[i])]
  }
  
  if(is.element(fixtures$away$name[i],team_df$team_1)){
    fixtures$away$name[i] <- team_df$team_2[which(team_df$team_1==fixtures$away$name[i])]
  }
}

fixtures <- fixtures[fixtures$spi1!=0,]


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

####### Game Prediction ########

team_df2 <-  data.frame(team_1 = c("Sporting de Gijón" ,"Atlético Madrid","Alavés","Valladolid"  ),
                        team_2 = c("Sporting GijÃ³n" ,"Atletico Madrid","AlavÃ©s","Real Valladolid"  ))




setwd("C:/Users/CORSAIR GAMING/Documents/FotMob Soccer Modelling/Copa del Rey")

source("running_copa_del_rey_lines.R")
whsa
unique(c(unique(whsa$BARSTOOL_TEAM1),unique(whsa$BARSTOOL_TEAM2)))
unique(day_lineups$team)

for(i in 1:length(team_df2$team_1)){
  
  whsa$BARSTOOL_TEAM1[whsa$BARSTOOL_TEAM1==team_df2$team_1[i]] <- team_df2$team_2[i]
  whsa$BARSTOOL_TEAM2[whsa$BARSTOOL_TEAM2==team_df2$team_1[i]] <- team_df2$team_2[i]
  
}

test1 <- stat_game$passes[!is.na(stat_game$passes)]
test1_1 <- stat_game$shots[!is.na(stat_game$shots)]
test1_2 <- stat_game$shotsongoal[!is.na(stat_game$shotsongoal)]
test2 <- stat_game$goals[!is.na(stat_game$expected_goals)]
test3 <- stat_game$is_mc[!is.na(stat_game$passes)]

ggplot(data.frame(test1,test2),aes(x=test1_1,
                                   y=test1_2,
                                   col=test3))+geom_point()+geom_smooth(method='lm', formula= y~x)

xg_exp <- summary(lm(test2~test1+test1_2))

for(j in 1:length(fixtures$round)){
  
  h_odds <- whsa$BARSTOOL_TEAM1_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  a_odds <- whsa$BARSTOOL_TEAM2_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  d_odds <- whsa$BARSTOOL_X_ML[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  
  total <- whsa$BARSTOOL_TOTAL[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  total_line_over <- whsa$BARSTOOL_OVER_LINE[whsa$BARSTOOL_TEAM1==fixtures$home$name[j]&whsa$BARSTOOL_TEAM2==fixtures$away$name[j]][1]
  
  print(paste0(fixtures$home$name[j]," Barstool Odds are: ", round(1/as.numeric(h_odds) * 100, 2)))
  print(paste0("The Barstool Odds are is: ", round(1/as.numeric(d_odds) * 100, 2)))
  print(paste0(fixtures$away$name[j]," Barstool Odds are: ", round(1/as.numeric(a_odds) * 100, 2)))
  
  print(paste0(fixtures$home$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(h_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(a_odds) * 100, 2))),3)))
  print(paste0("The Adjusted Barstool Odds are is: ", round(100*round(1/as.numeric(d_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                           round(1/as.numeric(d_odds) * 100, 2),
                                                                                                           round(1/as.numeric(a_odds) * 100, 2))),3)))
  print(paste0(fixtures$away$name[j]," Adjusted Barstool Odds are: ", round(100*round(1/as.numeric(a_odds) * 100, 2)/sum(c(round(1/as.numeric(h_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(d_odds) * 100, 2),
                                                                                                                           round(1/as.numeric(a_odds) * 100, 2))),3)))
  
  print(paste0("Barstool's line for the total is ", total, " at ",total_line_over))
  
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
  
  print(paste0(fixtures$home$name[j]," pass/shot Predicted Win %: ", round(team_one_win_pct * 100, 2)))
  print(paste0("The pass/shot Predicted Draw % is: ", round(team_one_draw_pct * 100, 2)))
  print(paste0(fixtures$away$name[j]," pass/shot Predicted Win %: ", round(team_two_win_pct * 100, 2)))
  
  
  
  
  
  print("-----------------------------------")
}




######### Player Prop Prediction #########

player_prop_list <- player_prop_lines()



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


stat_afc$min_p90min <- stat_afc$minutes/stat_afc$games
player_stats_game$min_p90min <- 0
for(b in 1:length(player_stats_game$pageUrl)){
  if(length(stat_afc$min_p90min[stat_afc$page_url==player_stats_game$pageUrl[b]&stat_afc$position_row==player_stats_game$positionRow[b]])==0){
    player_stats_game$min_p90min[b] <- mean(stat_afc$min_p90min[stat_afc$position_row==player_stats_game$positionRow[b]&player_stats_game$team[b]==stat_afc$team_name])
  }else{
    if(stat_afc$minutes[stat_afc$page_url==player_stats_game$pageUrl[b]&stat_afc$position_row==player_stats_game$positionRow[b]]<90){
      player_stats_game$min_p90min[b] <- mean(stat_afc$min_p90min[stat_afc$position_row==player_stats_game$positionRow[b]&player_stats_game$team[b]==stat_afc$team_name])
    }else{
      player_stats_game$min_p90min[b] <- stat_afc$min_p90min[stat_afc$page_url==player_stats_game$pageUrl[b]&stat_afc$position_row==player_stats_game$positionRow[b]]
    }
  }
  
  
}

player_stats_game$min_p90min[player_stats_game$positionRow==0] <- 90
player_stats_game$min_p90min <- ifelse(player_stats_game$min_p90min>90,90,player_stats_game$min_p90min)


for(b in 1:length(player_stats_game$pageUrl)){
  
  team_mins <- player_stats_game$min_p90min[player_stats_game$team==player_stats_game$team[b]]
  
  team_max_mins <- team_mins[order(-team_mins)][c(1:6)]
  
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


player_stats_game$expected_passes_low <- player_stats_game$expected_passes_low*player_stats_game$exp_min/90
player_stats_game$expected_passes <- player_stats_game$expected_passes*player_stats_game$exp_min/90
player_stats_game$expected_passes_high <- player_stats_game$expected_passes_high*player_stats_game$exp_min/90

player_stats_game$expected_tackles_low <- player_stats_game$expected_tackles_low*player_stats_game$exp_min/90
player_stats_game$expected_tackles <- player_stats_game$expected_tackles*player_stats_game$exp_min/90
player_stats_game$expected_tackles_high <- player_stats_game$expected_tackles_high*player_stats_game$exp_min/90

player_stats_game$expected_shots_low <- player_stats_game$expected_shots_low*player_stats_game$exp_min/90
player_stats_game$expected_shots <- player_stats_game$expected_shots*player_stats_game$exp_min/90
player_stats_game$expected_shots_high <- player_stats_game$expected_shots_high*player_stats_game$exp_min/90

player_stats_game$expected_sogs_low <- player_stats_game$expected_sogs_low*player_stats_game$exp_min/90
player_stats_game$expected_sogs <- player_stats_game$expected_sogs*player_stats_game$exp_min/90
player_stats_game$expected_sogs_high <- player_stats_game$expected_sogs_high*player_stats_game$exp_min/90

player_stats_game$expected_xg_low <- player_stats_game$expected_xg_low*player_stats_game$exp_min/90
player_stats_game$expected_xg <- player_stats_game$expected_xg*player_stats_game$exp_min/90
player_stats_game$expected_xg_high <- player_stats_game$expected_xg_high*player_stats_game$exp_min/90

player_stats_game$expected_xa_low <- player_stats_game$expected_xa_low*player_stats_game$exp_min/90
player_stats_game$expected_xa <- player_stats_game$expected_xa*player_stats_game$exp_min/90
player_stats_game$expected_xa_high <- player_stats_game$expected_xa_high*player_stats_game$exp_min/90



key_player_sogs <- player_stats_game[,c("player_name"      ,         "positionRow"                ,       "team"     ,
                                        "expected_sogs_low","expected_sogs","expected_sogs_high","exp_min"  )]

key_player_shots <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                         "expected_shots_low","expected_shots","expected_shots_high", "exp_min" )]


key_player_tackles <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                           "expected_tackles_low","expected_tackles","expected_tackles_high","exp_min")]


key_player_passes <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                          "expected_passes_low","expected_passes","expected_passes_high","exp_min" )]

key_player_xg <- player_stats_game[,c("player_name"      ,         "positionRow"               ,       "team"     ,
                                           "expected_xg_low","expected_xg","expected_xg_high","exp_min")]


key_player_xa <- player_stats_game[,c("player_name"      ,         "positionRow"              ,       "team"     ,
                                          "expected_xa_low","expected_xa","expected_xa_high","exp_min" )]


key_player_tackles$player_name <- stri_trans_general(str = key_player_tackles$player_name, id = "Latin-ASCII")
key_player_passes$player_name <- stri_trans_general(str = key_player_passes$player_name, id = "Latin-ASCII")
key_player_shots$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
key_player_sogs$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")
key_player_xg$player_name <- stri_trans_general(str = key_player_shots$player_name, id = "Latin-ASCII")
key_player_xa$player_name <- stri_trans_general(str = key_player_sogs$player_name, id = "Latin-ASCII")

unique(all_player_passes_lines$team)

b_team <- c("MUN", "WOL", "BOU", "CRY", "FUL", "SOU", "EVE", "MCI", "LEE", "NEW", "ARS", "BHA", "TOT", "AVL" ,"NOF", "CHE","LIV","BRE","WHU","LEI")
f_team <- c(   "Manchester United" ,     "Wolverhampton"   ,              
            "AFC Bournemouth"   ,       "Crystal Palace"     ,      "Fulham"          ,         "Southampton"       ,   "Everton"  ,    "Manchester City" ,                        
            "Leeds United"   ,  "Newcastle"          ,         "Arsenal"    ,      "Brighton and Hove Albion"   ,            "Tottenham Hotspur"   ,     "Aston Villa"      ,       
           "Nottingham Forest"     ,   "Chelsea","Liverpool","Brentford","West Ham United","Leicester City")

team_df3 <- data.frame(b_team,f_team)

all_player_goals_lines$ex_prob <- 0
options(scipen = 999)
for(a in 1:length(all_player_goals_lines$player)){
  game_key_player_goals <- key_player_xg[which(key_player_xg$team==team_df3$f_team[team_df3$b_team==all_player_goals_lines$team[a]]),]
  h_name <- tolower(gsub(" ","-",all_player_goals_lines$player[a]))
  
  if(any(grepl(h_name,game_key_player_goals$player_name))){
    
    
    
    mu <- game_key_player_goals$expected_xg[which(grepl(h_name,game_key_player_goals$player_name))]
    x <- all_player_goals_lines$number[a]
    probability <- ppois(x, lambda = mu, lower.tail = FALSE)
    
    # Print the probability
    probability
    
    all_player_goals_lines$ex_prob[a] <- round(probability,4)
    
  }else{
    
    h_name_2 <- tolower(paste0(strsplit(all_player_goals_lines$player[a]," ")[[1]][1],"-"))
    
    if(length(which(grepl(h_name_2,game_key_player_goals$player_name)))==1){
      
      mu <- game_key_player_goals$expected_xg[which(grepl(h_name_2,game_key_player_goals$player_name))]
      x <- all_player_goals_lines$number[a]
      probability <- ppois(x, lambda = mu, lower.tail = FALSE)
      
      # Print the probability
      probability
      
      all_player_goals_lines$ex_prob[a] <- round(probability,4)
      
    }else{
      
      h_name_3 <- tolower(paste0("-",strsplit(all_player_goals_lines$player[a]," ")[[1]][2]))
      
      if(length(which(grepl(h_name_3,game_key_player_goals$player_name)))==1){
        
        mu <- game_key_player_goals$expected_xg[which(grepl(h_name_3,game_key_player_goals$player_name))]
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
all_player_goals_lines <- all_player_goals_lines[all_player_goals_lines$ex_prob>0,]
if(length(all_player_ast_lines$ex_prob>0)==0){
  
}else{
  all_player_ast_lines <- all_player_ast_lines[all_player_ast_lines$ex_prob>0,]
}


plot(all_player_passes_lines$prob_passes,all_player_passes_lines$number)

all_player_passes_lines <- all_player_passes_lines[-which(all_player_passes_lines$number<7.5),]

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

View(pass_lines)
View(sogs_lines)




