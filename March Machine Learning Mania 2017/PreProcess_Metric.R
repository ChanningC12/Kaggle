rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)

##################### Regular Season Compact Result #######################
# Create Unique game ID
RSCR$id = paste(RSCR$Season,"_",RSCR$Wteam,"_",RSCR$Lteam,sep="")
RSCR = RSCR[,c(9,1:8)]

# Create Lose team location
RSCR$Lloc = ifelse(RSCR$Wloc=="N","N",
                   ifelse(RSCR$Wloc=="H","A","H"))
table(RSCR$Wloc,RSCR$Lloc)

# Subset Winning team and Losing Team
RSCR_W = RSCR[,c(1:5,8,9,7)]
RSCR_L = RSCR[,c(1:3,6:7,10,9,5)]
# Assign result
RSCR_W$Result = "Win"
RSCR_L$Result = "Lose"

# Format title for appending
title = c("id","Season","Daynum","Team","Score","Location","Numot","Score_opp","Result")
colnames(RSCR_W) = title
colnames(RSCR_L) = title

# Merge the two tables
RSCR_all = rbind(RSCR_W,RSCR_L)
class(RSCR_all)

# Sort by Season, then daynum
RSCR_all = RSCR_all[order(RSCR_all$Season,RSCR_all$Daynum,RSCR_all$id),]

# Create Season+Team ID
RSCR_all$ID_SeasonTeam = paste(RSCR_all$Season,"_",RSCR_all$Team,sep="")
RSCR_all = RSCR_all[,c(1,10,2:9)]

##################### Regular Season Detailed Results #######################
# Create Unique game ID
RSDR$id = paste(RSDR$Season,"_",RSDR$Wteam,"_",RSDR$Lteam,sep="")
RSDR = RSDR[,c(35,1:34)]

# Create Lose team location
RSDR$Lloc = ifelse(RSDR$Wloc=="N","N",
                   ifelse(RSDR$Wloc=="H","A","H"))
table(RSDR$Wloc,RSDR$Lloc)

# Subset Winning team and Losing Team
RSDR_W = RSDR[,c(1:5,8:22,7,23:36)]
RSDR_L = RSDR[,c(1:3,6:7,36,9,23:35,5,10:22,8)]
# Assign result
RSDR_W$Result = "Win"
RSDR_L$Result = "Lose"

# Format title for appending
title = c("id","Season","Daynum","Team","Score","Location","Numot",
          "fgm","fga","fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf",
          "Score_opp","fgm_opp","fga_opp","fgm3_opp","fga3_opp","ftm_opp",
          "fta_opp","or_opp","dr_opp","ast_opp","to_opp","stl_opp","blk_opp","pf_opp","Location_opp",
          "Result")
colnames(RSDR_W) = title
colnames(RSDR_L) = title

# Merge the two tables
RSDR_all = rbind(RSDR_W,RSDR_L)
class(RSDR_all)

# Sort by Season, then daynum
RSDR_all = RSDR_all[order(RSDR_all$Season,RSDR_all$Daynum,RSDR_all$id),]

# Create Season+Team ID
RSDR_all$ID_SeasonTeam = paste(RSDR_all$Season,"_",RSDR_all$Team,sep="")
RSDR_all = RSDR_all[,c(1,37,2:36)]

################## Merge RSCR and RSDR tables ####################
Game = merge(RSCR_all,RSDR_all,by=c("id","ID_SeasonTeam","Team","Season","Daynum"),all.y = T)

# Check the data quality, score, location, numov, result
Game$score_qc <- Game$Score.x == Game$Score.y
Game$location_qc <- Game$Location.x == Game$Location.y
Game$numov_qc <- Game$Numov.x == Game$Numov.y
Game$result_qc <- Game$Result.x == Game$Result.y

table(Game$score_qc)
table(Game$location_qc)
table(Game$numov_qc)
table(Game$result_qc)

Game = Game[,c(1:9,14:26,28:42)]
names(Game)[6:9] = c("Score","Location","Numot","Score_opp")
names(Game)[37] = "Result"

write.csv(Game,"Game.csv",row.names = F)










