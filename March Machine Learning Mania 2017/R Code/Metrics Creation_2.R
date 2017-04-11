############## Additional Metrics ##################
# Reference: https://www.teamrankings.com/nba/team-stats/
# 1. Close game winning percentage: close game is defined as game within 10 points
# 2. True shooting percentage: PTS / 2*(FGA + 0.44*FTA)
# 3. Effective Field Goal Percentage: (FG + 0.5*3P) / FGA
# 4. Free throw attempt to Field goal attempt
# 5. Block per foul
# 6. Defensive plays per foul: (Steal + Blocks) / Fouls
# 7. Win score: points+rebounds+steals+0.5*Assists+0.5*blocked shots-field goal attempts-turnovers-0.5*FTA-0.5*PF
# 8. Game score: points+0.4*FG-0.7*FGA-0.4*(FTA-FT)+0.7*OR+0.3*DB+STL+0.7*AST+0.7*BLK-0.4*PF-TOV
# 9. Percent of points from 3 pointers: FG3*3 / avg scores
# From Basketball Analytics
# 10. Offensive Efficiency: (FG+A)/(FGA-ORB+A+TO)
# 11. Estimated Team Possessions: POSS = FGA+0.45*FTA+TO-ORB
# 12. Efficient Offensive Production: RAW EOP = 0.76*Assists+Points*OE
# 13. Defensive Stops Gained: -(0.82*0.735*EFG)-(0.42*ORB%)+1.06*TO%

rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)

# Read in Game dataset
Game = read.csv("Intermediate Data/Game.csv")

Game = mutate(Game,
              # 2. True shooting percentage: PTS / 2*(FGA + 0.44*FTA)
              TSP = Score/(2*(fga+0.44*fta)),
              TSP_opp = Score_opp/(2*(fga_opp+0.44*fta_opp)),
              # Close game ind
              Close_game_ind = ifelse(abs(Score-Score_opp)<10,1,0),
              # 3. Effective Field Goal Percentage: (FG + 0.5*3P) / FGA
              EFGP = (fgm+0.5*fgm3)/fga,
              # 4. Free throw attempt to Field goal attempt
              FTA_to_FGA = fta/fga,
              # 5. Block per foul
              BLK_to_PF = blk/pf,
              # 6. Defensive plays per foul: (Steal + Blocks) / Fouls
              D_to_PF = (stl+blk)/pf,
              # 7. Win score: points+rebounds+steals+0.5*Assists+0.5*blocked shots-field goal attempts-turnovers-0.5*FTA-0.5*PF
              Win_score = Score+or+dr+stl+0.5*ast+0.5*blk-fga-to-0.5*fta-0.5*pf,
              # 8. Game score: points+0.4*FG-0.7*FGA-0.4*(FTA-FT)+0.7*OR+0.3*DB+STL+0.7*AST+0.7*BLK-0.4*PF-TOV
              Game_score = Score+0.4*fgm-0.7*fga-0.4*(fta-ftm)+0.7*or+0.3*dr+stl+0.7*ast+0.7*blk-0.4*pf-to,
              # 9. Percent of points from 3 pointers: FG3*3 / avg scores
              Score_3_percent = fgm3*3/Score,
              # 10. Offensive Efficiency: (FG+A)/(FGA-ORB+A+TO)
              OE = (fgm+ast)/(fga-or+ast+to),
              # 11. Score per Estimated Team Possessions: POSS = FGA+0.45*FTA+TO-ORB
              Score_to_Poss = Score/(fga+0.45*fta+to-or),
              # 12. Efficient Offensive Production: RAW EOP = 0.76*Assists+Points*OE
              EOP = (0.76*ast+Score)*(fgm+ast)/(fga-or+ast+to)
              )

tapply(Game$TSP,Game$Result,mean)
tapply(Game$TSP_opp,Game$Result,mean)
  
Team_metric_add <- Game %>%
  group_by(ID_SeasonTeam)%>%
  dplyr::summarise(
    # 1. Close game winning percentage: close game is defined as game within 10 points
    Winning_percent_close = sum(Result=="Win" & Close_game_ind==1) / sum(Close_game_ind==1),
    # 2. True shooting percentage: PTS / 2*(FGA + 0.44*FTA)
    TSP_avg = mean(TSP), 
    TSP_avg_opp = mean(TSP_opp),
    # 3. Effective Field Goal Percentage: (FG + 0.5*3P) / FGA
    EFGP_avg = mean(EFGP),
    # 4. Free throw attempt to Field goal attempt
    FTA_to_FGA_avg = mean(FTA_to_FGA),
    # 5. Block per foul
    BLK_to_PF_avg = mean(BLK_to_PF),
    # 6. Defensive plays per foul: (Steal + Blocks) / Fouls
    D_to_PF_avg = mean(D_to_PF),
    # 7. Win Score
    Win_score_avg = mean(Win_score),
    # 8. Game Score
    Game_score_avg = mean(Game_score),
    # 9. Percent of points from 3 pointers: FG3*3 / avg scores
    Score_3_percent_avg = mean(Score_3_percent),
    # 10. Offensive Efficiency: (FG+A)/(FGA-ORB+A+TO)
    OE_avg = mean(OE),
    # 11. Score per Estimated Team Possessions: POSS = FGA+0.45*FTA+TO-ORB
    Score_to_Poss_avg = mean(Score_to_Poss),
    # 12. Efficient Offensive Production: RAW EOP = 0.76*Assists+Points*OE
    EOP_avg = mean(EOP)
    )

colSums(is.na(Team_metric_add))

# Output the dataset
write.csv(Team_metric_add,"Team_metric_add_0216.csv",row.names = F)

# Merge with the previous metrics
Team_metric = read.csv("Intermediate Data/Team_metric_all.csv")
Team_metric_all = merge(Team_metric,Team_metric_add,by="ID_SeasonTeam",all=T)
colSums(is.na(Team_metric_all))

write.csv(Team_metric_all,"Intermediate Data/Team_metric_all_0216.csv",row.names = F)





