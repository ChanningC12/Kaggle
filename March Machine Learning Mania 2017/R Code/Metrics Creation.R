rm(list=ls())
gc()
getwd()
setwd("Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)

# Read in Game dataset
Game = read.csv("Game.csv")

# Group by ID_SeasonTeam
Team_metric <- Game %>%
  group_by(ID_SeasonTeam)%>%
  summarise(Games=n(), # Games played
            Games_won = sum(Result=="Win"), # Games won
            Winning_percent = Games_won/Games, # Winning %
            Score_avg = mean(Score), # Average scores per game
            Score_avg_opp = mean(Score_opp), # Average scores per game allowed
            Score_avg_diff = Score_avg-Score_avg_opp, # Average scores differential
            Overtime = sum(Numot), # Number of overtimes played
            Overtime_won_prob = sum(Numot>0 & Result=="Win") / Overtime, # Overtime games winning %
            Home_won_prob = sum(Location=="H" & Result=="Win") / sum(Location=="H"), # Home game winning %
            Away_won_prob = sum(Location=="A" & Result=="Win") / sum(Location=="A"), # Away game winning %
            Neutral_won_prob = sum(Location=="N" & Result=="Win") / sum(Location=="N"), # Neutral game winning %
            FGM_avg = mean(fgm), # Field Goal Made Average
            FGA_avg = mean(fga), # Field Goal Attempted Average
            FGP = FGM_avg/FGA_avg, # Field Goal %
            FGP_opp = mean(fgm_opp) / mean(fga_opp), # Field Goal % Allowed
            FGP_diff = FGP - FGP_opp, # Field Goal % Diff
            FGM3_avg = mean(fgm3), # 3 pointer made average
            FGA3_avg = mean(fga3), # 3 pointer attemped average
            FGP3 = FGM3_avg / FGA3_avg, # 3 pointer %
            FGP3_opp = mean(fgm3_opp) / mean(fga3_opp), # 3 pointer % allowed
            FGP3_diff = FGP3 - FGP3_opp, # 3 pointer % diff
            FTM_avg = mean(ftm), # Free throw made average
            FTA_avg = mean(fta), # Free throw attemped average
            FTP = FTM_avg / FTA_avg, # Free throw %
            FTP_opp = mean(ftm_opp) / mean(fta_opp), # Free throw % allowed
            FTP_diff = FTP - FTP_opp, # Free throw % diff
            OR_avg = mean(or), # Offensive rebound average
            OR_avg_opp = mean(or_opp), # Offensive rebound allowed
            DR_avg = mean(dr), # Defensive rebound average
            DR_avg_opp = mean(dr_opp), # Defensive rebound allowexd
            TR_avg = OR_avg + DR_avg, # Total rebounds average
            TR_avg_opp = OR_avg_opp + DR_avg_opp, # Total rebounds allowed
            TR_avg_diff = TR_avg - TR_avg_opp, # Total rebounds diff
            OR_percent = OR_avg / TR_avg, # Offensive rebound %
            AST_avg = mean(ast), # Assists average
            AST_avg_opp = mean(ast_opp), # Assists allowed
            TO_avg = mean(to), # turnovers
            TO_avg_opp = mean(to_opp), # turnovers forced
            AST_to_TO = AST_avg / TO_avg, # A/O
            AST_to_SCORE = AST_avg / Score_avg, # Assists to Scores
            STL_avg = mean(stl), # Steal average
            STL_to_TO = STL_avg / TO_avg, # STL to TO
            BLK_avg = mean(blk), # Blocks average
            PF_avg = mean(pf), # Personal fouls average
            PF_avg_opp = mean(pf_opp), # Personal fouls draw
            EFF = (Score_avg + TR_avg + AST_avg + STL_avg + BLK_avg) - 
              ((FGA_avg - FGM_avg) - (FTA_avg - FTM_avg) + TO_avg) # Team Efficiency
            )

# Imputing NaN in metrics dataset
colSums(is.na(Team_metric))
# Overtime_won_prob has 290 NaN, Neutral_won_prob has 196 NaN
# Let's assume the Overtime winning percentage and Neutral winning percentage are the same as winning %
Team_metric$Overtime_won_prob[is.na(Team_metric$Overtime_won_prob)] = Team_metric$Winning_percent[is.na(Team_metric$Overtime_won_prob)]
Team_metric$Neutral_won_prob[is.na(Team_metric$Neutral_won_prob)] = Team_metric$Winning_percent[is.na(Team_metric$Neutral_won_prob)]
Team_metric$Home_won_prob[is.na(Team_metric$Home_won_prob)] = Team_metric$Winning_percent[is.na(Team_metric$Home_won_prob)]
# Output the dataset
write.csv(Team_metric,"Team_metric_all.csv",row.names = F)


