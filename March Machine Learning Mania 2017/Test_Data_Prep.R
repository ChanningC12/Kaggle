rm(list=ls())
gc()
getwd()
setwd("Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)
library(caret)

# Read in Test Data
Test = read.csv("sample_submission.csv")

# Season
Test$Season = as.numeric(substr(Test$id,1,4))
Test$Team1 = as.numeric(substr(Test$id,6,9))
Test$Team2 = as.numeric(substr(Test$id,11,14))
Test$ID_SeasonTeam1 = paste(Test$Season,"_",Test$Team1,sep="")
Test$ID_SeasonTeam2 = paste(Test$Season,"_",Test$Team2,sep="")

Test = Test[,c(3,1,4,6,5,7)]

# Merge in Team1 metrics
Test_1 = merge(Test,Team_metric,by.x="ID_SeasonTeam1",by.y="ID_SeasonTeam",all.x=T)
Test_2 = merge(Test_1,Team_metric,by.x="ID_SeasonTeam2",by.y="ID_SeasonTeam",all.x=T,
                      suffixes = c("_Team1","Team_2"))
colSums(is.na(Test_2))

# Finalize Test dataset
Games_allvars_test = Test_2[,c(3,4,2,5,1,6,7:ncol(Test_2))]
write.csv(Games_allvars_test,"Games_allvars_test.csv",row.names = F)

############## Examine the Submission ID ##################
# Read in Test Data
Test_ID = read.csv("sample_submission.csv")
Seed = read.csv("Seed.csv")

# Create SeasonTeam Id
Test_ID$ID_SeasonTeam1 = paste(substr(Test_ID$id,1,4),substr(Test_ID$id,6,9),sep="_")
Test_ID$ID_SeasonTeam2 = paste(substr(Test_ID$id,1,4),substr(Test_ID$id,11,14),sep="_")

# Match the seed
Test_ID_team1 = merge(Test_ID,Seed,by.x="ID_SeasonTeam1",by.y="Team_ID",all.x=T)
Test_ID_team2 = merge(Test_ID_team1,Seed,by.x="ID_SeasonTeam2",by.y="Team_ID",all.x=T)
colSums(is.na(Test_ID_team2))

##### **Confirmed that the test dataset are games in tournament not regular seasons

############## Test set for Modeling_Tournament_Simple ##################
# Read in Test Data
Test_Simple = read.csv("sample_submission.csv")
Seed = read.csv("Seed.csv")
Seed_sub = Seed[Seed$Season>=1986,c(1,5:7)]

Test_Simple$Season = as.numeric(substr(Test_Simple$id,1,4))
Test_Simple$Team1 = as.numeric(substr(Test_Simple$id,6,9))
Test_Simple$Team2 = as.numeric(substr(Test_Simple$id,11,14))
Test_Simple$ID_SeasonTeam1 = paste(Test_Simple$Season,"_",Test_Simple$Team1,sep="")
Test_Simple$ID_SeasonTeam2 = paste(Test_Simple$Season,"_",Test_Simple$Team2,sep="")

Test_Simple_1 = merge(Test_Simple,Seed_sub,by.x="ID_SeasonTeam1",by.y="Team_ID",all.x=T)
Test_Simple_2 = merge(Test_Simple_1,Seed_sub,by.x="ID_SeasonTeam2",by.y="Team_ID",all.x=T,
                     suffixes = c("_Team1","_Team2"))
# Create seed differential variable
Test_Simple_2 = mutate(Test_Simple_2,Seed_Diff = Seed_num_Team2 - Seed_num_Team1)

# Create Tournament_allvars dataset
Test_Simple_0208 = Test_Simple_2[,c(3,5,2,6,1,7,8:14)]
write.csv(Test_Simple_0208,"Test_Simple_0208.csv",row.names = F)

############## Test set for Modeling_Tournament_Full ##################
# Read in Test Data
Test_Full = read.csv("sample_submission.csv")
Seed = read.csv("Seed.csv")
Seed_sub = Seed[Seed$Season>=2003,c(1,5:7)]

Test_Full$Season = as.numeric(substr(Test_Full$id,1,4))
Test_Full$Team1 = as.numeric(substr(Test_Full$id,6,9))
Test_Full$Team2 = as.numeric(substr(Test_Full$id,11,14))
Test_Full$ID_SeasonTeam1 = paste(Test_Full$Season,"_",Test_Full$Team1,sep="")
Test_Full$ID_SeasonTeam2 = paste(Test_Full$Season,"_",Test_Full$Team2,sep="")

Test_Full_1 = merge(Test_Full,Seed_sub,by.x="ID_SeasonTeam1",by.y="Team_ID",all.x=T)
Test_Full_2 = merge(Test_Full_1,Seed_sub,by.x="ID_SeasonTeam2",by.y="Team_ID",all.x=T,
                    suffixes = c("_Team1","_Team2"))
# Create seed differential variable
Test_Full_2 = mutate(Test_Full_2,Seed_Diff = Seed_num_Team2 - Seed_num_Team1)

Test_Seed_1 = merge(Test_Full_2,Team_metric,by.x="ID_SeasonTeam1",by.y="ID_SeasonTeam",all.x=T)
Test_Seed_2 = merge(Test_Seed_1,Team_metric,by.x="ID_SeasonTeam2",by.y="ID_SeasonTeam",all.x=T,
                    suffixes = c("_Team1","_Team2"))

Test_Full_0209 = mutate(Test_Seed_2,
                             Winning_percent_diff = Winning_percent_Team1 - Winning_percent_Team2,
                             Score_avg_diff = Score_avg_Team1 - Score_avg_Team2,
                             Score_avg_opp_diff = Score_avg_opp_Team2 - Score_avg_opp_Team1,
                             Score_avg_diff_diff = Score_avg_diff_Team1 - Score_avg_diff_Team2,
                             Overtime_won_prob_diff = Overtime_won_prob_Team1 - Overtime_won_prob_Team2,
                             Home_won_prob_diff = Home_won_prob_Team1 - Home_won_prob_Team2,
                             Away_won_prob_diff = Away_won_prob_Team1 - Away_won_prob_Team2,
                             Neutral_won_prob_diff = Neutral_won_prob_Team1 - Neutral_won_prob_Team2,
                             FGP_diff = FGP_Team1 - FGP_Team2,
                             FGP_opp_diff = FGP_opp_Team2 - FGP_opp_Team1,
                             FGP_diff_diff = FGP_diff_Team1 - FGP_diff_Team2,
                             FGP3_diff = FGP3_Team1 - FGP3_Team2,
                             FGP3_opp_diff = FGP3_opp_Team2 - FGP3_opp_Team1,
                             FGP3_diff_diff = FGP3_diff_Team1 - FGP3_diff_Team2,
                             FTP_diff = FTP_Team1 - FTP_Team2,
                             FTP_opp_diff = FTP_opp_Team2 - FTP_opp_Team1,
                             FTP_diff_diff = FTP_diff_Team1 - FTP_diff_Team2,
                             OR_diff = OR_avg_Team1 - OR_avg_Team2,
                             OR_opp_diff = OR_avg_opp_Team2 - OR_avg_opp_Team1,
                             DR_diff = DR_avg_Team1 - DR_avg_Team2,
                             DR_opp_diff = DR_avg_opp_Team2 - DR_avg_opp_Team1,
                             TR_diff = TR_avg_Team1 - TR_avg_Team2,
                             TR_opp_diff = TR_avg_opp_Team2 - TR_avg_opp_Team1,
                             TR_diff_diff = TR_avg_diff_Team1 - TR_avg_diff_Team2,
                             OR_percent_diff = OR_percent_Team1 - OR_percent_Team2,
                             AST_diff = AST_avg_Team1 - AST_avg_Team2,
                             AST_opp_diff = AST_avg_opp_Team2 - AST_avg_opp_Team1,
                             TO_diff = TO_avg_Team2 - TO_avg_Team1,
                             TO_opp_diff = TO_avg_opp_Team2 - TO_avg_opp_Team1,
                             AST_to_TO_diff = AST_to_TO_Team1 - AST_to_TO_Team2,
                             AST_to_SCORE_diff = AST_to_SCORE_Team1 - AST_to_SCORE_Team2,
                             STL_diff = STL_avg_Team1 - STL_avg_Team2,
                             STL_to_TO_diff = STL_to_TO_Team1 - STL_to_TO_Team2,
                             BLK_diff = BLK_avg_Team1 - BLK_avg_Team2,
                             PF_diff = PF_avg_Team2 - PF_avg_Team1,
                             PF_opp_diff = PF_avg_opp_Team1 - PF_avg_opp_Team2,
                             EFF_diff = EFF_Team1 - EFF_Team2)

# Create Tournament_allvars dataset
Test_Full_0209 = Test_Full_0209[,c(3,5,2,6,1,7,8:ncol(Test_Full_0209))]
write.csv(Test_Full_0209,"Test_Full_0209",row.names = F)





