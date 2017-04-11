rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)
library(caret)
library(ROCR)

# Read in seed,TCR,TDR datasets
Seed = read.csv("Intermediate Data/Seed.csv")
TDR = read.csv("Raw Data/TourneyDetailedResults.csv")
Team_metric = read.csv("Intermediate Data/Team_metric_all_0216.csv")

# Only use data points after 2003, this allows to use seed_pre
Seed_sub = Seed[Seed$Season>=2003,c(1,5:7)]
TDR_sub = TDR[TDR$Season>=2003,]

# Modeling dataset structure should be like: 
# id, ID_SeasonTeam1, Team1, ID_SeasonTeam2, Team2, Seed_Team1, Seed_Team2, Seed_diff, 
# Seed_pre_Team1, Seed_pre_Team2, Seed_diff_self_Team1, Seed_diff_self_Team2
# Append all the stats
# Assumption: 
# 1. Team1 always has the smaller id number
# 2. If team didn't have seed in previous year, assume it to be 20

Tournament = TDR_sub
Tournament$Team1 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Lteam,Tournament$Wteam)
Tournament$Score_Team1 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Lscore,Tournament$Wscore)
Tournament$Team2 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Wteam,Tournament$Lteam)
Tournament$Score_Team2 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Wscore,Tournament$Lscore)
Tournament$Result = ifelse(Tournament$Score_Team1>Tournament$Score_Team2,"Win","Lose")

Tournament$id = paste(Tournament$Season,Tournament$Team1,Tournament$Team2,sep="_")
Tournament$ID_SeasonTeam1 = paste(Tournament$Season,Tournament$Team1,sep="_")
Tournament$ID_SeasonTeam2 = paste(Tournament$Season,Tournament$Team2,sep="_")

Tournament = Tournament[,c(40,1,2,41,35,42,37,39)]

# Merge seed information
Tournament_1 = merge(Tournament,Seed_sub,by.x="ID_SeasonTeam1",by.y="Team_ID",all.x=T)
Tournament_2 = merge(Tournament_1,Seed_sub,by.x="ID_SeasonTeam2",by.y="Team_ID",all.x=T,
                     suffixes = c("_Team1","_Team2"))

# Create seed differential variable
Tournament_2 = mutate(Tournament_2,Seed_Diff = Seed_num_Team2 - Seed_num_Team1)

# Reorder the columns
Tournament_seed = Tournament_2[,c(3,4,5,2,6,1,7,9:15,8)]

# Merge metrics information
Tournament_seed_1 = merge(Tournament_seed,Team_metric,by.x="ID_SeasonTeam1",by.y="ID_SeasonTeam",all.x=T)
Tournament_seed_2 = merge(Tournament_seed_1,Team_metric,by.x="ID_SeasonTeam2",by.y="ID_SeasonTeam",all.x=T,
                          suffixes = c("_Team1","_Team2"))

# Reorder the columns
Tournament_allvars = Tournament_seed_2[,c(3,4,5,2,6,1,7,8:14,16:ncol(Tournament_seed_2),15)]

# Create differential variables, be careful all the opp variables need to be reversed in calculation
Tournament_modeling = mutate(Tournament_allvars,
                             # Add all the differential values
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
                             EFF_diff = EFF_Team1 - EFF_Team2,
                             Win_close_diff = Winning_percent_close_Team1 - Winning_percent_close_Team2,
                             TSP_diff = TSP_avg_Team1 - TSP_avg_Team2,
                             EFGP_diff = EFGP_avg_Team1 - EFGP_avg_Team2,
                             FTA_to_FGA_diff = FTA_to_FGA_avg_Team1 - FTA_to_FGA_avg_Team2,
                             BLK_to_PF_diff = BLK_to_PF_avg_Team1 - BLK_to_PF_avg_Team2,
                             D_to_PF_diff = D_to_PF_avg_Team1 - D_to_PF_avg_Team2,
                             Win_score_diff = Win_score_avg_Team1 - Win_score_avg_Team2,
                             Game_score_diff = Game_score_avg_Team1 - Game_score_avg_Team2,
                             Score_3_percent_diff = Score_3_percent_avg_Team1 - Score_3_percent_avg_Team2,
                             OE_diff = OE_avg_Team1 - OE_avg_Team2,
                             Score_to_Poss_diff = Score_to_Poss_avg_Team1 - Score_to_Poss_avg_Team2,
                             EOP_diff = EOP_avg_Team1 - EOP_avg_Team2
                             )

# Check the missing values
colSums(is.na(Tournament_modeling))
write.csv(Tournament_modeling,"Intermediate Data/Modeling_tournament_0219.csv",row.names = F)

############################ Modeling #####################################
Tournament_modeling$Result_num = ifelse(Tournament_modeling$Result=="Win",1,0)
table(Tournament_modeling$Season)
# Use 30% Time series validation dataset, Season 2013-2016
Tournament_allvars_trn = Tournament_modeling[Tournament_modeling$Season<2013,]
Tournament_allvars_val = Tournament_modeling[Tournament_modeling$Season>=2013,]

Tournament_allvars_trn = subset(Tournament_allvars_trn,
                                select=-c(id,Season,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                          Team1,Team2,Result_num))

control = trainControl(method="cv",number=5,classProbs = T)
## Random Forest ##
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=Tournament_allvars_trn,method="rf",
                            trControl = control))
mod_rf
varImp(mod_rf)
plot(mod_rf)
plot(varImp(mod_rf))

# Tuning mtry
rfGrid = expand.grid(mtry=c(10,15,20,25,30,35,40,45,50))
set.seed(1111) 
system.time(mod_rf <- train(as.factor(Result)~.,data=Tournament_allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=Tournament_allvars_val,type="raw")
confusionMatrix(rf_pred,Tournament_allvars_val$Result,positive = "Win")

# Examine the ROC curve
rf_pred_prob = predict(mod_rf,newdata=Tournament_allvars_val,type="prob")
Tournament_pred = prediction(rf_pred_prob$Win,Tournament_allvars_val$Result)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values)

# Build on the whole trn dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(id,Season,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                            Team1,Team2,Result_num))
rfGrid_final = expand.grid(mtry=40)
set.seed(1111)
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=Tournament_allvars_final,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
varImp(mod_rf)
plot(mod_rf)
plot(varImp(mod_rf))

Test_Full_0219 = read.csv("Intermediate Data/Test_Full_0219.csv")
rf_pred = predict(mod_rf,newdata=Test_Full_0219,type="prob")
Pred_Tournament_Full_0219 = data.frame(id=Test_Full_0219$id,pred=rf_pred$Win)
write.csv(Pred_Tournament_Full_0219,"Pred_Tournament_Full_0219_rf.csv",row.names = F)





