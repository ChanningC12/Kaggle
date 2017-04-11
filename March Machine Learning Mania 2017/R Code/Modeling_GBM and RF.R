rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)
library(caret)
library(ROCR)

Tournament_modeling = read.csv("Intermediate Data/Modeling_tournament_0209.csv")
Tournament_modeling$Result_num = ifelse(Tournament_modeling$Result=="Win",1,0)

# Use 30% Time series validation dataset, Season 2013-2016
Tournament_allvars_trn = Tournament_modeling[Tournament_modeling$Season<2013,]
Tournament_allvars_val = Tournament_modeling[Tournament_modeling$Season>=2013,]

Tournament_allvars_trn = subset(Tournament_allvars_trn,
                                select=-c(id,Season,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                          Team1,Team2,Result_num))

control = trainControl(method="cv",number=5,classProbs = T)

## GBM ##
system.time(mod_gbm <- train(as.factor(Result)~.,
                             data=Tournament_allvars_trn,method="gbm",
                             trControl = control))
mod_gbm
mod_gbm$finalModel
varImp(mod_gbm)

gbmGrid = expand.grid(interaction.depth=c(1,2),
                      n.trees=50,
                      shrinkage=seq(0,0.2,0.02),
                      n.minobsinnode=10)
set.seed(1111) 
system.time(mod_gbm <- train(as.factor(Result)~.,data=Tournament_allvars_trn,method="gbm",
                             trControl = control,tuneGrid=gbmGrid))
mod_gbm
mod_gbm$finalModel
varImp(mod_gbm)
plot(varImp(mod_gbm))

gbm_pred = predict(mod_gbm,newdata=Tournament_allvars_val,type="raw")
confusionMatrix(gbm_pred,Tournament_allvars_val$Result,positive = "Win")

# Examine the ROC curve
gbm_pred_prob = predict(mod_gbm,newdata=Tournament_allvars_val,type="prob")
Tournament_pred = prediction(gbm_pred_prob$Win,Tournament_allvars_val$Result)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values)

# Build on entire train dataset and test on test set
# build on the whole trn dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(id,Season,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                            Team1,Team2,Result_num))
gbmGrid_final = expand.grid(interaction.depth=1,
                            n.trees=50,
                            shrinkage=0.1,
                            n.minobsinnode=10)
set.seed(1111)
system.time(mod_gbm <- train(as.factor(Result)~.,
                             data=Tournament_allvars_final,method="gbm",
                             trControl = control,tuneGrid=gbmGrid_final))
mod_gbm

Test_Full_0209 = read.csv("Intermediate Data/Test_Full_0209.csv")
gbm_pred = predict(mod_gbm,newdata=Test_Full_0209,type="prob")
Pred_Tournament_Full_0209 = data.frame(id=Test_Full_0209$id,pred=gbm_pred$Win)
write.csv(Pred_Tournament_Full_0209,"Pred_Tournament_Full_0210_gbm.csv",row.names = F)



## Random Forest ##
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=Tournament_allvars_trn,method="rf",
                            trControl = control,tuneLength=20))
mod_rf
mod_rf$finalModel
varImp(mod_rf)

rfGrid = expand.grid(mtry=c(10,15,20,25,30,35,40,45,50))
set.seed(1111) 
system.time(mod_rf <- train(as.factor(Result)~.,data=Tournament_allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
mod_rf$finalModel
rfVarImp = varImp(mod_rf)
rfVarImp = data.frame(rfVarImp$importance)
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

# Filter non significant variables
rfGrid = expand.grid(mtry=20)
set.seed(1111) 
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=subset(Tournament_allvars_trn,
                                        select=-c(Overtime_Team1,Overtime_won_prob_Team1,
                                                  Overtime_won_prob_Team2,Overtime_Team2,
                                                  Games_Team1,Games_Team2,Games_won_Team2,
                                                  Neutral_won_prob_Team1,TR_avg_opp_Team1,
                                                  Neutral_won_prob_Team2,FTM_avg_Team1,
                                                  FGP_opp_Team2,AST_avg_opp_Team2,
                                                  Home_won_prob_Team2,TO_avg_Team2,
                                                  Score_avg_opp_Team2,FTP_Team1,
                                                  TR_avg_Team1,STL_avg_Team1,DR_avg_Team1,
                                                  Home_won_prob_Team1, # 0211 submission
                                                  Seed_Diff_Team2,Seed_Pre_Team2,STL_avg_Team2, # AUC 0.733
                                                  FTP_diff_Team1,FTA_avg_Team1,TO_opp_diff)),
                            method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
rfVarImp = varImp(mod_rf)
rfVarImp = data.frame(rfVarImp$importance)
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

# Top 10 Predictors
rfGrid = expand.grid(mtry=20)
set.seed(1111) 
system.time(mod_rf <- train(as.factor(Result)~Seed_Diff+Seed_num_Team1+Score_avg_diff_diff+
                              Seed_num_Team2+Score_avg_diff_Team1+Games_won_Team1+Winning_percent_Team1+
                              EFF_diff+Winning_percent_diff+FGP3_opp_diff,
                            data=Tournament_allvars_trn,
                            method="rf",
                            trControl = control))

mod_rf
rfVarImp = varImp(mod_rf)
rfVarImp = data.frame(rfVarImp$importance)
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
unlist(Tournament_auc@y.values) # AUC: 0.74


# Build on entire train dataset and test on test set
# build on the whole trn dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(id,Season,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                            Team1,Team2,Result_num))
rfGrid_final = expand.grid(mtry=6)
set.seed(1111)
system.time(mod_rf <- train(as.factor(Result)~Seed_Diff+Seed_num_Team1+Score_avg_diff_diff+
                              Seed_num_Team2+Score_avg_diff_Team1+Games_won_Team1+Winning_percent_Team1+
                              EFF_diff+Winning_percent_diff+FGP3_opp_diff,
                            data=Tournament_allvars_final
                                        ,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf

Test_Full_0209 = read.csv("Intermediate Data/Test_Full_0209.csv")
rf_pred = predict(mod_rf,newdata=Test_Full_0209,type="prob")
Pred_Tournament_Full_0209 = data.frame(id=Test_Full_0209$id,pred=rf_pred$Win)
write.csv(Pred_Tournament_Full_0209,"Pred_Tournament_Full_0211_rf_seedDiff.csv",row.names = F)



