library(data.table)
library(dplyr)
library(DataCombine)
library(caret)
library(ROCR)

rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

# Important variables from the full model
mod_varImp_list = c(
  "Seed_Diff",
  "Seed_num_Team1",
  "Score_avg_diff_diff",
  "Win_score_diff",
  "Score_avg_diff_Team1",
  "Winning_percent_diff",
  "Games_won_Team1",
  "Seed_num_Team2",
  "Game_score_diff",
  "Score_to_Poss_diff",
  "OE_diff",
  "Winning_percent_Team1",
  "Seed_Pre_Team1",
  "EFF_diff",
  "Away_won_prob_diff",
  "D_to_PF_diff",
  "Score_avg_diff_Team2",
  "AST_to_TO_diff",
  "BLK_to_PF_diff",
  "Home_won_prob_diff",
  "D_to_PF_avg_Team1",
  "Score_to_Poss_avg_Team2",
  "PF_avg_Team1",
  "STL_to_TO_diff",
  "TR_avg_diff_Team2",
  "FGP3_opp_diff",
  "FGP3_opp_Team1",
  "FTA_to_FGA_diff",
  "Win_close_diff",
  "AST_to_SCORE_Team1",
  "AST_to_SCORE_diff",
  "FTP_Team2",
  "FTP_diff_diff",
  "OR_opp_diff",
  "FTP_diff",
  "PF_opp_diff",
  "AST_to_SCORE_Team2",
  "Winning_percent_close_Team2"
)

Tournament_modeling = read.csv("Intermediate Data/Modeling_tournament_0219.csv")
Tournament_modeling$Result_num = ifelse(Tournament_modeling$Result=="Win",1,0)
Tournament_modeling = Tournament_modeling[,names(Tournament_modeling) %in% mod_varImp_list | 
                                            names(Tournament_modeling) %in% c("Result","Result_num","Season")]

# Separate training and testing datasets
Tournament_allvars_trn = Tournament_modeling[Tournament_modeling$Season<2013,]
Tournament_allvars_val = Tournament_modeling[Tournament_modeling$Season>=2013,]

Tournament_allvars_trn = subset(Tournament_allvars_trn,
                                select=-c(Result_num,Season))

control = trainControl(method="cv",number=5,classProbs = T)

############### Exploratory Analysis ##################
allvars_cor = cor(subset(Tournament_modeling,select=-c(Season,Result,Result_num)),
                  Tournament_modeling$Result_num)
# transform correlation matrix to three column data frame
cor_summary = as.data.frame(as.table(allvars_cor))
colnames(cor_summary)[3] = "correlation"
# create absolute correlation
cor_summary$abs_cor = abs(cor_summary$correlation)
# sort by absolute correlation
cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]
cor_summary$Var2 = "Result_num"
write.csv(cor_summary,"Univariate_top_0220.csv",row.names = F)

####### Mosaic Plot
Mosaic = Tournament_modeling[,names(Tournament_modeling) %in% c(
  "Seed_Diff",
  "Seed_num_Team1",
  "Score_avg_diff_diff",
  "Win_score_diff",
  "Score_avg_diff_Team1",
  "Winning_percent_diff",
  "Games_won_Team1",
  "Seed_num_Team2",
  "Game_score_diff",
  "Score_to_Poss_diff",
  "OE_diff",
  "Result"
)]

# Create breaks
Mosaic = mutate(Mosaic,
                Seed_Diff_bins = cut(Seed_Diff,10),
                Seed_num_Team1_bins = cut(Seed_num_Team1,10),
                Score_avg_diff_diff_bins = cut(Score_avg_diff_diff,10),
                Win_score_diff_bins = cut(Win_score_diff,10),
                Score_avg_diff_Team1_bins = cut(Score_avg_diff_Team1,10),
                Winning_percent_diff_bins = cut(Winning_percent_diff,10),
                Games_won_Team1_bins = cut(Games_won_Team1,10),
                Seed_num_Team2_bins = cut(Seed_num_Team2,10),
                Game_score_diff_bins = cut(Game_score_diff,10),
                Score_to_Poss_diff_bins = cut(Score_to_Poss_diff,10),
                OE_diff_bins = cut(OE_diff,10)
                )

Mosaic = mutate(Mosaic,
                Seed_Diff_bins = ntile(Seed_Diff,5),
                Seed_num_Team1_bins = ntile(Seed_num_Team1,5),
                Score_avg_diff_diff_bins = ntile(Score_avg_diff_diff,5),
                Win_score_diff_bins = ntile(Win_score_diff,5),
                Score_avg_diff_Team1_bins = ntile(Score_avg_diff_Team1,5),
                Winning_percent_diff_bins = ntile(Winning_percent_diff,5),
                Games_won_Team1_bins = ntile(Games_won_Team1,5),
                Seed_num_Team2_bins = ntile(Seed_num_Team2,5),
                Game_score_diff_bins = ntile(Game_score_diff,5),
                Score_to_Poss_diff_bins = ntile(Score_to_Poss_diff,5),
                OE_diff_bins = ntile(OE_diff,5)
)

str(Mosaic)

mosaicplot(~ Seed_Diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Seed_num_Team1_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Score_avg_diff_diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Win_score_diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Score_avg_diff_Team1_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Winning_percent_diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Games_won_Team1_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Seed_num_Team2_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Game_score_diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ Score_to_Poss_diff_bins + Result, data = Mosaic, color = TRUE)
mosaicplot(~ OE_diff_bins + Result, data = Mosaic, color = TRUE)

############## Modeling ##################
## Random Forest ##
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=Tournament_allvars_trn,method="rf",
                            trControl = control))
mod_rf
varImp(mod_rf)
plot(mod_rf)
plot(varImp(mod_rf))

# Tuning mtry
rfGrid = expand.grid(mtry=c(10,15,20,25,30))
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
unlist(Tournament_auc@y.values) # 0.762

# Build on the whole trn dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(Season,Result_num))
rfGrid_final = expand.grid(mtry=10)
set.seed(1111)
system.time(mod_rf <- train(as.factor(Result)~.,
                            data=Tournament_allvars_final,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

Test_Full_0219 = read.csv("Intermediate Data/Test_Full_0219.csv")
Test_Top_0219 = Test_Full_0219[,names(Test_Full_0219) %in% names(Tournament_allvars_trn)]
rf_pred = predict(mod_rf,newdata=Test_Top_0219,type="prob")
Pred_Tournament_Top_0219 = data.frame(id=Test_Full_0219$id,pred=rf_pred$Win)
write.csv(Pred_Tournament_Top_0219,"Pred_Tournament_Top_0219_rf.csv",row.names = F)


######### Examine the wrong prediction pattern ###########
rf_pred_exam = data.frame(Tournament_allvars_val,Predict = rf_pred)
rf_pred_exam = dplyr::mutate(rf_pred_exam,
                      Predict_type = ifelse(rf_pred_exam$Result_num==1 & rf_pred_exam$Predict=="Win","TP",
                                            ifelse(rf_pred_exam$Result_num==1 & rf_pred_exam$Predict=="Lose","FN",
                                                   ifelse(rf_pred_exam$Result_num==0 & rf_pred_exam$Predict=="Win","FP","TN")
)))

table(rf_pred_exam$Predict_type)
tapply(rf_pred_exam$Seed_Diff,rf_pred_exam$Predict_type,mean)
prop.table(table(rf_pred_exam$Predict_type,rf_pred_exam$Seed_Diff),2)

rf_pred_percent = predict(mod_rf,newdata=Tournament_allvars_val,type="prob")
rf_pred_exam = data.frame(rf_pred_exam,Predict_prob = rf_pred_percent$Win)

# Create residual variable to examine the False predictions
rf_pred_exam = dplyr::mutate(rf_pred_exam,
                             residual = Result_num - Predict_prob)
tapply(rf_pred_exam$residual,rf_pred_exam$Predict_type,mean)



# Quick model Diagnostic
fast_mod = createDataPartition(rf_pred_exam$Predict_type,p=0.5,list=F)
fast_mod_trn = rf_pred_exam[fast_mod,]
fast_mod_val = rf_pred_exam[-fast_mod,]
fast_mod_trn = fast_mod_trn[,names(fast_mod_trn) %in% names(Tournament_allvars_final) |
                              names(fast_mod_trn) %in% c("residual")]
fast_mod_val = fast_mod_val[,names(fast_mod_val) %in% names(Tournament_allvars_final) |
                              names(fast_mod_val) %in% c("residual")]

fast_mod = rf_pred_exam[,names(rf_pred_exam) %in% names(Tournament_allvars_final) |
                          names(rf_pred_exam) %in% c("residual")]

control_fast = trainControl(method="cv",number=5,classProbs = F)
rfGrid_final = expand.grid(mtry=10)
system.time(mod_rf <- train(residual~.-Result,
                            data=fast_mod,method="rf",
                            trControl = control_fast,tuneGrid=rfGrid_final))
mod_rf
varImp(mod_rf)
plot(mod_rf)
plot(varImp(mod_rf))

rf_pred_fastmod = predict(mod_rf,newdata=fast_mod)
rf_pred_fastmod_res = data.frame(fast_mod,fastmod_pred = rf_pred_fastmod)
rf_pred_fastmod_res$fastmod_pred_abs = abs(rf_pred_fastmod_res$residual-rf_pred_fastmod_res$fastmod_pred)





