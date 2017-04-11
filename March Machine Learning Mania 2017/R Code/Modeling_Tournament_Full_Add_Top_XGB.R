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
                                select=-c(Result,Season))
Tournament_allvars_val = subset(Tournament_allvars_val,
                                select=-c(Result,Season))

############## XGBoost ##################
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
xgb <- xgboost(data = data.matrix(Tournament_allvars_trn[,-39]), 
              label = Tournament_allvars_trn$Result_num,
              eta = 0.1,
              max_depth = 15, 
              nround=50, 
              objective = "binary:logistic"
)


xgb_cv <- xgb.cv(data = data.matrix(Tournament_allvars_trn[,-39]), 
               label = Tournament_allvars_trn$Result_num,
               eta = 0.1,
               max_depth = 15, 
               nround=50, 
               objective = "binary:logistic",
               nfold = 5 
)

xgb
summary(xgb)

# predict values in test set
y_pred <- predict(xgb, data.matrix(Tournament_allvars_val[,-39]))
mean(y_pred)
y_pred_class <- ifelse(y_pred>0.5,1,0)

# Examine the ROC curve
confusionMatrix(y_pred_class,Tournament_allvars_val$Result_num,positive="1")
Tournament_pred = prediction(y_pred,Tournament_allvars_val$Result_num)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values) # 0.756

# Build XGB on the full dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(Season,Result))

xgb <- xgboost(data = data.matrix(Tournament_allvars_final[,-39]), 
               label = Tournament_allvars_final$Result_num,
               eta = 0.1,
               max_depth = 15, 
               nround=50, 
               objective = "binary:logistic"
)


xgb_cv <- xgb.cv(data = data.matrix(Tournament_allvars_final[,-39]), 
                 label = Tournament_allvars_final$Result_num,
                 eta = 0.1,
                 max_depth = 15, 
                 nround=50, 
                 objective = "binary:logistic",
                 nfold = 5 
)

Test_Full_0303 = read.csv("Intermediate Data/Test_Full_0219.csv")
Test_Top_0303 = Test_Full_0303[,names(Test_Full_0303) %in% names(Tournament_allvars_trn)]
xgb_pred = predict(xgb,newdata = data.matrix(Test_Top_0303))
Pred_Tournament_Top_0303 = data.frame(id=Test_Full_0303$id,pred=xgb_pred)
write.csv(Pred_Tournament_Top_0303,"Pred_Tournament_Top_0303_xgb.csv",row.names = F)


######## With Elo Rating ##########
Tournament_modeling = read.csv("Intermediate Data/Modeling_tournament_0219.csv")
Tournament_modeling$Result_num = ifelse(Tournament_modeling$Result=="Win",1,0)
Tournament_modeling = merge(Tournament_modeling,Elo_all,by.x="ID_SeasonTeam1",by.y="Player",all.x=T)
Tournament_modeling = merge(Tournament_modeling,Elo_all,by.x="ID_SeasonTeam2",by.y="Player",all.x=T)

Tournament_modeling = Tournament_modeling[,names(Tournament_modeling) %in% mod_varImp_list | 
                                            names(Tournament_modeling) %in% c("Result","Result_num","Season",
                                                                              "Rating.x","Rating.y")]
Tournament_modeling$Rating_diff = Tournament_modeling$Rating.x - Tournament_modeling$Rating.y

# Separate training and testing datasets
Tournament_allvars_trn = Tournament_modeling[Tournament_modeling$Season<2013,]
Tournament_allvars_val = Tournament_modeling[Tournament_modeling$Season>=2013,]

Tournament_allvars_trn = subset(Tournament_allvars_trn,
                                select=-c(Result,Season))
Tournament_allvars_val = subset(Tournament_allvars_val,
                                select=-c(Result,Season))


############## XGBoost ##################
xgb <- xgboost(data = data.matrix(Tournament_allvars_trn[,-39]), 
               label = Tournament_allvars_trn$Result_num,
               eta = 0.1,
               max_depth = 15, 
               nround=50, 
               objective = "binary:logistic"
)


xgb_cv <- xgb.cv(data = data.matrix(Tournament_allvars_trn[,-39]), 
                 label = Tournament_allvars_trn$Result_num,
                 eta = 0.1,
                 max_depth = 15, 
                 nround=50, 
                 objective = "binary:logistic",
                 nfold = 5 
)

xgb
summary(xgb)

# predict values in test set
y_pred <- predict(xgb, data.matrix(Tournament_allvars_val[,-39]))
mean(y_pred)
y_pred_class <- ifelse(y_pred>0.5,1,0)

# Examine the ROC curve
confusionMatrix(y_pred_class,Tournament_allvars_val$Result_num,positive="1")
Tournament_pred = prediction(y_pred,Tournament_allvars_val$Result_num)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values) # 0.756

# Build XGB on the full dataset
Tournament_allvars_final = subset(Tournament_modeling,
                                  select=-c(Season,Result))

xgb <- xgboost(data = data.matrix(Tournament_allvars_final[,-39]), 
               label = Tournament_allvars_final$Result_num,
               eta = 0.1,
               max_depth = 15, 
               nround=50, 
               objective = "binary:logistic"
)


xgb_cv <- xgb.cv(data = data.matrix(Tournament_allvars_final[,-39]), 
                 label = Tournament_allvars_final$Result_num,
                 eta = 0.1,
                 max_depth = 15, 
                 nround=50, 
                 objective = "binary:logistic",
                 nfold = 5 
)

Test_Full_0303 = read.csv("Intermediate Data/Test_Full_0219.csv")
Elo_all = read.csv("Elo_rating_0303.csv")
Test_Full_0303 = merge(Test_Full_0303,Elo_all,by.x="ID_SeasonTeam1",by.y="Player",all.x=T)
Test_Full_0303 = merge(Test_Full_0303,Elo_all,by.x="ID_SeasonTeam2",by.y="Player",all.x=T)
Test_Full_0303$Rating_diff = Test_Full_0303$Rating.x - Test_Full_0303$Rating.y
Test_Top_0303 = Test_Full_0303[,names(Test_Full_0303) %in% names(Tournament_allvars_trn)]
xgb_pred = predict(xgb,newdata = data.matrix(Test_Top_0303))
Pred_Tournament_Top_0303 = data.frame(id=Test_Full_0303$id,pred=xgb_pred)
write.csv(Pred_Tournament_Top_0303,"Pred_Tournament_Top_0303_xgb_Elo.csv",row.names = F)





