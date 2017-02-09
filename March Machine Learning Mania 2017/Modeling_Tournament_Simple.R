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
Seed = read.csv("Seed.csv")
TCR = read.csv("TourneyCompactResults.csv")

# Only use data points after 1986, this allows to use seed_pre
Seed_sub = Seed[Seed$Season>=1986,c(1,5:7)]
TCR_sub = TCR[TCR$Season>=1986,]

# Modeling dataset structure should be like: 
# id, ID_SeasonTeam1, Team1, ID_SeasonTeam2, Team2, Seed_Team1, Seed_Team2, Seed_diff, 
# Seed_pre_Team1, Seed_pre_Team2, Seed_diff_self_Team1, Seed_diff_self_Team2
# Assumption: 
# 1. Team1 always has the smaller id number
# 2. If team didn't have seed in previous year, assume it to be 20

Tournament = TCR_sub
Tournament$Team1 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Lteam,Tournament$Wteam)
Tournament$Score_Team1 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Lscore,Tournament$Wscore)
Tournament$Team2 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Wteam,Tournament$Lteam)
Tournament$Score_Team2 = ifelse(Tournament$Wteam>Tournament$Lteam,Tournament$Wscore,Tournament$Lscore)
Tournament$Result = ifelse(Tournament$Score_Team1>Tournament$Score_Team2,"Win","Lose")

Tournament$id = paste(Tournament$Season,Tournament$Team1,Tournament$Team2,sep="_")
Tournament$ID_SeasonTeam1 = paste(Tournament$Season,Tournament$Team1,sep="_")
Tournament$ID_SeasonTeam2 = paste(Tournament$Season,Tournament$Team2,sep="_")

Tournament = Tournament[,c(14,1,2,15,9,16,11,13)]

# Merge seed information
Tournament_1 = merge(Tournament,Seed_sub,by.x="ID_SeasonTeam1",by.y="Team_ID",all.x=T)
Tournament_2 = merge(Tournament_1,Seed_sub,by.x="ID_SeasonTeam2",by.y="Team_ID",all.x=T,
                     suffixes = c("_Team1","_Team2"))
# Create seed differential variable
Tournament_2 = mutate(Tournament_2,Seed_Diff = Seed_num_Team2 - Seed_num_Team1)

# Create Tournament_allvars dataset
Tournament_allvars = Tournament_2[,c(3,4,5,2,6,1,7,9:15,8)]
write.csv(Tournament_allvars,"Tournament_allvars_0208.csv")

################### Exploratory Analysis ####################
mosaicplot(~ Seed_num_Team1 + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_num_Team2 + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_Diff + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_Pre_Team1 + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_Pre_Team2 + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_Diff_Team1 + as.factor(Result), data = Tournament_allvars, color = TRUE)
mosaicplot(~ Seed_Diff_Team2 + as.factor(Result), data = Tournament_allvars, color = TRUE)


################### Modeling #########################
# Use 30% Time series validation dataset, Season 2008-2016
Tournament_allvars_trn = Tournament_allvars[Tournament_allvars$Season<2008,]
Tournament_allvars_val = Tournament_allvars[Tournament_allvars$Season>=2008,]

control = trainControl(method="cv",number=5,classProbs = T)

## Decision Tree ##
system.time(mod_rpart <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                                 Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                               data=Tournament_allvars_trn,method="rpart",
                               trControl = control,tuneLength=20))
mod_rpart
varImp(mod_rpart)

rpartGrid = expand.grid(cp=seq(0.003,0.005,0.0001))
set.seed(1111) 
system.time(mod_rpart <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                                 Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                               data=Tournament_allvars_trn,method="rpart",
                               trControl = control,tuneGrid=rpartGrid))
mod_rpart
varImp(mod_rpart)
plot(varImp(mod_rpart))

rpart_pred = predict(mod_rpart,newdata=Tournament_allvars_val,type="raw")
confusionMatrix(rpart_pred,Tournament_allvars_val$Result,positive = "Win")

# Examine the ROC curve
rpart_pred_prob = predict(mod_rpart,newdata=Tournament_allvars_val,type="prob")
Tournament_pred = prediction(rpart_pred_prob$Win,Tournament_allvars_val$Result)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values)

# Build on entire train dataset and test on test set
# build on the whole trn dataset
Tournament_allvars_final = Tournament_allvars[,c(8:ncol(Tournament_allvars))]
rpartGrid_final = expand.grid(cp=0.004)
set.seed(1111)
system.time(mod_rpart <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                                 Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                               data=Tournament_allvars_final,method="rpart",
                               trControl = control,tuneGrid=rpartGrid_final))
mod_rpart
Test_Simple_0208 = read.csv("Test_Simple_0208.csv")
rpart_pred = predict(mod_rpart,newdata=Test_Simple_0208,type="prob")
Pred_Tournament_Simple_0208 = data.frame(id=Test_Simple_0208$id,pred=rpart_pred$Win)
write.csv(Pred_Tournament_Simple_0208,"Pred_Tournament_Simple_0208.csv",row.names = F)


## Random Forest ##
# System.time: 
system.time(mod_rf <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                              Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                            data=Tournament_allvars_trn,method="rf",
                            trControl = control, tuneLength=5))
mod_rf
varImp(mod_rf)

rfGrid = expand.grid(mtry=2)
set.seed(1111) 
system.time(mod_rf <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                              Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                            data=Tournament_allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
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

# Build on entire train dataset and test on test set
# build on the whole trn dataset
Tournament_allvars_final = Tournament_allvars[,c(8:ncol(Tournament_allvars))]
rfGrid_final = expand.grid(mtry=2)
set.seed(1111)
system.time(mod_rf <- train(as.factor(Result)~Seed_num_Team1+Seed_num_Team2+Seed_Pre_Team1+
                              Seed_Pre_Team2+Seed_Diff_Team1+Seed_Diff_Team2+Seed_Diff,
                            data=Tournament_allvars_final,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
Test_Simple_0208 = read.csv("Test_Simple_0208_rf.csv")
rf_pred = predict(mod_rf,newdata=Test_Simple_0208,type="prob")
Pred_Tournament_Simple_0208 = data.frame(id=Test_Simple_0208$id,pred=rf_pred$Win)
write.csv(Pred_Tournament_Simple_0208,"Pred_Tournament_Simple_0208_rf.csv",row.names = F)




