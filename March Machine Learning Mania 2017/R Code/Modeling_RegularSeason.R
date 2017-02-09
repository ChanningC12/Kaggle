rm(list=ls())
gc()
getwd()
setwd("Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)
library(caret)

# Read in Game dataset
Team_metric = read.csv("Team_metric.csv")
Games = read.csv("Game.csv")
Games_sub = Games[Games$Season>=2011,]
rm(Games)

# Subset W/L
Games_W = Games_sub[Games_sub$Result=="Win",]
Games_L = Games_sub[Games_sub$Result=="Lose",]

# Randomly select 50% W and 50% L
set.seed(12345)
Games_W_sub = Games_W[sample(nrow(Games_W),nrow(Games_W)/2),]
set.seed(123456)
Games_L_sub = Games_L[sample(nrow(Games_L),nrow(Games_L)/2),]

# Filter out variables that will not make the model
Games_W_sub = Games_W_sub[,c(1:4,7,37)]
Games_L_sub = Games_L_sub[,c(1:4,7,37)]

# Add team two number
names(Games_W_sub)[2] = "ID_SeasonTeam1"
names(Games_L_sub)[2] = "ID_SeasonTeam1"
names(Games_W_sub)[3] = "Team1"
names(Games_L_sub)[3] = "Team1"
names(Games_W_sub)[5] = "Location1"
names(Games_L_sub)[5] = "Location1"


Games_W_sub$Location2 = ifelse(Games_W_sub$Location1=="N","N",ifelse(Games_W_sub$Location1=="H","A","H"))
Games_L_sub$Location2 = ifelse(Games_L_sub$Location1=="N","N",ifelse(Games_L_sub$Location1=="H","A","H"))


Games_W_sub$Team2 = as.numeric(substr(Games_W_sub$id,nchar(as.character(Games_W_sub$id))-3,
                                      nchar(as.character(Games_W_sub$id))))
Games_L_sub$Team2 = as.numeric(substr(Games_L_sub$id,6,9))

Games_W_sub$ID_SeasonTeam2 = paste(Games_W_sub$Season,"_",Games_W_sub$Team2,sep="")
Games_L_sub$ID_SeasonTeam2 = paste(Games_L_sub$Season,"_",Games_L_sub$Team2,sep="")

Games_W_sub = Games_W_sub[,c(1,2,3,5,9,8,7,4,6)]
Games_L_sub = Games_L_sub[,c(1,2,3,5,9,8,7,4,6)]

# Append Games_W_sub and Games_L_sub to the pre-modeling dataset
Games_train = rbind(Games_W_sub,Games_L_sub)

# Merge in Team1 metrics
Games_train_1 = merge(Games_train,Team_metric,by.x="ID_SeasonTeam1",by.y="ID_SeasonTeam",all.x=T)
Games_train_2 = merge(Games_train_1,Team_metric,by.x="ID_SeasonTeam2",by.y="ID_SeasonTeam",all.x=T,
                      suffixes = c("_Team1","Team_2"))

# Modeling dataset
Games_allvars = Games_train_2[,c(8,3,2,4,5,1,6,7,9:ncol(Games_train_2))]
Games_allvars$Location1 = NULL
Games_allvars$Location2 = NULL

# Output
write.csv(Games_allvars,"Games_allvars.csv",row.names = F)

# Read in test dataset
Games_allvars_test = read.csv("Games_allvars_test.csv")

################ Modeling #################
# Divide into trn/test
ind = createDataPartition(Games_allvars$Result,p=0.7,list=F)
Games_allvars_train_trn = Games_allvars[ind,]
Games_allvars_train_tst = Games_allvars[-ind,]
control = trainControl(method="cv",number=5,classProbs = T)
## Decision Tree ##
system.time(mod_rpart <- train(as.factor(Result)~.-Season-id-ID_SeasonTeam1-Team1-ID_SeasonTeam2-Team2,
                               data=Games_allvars_train_trn,method="rpart",
                               trControl = control,tuneLength=20))
mod_rpart
varImp(mod_rpart)

rpartGrid = expand.grid(cp=seq(0,0.005,0.00025))
set.seed(1111) 
system.time(mod_rpart <- train(as.factor(Result)~.-Season-id-ID_SeasonTeam1-Team1-ID_SeasonTeam2-Team2,
                               data=Games_allvars_train_trn,method="rpart",
                               trControl = control,tuneGrid=rpartGrid))
mod_rpart
varImp(mod_rpart)
plot(varImp(mod_rpart))

rpart_pred = predict(mod_rpart,newdata=Games_allvars_train_tst,type="raw")
confusionMatrix(rpart_pred,Games_allvars_train_tst$Result)

# Build on entire train dataset and test on test set
# build on the whole trn dataset
Games_allvars_final = Games_allvars[,c(7:ncol(Games_allvars))]
rpartGrid_final = expand.grid(cp=0.00125)
set.seed(1111)
system.time(mod_rpart <- train(as.factor(Result)~.,
                               data=Games_allvars_final,method="rpart",
                               trControl = control,tuneGrid=rpartGrid_final))
mod_rpart
Games_allvars_test_final = Games_allvars_test[,c(6:ncol(Games_allvars_test))]
rpart_pred = predict(mod_rpart,newdata=Games_allvars_test_final,type="prob")
Pred_0208 = data.frame(id=Games_allvars_test$id,pred=rpart_pred$Win)
write.csv(Pred_0208,"Pred_0208.csv",row.names = F)









