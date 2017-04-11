#################### GLM Model with Stepwise ########################
rm(list=ls())
gc()
getwd()
# setwd()

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
                                select=-c(id,Daynum,ID_SeasonTeam1,ID_SeasonTeam2,
                                          Team1,Team2,Result_num)) # keep season

control = trainControl(method="cv",number=5,classProbs = T)

# GLM
mod_glm_full <- glm(formula=as.factor(Result)~.,
                 data=Tournament_allvars_trn,family="binomial")

mod_glm_full
summary(mod_glm_full)

# Stepwise selection
mod_glm_step = step(mod_glm_full)
summary(mod_glm_step)
mod_glm_coef = as.data.frame(mod_glm_step$coefficients)
mod_glm_coef_name = row.names(mod_glm_coef)

# GLM modeling dataset after filtering
modeling_glm = Tournament_modeling[,colnames(Tournament_modeling) %in% mod_glm_coef_name | 
                                     colnames(Tournament_modeling) %in% c("Result","Result_num","Season","id")]
write.csv(modeling_glm,"modeling_glm_0213.csv",row.names = F)

################# Correlation Analysis ####################
# With Target Variables
glm_cor = cor(subset(modeling_glm,select=-c(Result)),Tournament_modeling$Result_num)
# transform correlation matrix to three column data frame
glm_cor_summary = as.data.frame(as.table(glm_cor))
colnames(glm_cor_summary)[3] = "correlation"
# create absolute correlation
glm_cor_summary$abs_cor = abs(glm_cor_summary$correlation)
# sort by absolute correlation
glm_cor_summary = glm_cor_summary[order(glm_cor_summary$abs_cor,decreasing = T),]
glm_cor_summary$Var2 = "Result_num"
write.csv(cor_summary,"Univariate_0209.csv",row.names = F)

# Between Predictors
glm_cor_pred = cor(subset(modeling_glm,select=-c(Result)))
# transform correlation matrix to three column data frame
glm_cor_pred_summary = as.data.frame(as.table(glm_cor_pred))
colnames(glm_cor_pred_summary)[3] = "correlation"
# exclude if var1 is same as var2
glm_cor_pred_summary =glm_cor_pred_summary[glm_cor_pred_summary$Var1!=glm_cor_pred_summary$Var2,]
# create absolute correlation
glm_cor_pred_summary$abs_cor = abs(glm_cor_pred_summary$correlation)
# sort by absolute correlation
glm_cor_pred_summary = glm_cor_pred_summary[order(glm_cor_pred_summary$abs_cor,decreasing = T),]
write.csv(glm_cor_pred_summary,"Correlation between predictors_glm_0213.csv",row.names = F)

################### Principal Component Analysis ######################
### Principal Component Analysis
# PC_Winning
PC_Winning = prcomp(modeling_glm[,names(modeling_glm) %in% c(
  "Winning_percent_Team1",
  "Games_won_Team1",
  "Away_won_prob_Team1",
  "Home_won_prob_Team1",
  "Seed_num_Team1"
)],center = TRUE,scale. = TRUE)

PC_Winning
plot(PC_Winning, type = "l")
summary(PC_Winning) # First three PCs takes 96% of the variance

PC_Winning_pred = predict(PC_Winning,newdata = modeling_glm[,names(modeling_glm) %in% c(
  "Winning_percent_Team1",
  "Games_won_Team1",
  "Away_won_prob_Team1",
  "Home_won_prob_Team1",
  "Seed_num_Team1"
)])

head(PC_Winning_pred)
PC_Winning_pred = as.data.frame(PC_Winning_pred)
names(PC_Winning_pred) = paste(names(PC_Winning_pred),"_Winning",sep = "")

# PC_FG3
PC_FG3 = prcomp(modeling_glm[,names(modeling_glm) %in% c(
  "FGA3_avg_Team1",
  "FGM3_avg_Team1"
)],center = TRUE,scale. = TRUE)

PC_FG3
plot(PC_FG3, type = "l")
summary(PC_FG3) # First PC takes 96% of the variance

PC_FG3_pred = predict(PC_FG3,newdata = modeling_glm[,names(modeling_glm) %in% c(
  "FGA3_avg_Team1",
  "FGM3_avg_Team1"
)])

head(PC_FG3_pred)
PC_FG3_pred = as.data.frame(PC_FG3_pred)
names(PC_FG3_pred) = paste(names(PC_FG3_pred),"_FG3",sep = "")

# PC_OR
PC_OR = prcomp(modeling_glm[,names(modeling_glm) %in% c(
  "OR_percent_Team1",
  "OR_avg_Team1"
)],center = TRUE,scale. = TRUE)

PC_OR
plot(PC_OR, type = "l")
summary(PC_OR) # First PC takes 94% of the variance

PC_OR_pred = predict(PC_OR,newdata = modeling_glm[,names(modeling_glm) %in% c(
  "OR_percent_Team1",
  "OR_avg_Team1"
)])

head(PC_OR_pred)
PC_OR_pred = as.data.frame(PC_OR_pred)
names(PC_OR_pred) = paste(names(PC_OR_pred),"_OR",sep = "")


# PC_AST
PC_AST = prcomp(modeling_glm[,names(modeling_glm) %in% c(
  "AST_to_SCORE_Team1",
  "AST_avg_Team1"
)],center = TRUE,scale. = TRUE)

PC_AST
plot(PC_AST, type = "l")
summary(PC_AST) # First PC takes 92% of the variance

PC_AST_pred = predict(PC_AST,newdata = modeling_glm[,names(modeling_glm) %in% c(
  "AST_to_SCORE_Team1",
  "AST_avg_Team1"
)])

head(PC_AST_pred)
PC_AST_pred = as.data.frame(PC_AST_pred)
names(PC_AST_pred) = paste(names(PC_AST_pred),"_AST",sep = "")


# Create modeling dataset with PCs
modeling_glm_PC = modeling_glm[,!(names(modeling_glm) %in% c(  "Winning_percent_Team1",
                                                               "Games_won_Team1",
                                                               "Away_won_prob_Team1",
                                                               "Home_won_prob_Team1",
                                                               "Seed_num_Team1",
                                                               "FGA3_avg_Team1",
                                                               "FGM3_avg_Team1",
                                                               "OR_percent_Team1",
                                                               "OR_avg_Team1",
                                                               "AST_to_SCORE_Team1",
                                                               "AST_avg_Team1"
                                                               ))]

modeling_glm_PC = data.frame(modeling_glm_PC,
                             PC_Winning_pred$PC1_Winning,PC_Winning_pred$PC2_Winning,PC_Winning_pred$PC3_Winning,
                             PC_FG3_pred$PC1_FG3,
                             PC_OR_pred$PC1_OR,
                             PC_AST_pred$PC1_AST)
# GLM model with PCs
modeling_glm_PC_trn = modeling_glm_PC[modeling_glm_PC$Season<2013,!(names(modeling_glm_PC) %in% c("Result_num","Season"))]
modeling_glm_PC_val = modeling_glm_PC[modeling_glm_PC$Season>=2013,!(names(modeling_glm_PC) %in% c("Result_num","Season"))]

system.time(mod_glm <- train(as.factor(Result)~.,data=modeling_glm_PC_trn,method = "glm",
                             family="binomial",trControl = control))
summary(mod_glm)

#################### Test performance on holdout data ######################
# Prepare test dataset
test = read.csv("Intermediate Data/Test_Full_0209.csv")
test_glm_0213 = test[,names(test) %in% names(modeling_glm)]

PC_Winning_test_pred = predict(PC_Winning,newdata = test_glm_0213[,names(test_glm_0213) %in% c(
  "Winning_percent_Team1",
  "Games_won_Team1",
  "Away_won_prob_Team1",
  "Home_won_prob_Team1",
  "Seed_num_Team1"
)])
PC_Winning_test_pred = as.data.frame(PC_Winning_test_pred)
names(PC_Winning_test_pred) = paste(names(PC_Winning_test_pred),"_Winning",sep = "")

PC_FG3_test_pred = predict(PC_FG3,newdata = test_glm_0213[,names(test_glm_0213) %in% c(
  "FGA3_avg_Team1",
  "FGM3_avg_Team1"
)])
PC_FG3_test_pred = as.data.frame(PC_FG3_test_pred)
names(PC_FG3_test_pred) = paste(names(PC_FG3_test_pred),"_FG3",sep = "")

PC_OR_test_pred = predict(PC_OR,newdata = test_glm_0213[,names(test_glm_0213) %in% c(
  "OR_percent_Team1",
  "OR_avg_Team1"
)])
PC_OR_test_pred = as.data.frame(PC_OR_test_pred)
names(PC_OR_test_pred) = paste(names(PC_OR_test_pred),"_OR",sep = "")

PC_AST_test_pred = predict(PC_AST,newdata = test_glm_0213[,names(test_glm_0213) %in% c(
  "AST_to_SCORE_Team1",
  "AST_avg_Team1"
)])
PC_AST_test_pred = as.data.frame(PC_AST_test_pred)
names(PC_AST_test_pred) = paste(names(PC_AST_test_pred),"_AST",sep = "")

test_glm_0213 = data.frame(test_glm_0213,PC_Winning_test_pred$PC1_Winning,PC_Winning_test_pred$PC2_Winning,PC_Winning_test_pred$PC3_Winning,
                           PC_FG3_test_pred$PC1_FG3,
                           PC_OR_test_pred$PC1_OR,PC_AST_test_pred$PC2_AST)
# Change the colnames
colnames(test_glm_0213)[ncol(test_glm_0213)] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)]
colnames(test_glm_0213)[ncol(test_glm_0213)-1] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)-1]
colnames(test_glm_0213)[ncol(test_glm_0213)-2] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)-2]
colnames(test_glm_0213)[ncol(test_glm_0213)-3] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)-3]
colnames(test_glm_0213)[ncol(test_glm_0213)-4] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)-4]
colnames(test_glm_0213)[ncol(test_glm_0213)-5] = colnames(modeling_glm_PC)[ncol(modeling_glm_PC)-5]

test_glm_0213 = test_glm_0213[,names(test_glm_0213) %in% names(modeling_glm_PC_trn)]

# Make prediction
glm_pc_pred = predict(mod_glm,newdata=test_glm_0213,type="prob")
Pred_Tournament_Full_0213 = data.frame(id=test$id,pred=glm_pc_pred$Win)
write.csv(Pred_Tournament_Full_0213,"Pred_Tournament_GLM_PC_0213.csv",row.names = F)

# ROC
glm_pc_roc_pred = predict(mod_glm,newdata=modeling_glm_PC_val,type="prob")
Tournament_pred = prediction(glm_pc_roc_pred$Win,modeling_glm_PC_val$Result)
Tournament_roc = performance(Tournament_pred,measure="tpr",x.measure="fpr")
plot(Tournament_roc,main="ROC Curve",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Tournament_auc = performance(Tournament_pred,measure="auc")
unlist(Tournament_auc@y.values)

