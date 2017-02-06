############################### Titanic Project #####################################
### Using Random Forest to impute the missing values

rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Titanic_Machine Learning From Disaster/")

# Read in necessary packages
library(caret)
library(ROCR)
library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(reshape2)
library(dplyr)
library(Amelia)
library(mice)
library(randomForest)

############# Load training and testing datasets and combine the two datasets #################
# Load in datasets, use na.strings to convert blank as missing
training = read.csv("train.csv",na.strings=c("","NA"))
dim(training)
testing = read.csv("test.csv",na.strings=c("","NA"))
dim(testing)

# Impute a placeholder for "Survived" in testing dataset
testing$Survived = NA
testing = testing[,c(1,12,2:11)]

# Assign an indicator variable for TRN/TST/VAL
set.seed(1)
ind = createDataPartition(training$Survived,p=0.6,list=F)
training_trn = training[ind,]
training_trn$SPLIT = "TRN"
training_tst = training[-ind,]
training_tst$SPLIT = "TST"
testing$SPLIT = "VAL"

# Combine the two datasets for data cleansing and manipulation purpose
all = rbind(training_trn,training_tst,testing)
table(all$SPLIT)

# Extract title information
all$Title = gsub('(.*, )|(\\..*)', '', all$Name)
table(all$Title)
table(all$Title,all$Sex)
unique(all$Title)
rare_Title = c("Don","Rev","Dr","Mme","Major","Mlle","Col","Jonkheer","Ms",
               "Lady","Sir","Capt","the Countess","Dona")
all[all$Title %in% rare_Title,]$Title = "Rare_Title"
table(all$Title)

# Extract cabin_num
all$cabin_num = substr(all$Cabin,1,1)
table(all$cabin_num)

############# Impute Missing Values ###############
missmap(all,col=c("red","lightgrey"))

# Impute missing Age using Random Forest
estimateMissingVariables <- function(data) {
  predictors <- c("Age", "Sex", "Fare", "Pclass", "SibSp", "Parch", "Embarked", "Title")
  set.seed(2)
  capture.output(model <- mice(data[, names(data) %in% predictors], method='rf'))
  output <- complete(model)
  data$Age <- output$Age
  data$Fare <- output$Fare
  return(data)
}

fixedAll <- estimateMissingVariables(all)

missmap(fixedAll,col=c("red","lightgrey"))
colSums(is.na(fixedAll))
all = fixedAll

all = as.data.table(all)

# Cabin has 1014 missing values (77%), impute a new category to indicate missing value category
all[is.na(all$Cabin)]$Cabin = 'Z'
all[is.na(all$cabin_num)]$cabin_num = 'Z'

# Impute the missing Embark using Random Forest
estimateMissingEmbarked <- function(data) {
  missing <- data[is.na(data$Embarked), ]
  present <- data[!is.na(data$Embarked), ]
  
  model <- randomForest(as.factor(Embarked) ~ Sex + Age + Fare + Pclass + SibSp + Parch, data=present)
  missing$Embarked <-predict(model, missing, type="response")
  all <- rbind.fill(missing, present)
  all <- all[with(all, order(PassengerId)), ]
  return(all)
}

all = estimateMissingEmbarked(all)
colSums(is.na(all))

################# Create synthetic variables ###################
all$fam_size = all$SibSp + all$Parch + 1

################# Transform factor variables into numeric indicators #####################
transformDummy <- function(data) {
  data$cabin_num = as.factor(all$cabin_num)
  dummies = dummyVars(Survived~.-Name-Ticket-Cabin-SPLIT,data=data)
  all_dummy = predict(dummies,newdata=data)
  all_dummy = as.data.frame(all_dummy)
  all_sub = subset(data,select=c(PassengerId,Survived,SPLIT))
  allvars = merge(all_sub,all_dummy,by="PassengerId",all=T)
}

allvars = transformDummy(all)

################# Correlation report #####################
allvars_trn = allvars[allvars$SPLIT=="TRN"|allvars$SPLIT=="TST",]
allvars_val = allvars[allvars$SPLIT == "VAL",]

correlationAnalysis = function(data) {
  allvars_cor = cor(subset(data,select=-c(PassengerId,SPLIT,Survived)),data$Survived)
  # transform correlation matrix to three column data frame
  cor_summary = as.data.frame(as.table(allvars_cor))
  colnames(cor_summary)[3] = "correlation"
  # create absolute correlation
  cor_summary$abs_cor = abs(cor_summary$correlation)
  # sort by absolute correlation
  cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]
}

cor_summary = correlationAnalysis(allvars_trn)


# Univariate analysis has been done by Titanic Survival_Phase 2.R
# Age
allvars_trn = mutate(allvars_trn,age_decile = ntile(Age,5))
mosaicplot(~ age_decile + Survived, data = allvars_trn, color = TRUE)

################ Pre-modeling ###################
allvars_trn$age_decile = NULL
allvars_trn$Sex.male = NULL
allvars_val$Sex.male = NULL

allvars_trn$SPLIT = NULL
allvars_val$SPLIT = NULL
allvars_trn$Survived = ifelse(allvars_trn$Survived==0,"Dead","Survived")

trn_ind = createDataPartition(allvars_trn$Survived,p=0.6,list=F)
allvars_trn_trn = allvars_trn[trn_ind,]
allvars_trn_val = allvars_trn[-trn_ind,]

########################### Modeling ##################################
control = trainControl(method="cv",number=5,classProbs = T)

# Random Forest
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId,
                            data=allvars_trn_trn,method="rf",
                            trControl = control))
mod_rf

rfGrid = expand.grid(mtry=seq(2,10,1))
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=allvars_trn_val,type="raw")
confusionMatrix(rf_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rfGrid_final = expand.grid(mtry=4)
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId,
                            data=allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf

rf_pred_final = predict(mod_rf,newdata=allvars_val,type="raw")
rf_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rf_pred_final)
rf_pred_submit = rf_pred_submit[order(rf_pred_submit$PassengerId),]
rf_pred_submit$Survived = ifelse(rf_pred_submit$Survived=="Dead",0,1)
prop.table(table(rf_pred_submit$Survived))
write.csv(rf_pred_submit,"pred_013001.csv",row.names = F)

# Random Forest (remove few variables)
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneLength=5))
mod_rf

rfGrid = expand.grid(mtry=seq(2,8,1))
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=allvars_trn_val,type="raw")
confusionMatrix(rf_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rfGrid_final = expand.grid(mtry=c(2,3,4))
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q,
                            data=allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
rf_pred_final = predict(mod_rf,newdata=allvars_val,type="raw")
rf_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rf_pred_final)
rf_pred_submit = rf_pred_submit[order(rf_pred_submit$PassengerId),]
rf_pred_submit$Survived = ifelse(rf_pred_submit$Survived=="Dead",0,1)
prop.table(table(rf_pred_submit$Survived))
write.csv(rf_pred_submit,"pred_rf_013002.csv",row.names = F)


# SVM
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control))
mod_svmRadial

svmRadialGrid = expand.grid(C=c(0.75,0.9,1,1.1,1.25),
                            sigma=c(0.05,0.1,0.12,0.15,0.2))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid))
mod_svmRadial

svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=c(0.75,1),
                                  sigma=c(0.1,0.15))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q,
                                   data=allvars_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid_final))
mod_svmRadial
svmRadial_pred_final = predict(mod_svmRadial,newdata=allvars_val,type="raw")
svmRadial_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = svmRadial_pred_final)
svmRadial_pred_submit = svmRadial_pred_submit[order(svmRadial_pred_submit$PassengerId),]
svmRadial_pred_submit$Survived = ifelse(svmRadial_pred_submit$Survived=="Dead",0,1)
prop.table(table(svmRadial_pred_submit$Survived))
write.csv(svmRadial_pred_submit,"pred_svmRadial_013003.csv",row.names = F)


################### Interaction Variable ######################
# create 4 interaction variables: age*sex, age*fare, pclass*sex, pclass*age
allvars_trn_trn$intr_age_sex = allvars_trn_trn$Age*allvars_trn_trn$Sex.female
allvars_trn_trn$intr_age_fare = allvars_trn_trn$Age*allvars_trn_trn$Fare
allvars_trn_trn$intr_age_pclass = allvars_trn_trn$Age*allvars_trn_trn$Pclass
allvars_trn_trn$intr_sex_pclass = allvars_trn_trn$Sex.female*allvars_trn_trn$Pclass

allvars_trn_val$intr_age_sex = allvars_trn_val$Age*allvars_trn_val$Sex.female
allvars_trn_val$intr_age_fare = allvars_trn_val$Age*allvars_trn_val$Fare
allvars_trn_val$intr_age_pclass = allvars_trn_val$Age*allvars_trn_val$Pclass
allvars_trn_val$intr_sex_pclass = allvars_trn_val$Sex.female*allvars_trn_val$Pclass

allvars_val$intr_age_sex = allvars_val$Age*allvars_val$Sex.female
allvars_val$intr_age_fare = allvars_val$Age*allvars_val$Fare
allvars_val$intr_age_pclass = allvars_val$Age*allvars_val$Pclass
allvars_val$intr_sex_pclass = allvars_val$Sex.female*allvars_val$Pclass

allvars_trn$intr_age_sex = allvars_trn$Age*allvars_trn$Sex.female
allvars_trn$intr_age_fare = allvars_trn$Age*allvars_trn$Fare
allvars_trn$intr_age_pclass = allvars_trn$Age*allvars_trn$Pclass
allvars_trn$intr_sex_pclass = allvars_trn$Sex.female*allvars_trn$Pclass

################### Decision Tree Variable ######################
allvars_trn_trn$tree_var1 = ifelse(allvars_trn_trn$TitleMr>=0.5 & allvars_trn_trn$Fare<26,0,1)
allvars_trn_trn$tree_var2 = ifelse(allvars_trn_trn$TitleMr<0.5 & allvars_trn_trn$fam_size<4.5,1,0)

allvars_trn_val$tree_var1 = ifelse(allvars_trn_val$TitleMr>=0.5 & allvars_trn_val$Fare<26,0,1)
allvars_trn_val$tree_var2 = ifelse(allvars_trn_val$TitleMr<0.5 & allvars_trn_val$fam_size<4.5,1,0)

allvars_val$tree_var1 = ifelse(allvars_val$TitleMr>=0.5 & allvars_val$Fare<26,0,1)
allvars_val$tree_var2 = ifelse(allvars_val$TitleMr<0.5 & allvars_val$fam_size<4.5,1,0)

allvars_trn$tree_var1 = ifelse(allvars_trn$TitleMr>=0.5 & allvars_trn$Fare<26,0,1)
allvars_trn$tree_var2 = ifelse(allvars_trn$TitleMr<0.5 & allvars_trn$fam_size<4.5,1,0)

# Random Forest
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                            -intr_sex_pclass,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneLength=5))
mod_rf

rfGrid = expand.grid(mtry=seq(2,8,1))
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=allvars_trn_val,type="raw")
confusionMatrix(rf_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rfGrid_final = expand.grid(mtry=c(2,3))
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q,
                            data=allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
rf_pred_final = predict(mod_rf,newdata=allvars_val,type="raw")
rf_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rf_pred_final)
rf_pred_submit = rf_pred_submit[order(rf_pred_submit$PassengerId),]
rf_pred_submit$Survived = ifelse(rf_pred_submit$Survived=="Dead",0,1)
prop.table(table(rf_pred_submit$Survived))
write.csv(rf_pred_submit,"pred_rf_013004.csv",row.names = F)


# Random Forest (variable selection)
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                            -intr_sex_pclass-tree_var1-tree_var2-Parch-TitleMaster-Embarked.C-Embarked.S
                            -TitleMiss - TitleMrs - SibSp - cabin_num.Z - fam_size - Pclass - Sex.female,
                            data=allvars_trn_trn,method="rf"))
mod_rf

system.time(mod_rf <- train(as.factor(Survived)~ intr_age_sex + intr_age_fare + intr_age_pclass
                            + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                            + Embarked.C + Embarked.S + cabin_num.Z,
                            data=allvars_trn_trn,method="rf"))
mod_rf

rfGrid = expand.grid(mtry=seq(2,8,1))
set.seed(123456) # System time: 
system.time(mod_rf <- train(as.factor(Survived)~intr_age_sex + intr_age_fare + intr_age_pclass
                            + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                            + Embarked.C + Embarked.S + cabin_num.Z,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=allvars_trn_val,type="raw")
confusionMatrix(rf_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rfGrid_final = expand.grid(mtry=c(2,3))
system.time(mod_rf <- train(as.factor(Survived)~intr_age_sex + intr_age_fare + intr_age_pclass
                             + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                             + Embarked.C + Embarked.S + cabin_num.Z,
                            data=allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
rf_pred_final = predict(mod_rf,newdata=allvars_val,type="raw")
rf_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rf_pred_final)
rf_pred_submit = rf_pred_submit[order(rf_pred_submit$PassengerId),]
rf_pred_submit$Survived = ifelse(rf_pred_submit$Survived=="Dead",0,1)
prop.table(table(rf_pred_submit$Survived))
write.csv(rf_pred_submit,"pred_rf_013005.csv",row.names = F)

# SVM
system.time(mod_svmRadial <- train(as.factor(Survived)~intr_age_sex + intr_age_fare + intr_age_pclass
                                   + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                                   + Embarked.C + Embarked.S + cabin_num.Z,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control))
mod_svmRadial

svmRadialGrid = expand.grid(C=c(0.25,0.5,0.75,0.9,1,1.25),
                            sigma=c(0.05,0.1,0.12,0.15,0.2))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~intr_age_sex + intr_age_fare + intr_age_pclass
                                   + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                                   + Embarked.C + Embarked.S + cabin_num.Z,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid))
mod_svmRadial

svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=c(0.9,1.25),
                                  sigma=c(0.1,0.12,0.15))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~intr_age_sex + intr_age_fare + intr_age_pclass
                                   + intr_sex_pclass + tree_var1 + tree_var2 + fam_size + Pclass + Sex.female
                                   + Embarked.C + Embarked.S + cabin_num.Z,
                                   data=allvars_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid_final))
mod_svmRadial
svmRadial_pred_final = predict(mod_svmRadial,newdata=allvars_val,type="raw")
svmRadial_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = svmRadial_pred_final)
svmRadial_pred_submit = svmRadial_pred_submit[order(svmRadial_pred_submit$PassengerId),]
svmRadial_pred_submit$Survived = ifelse(svmRadial_pred_submit$Survived=="Dead",0,1)
prop.table(table(svmRadial_pred_submit$Survived))
write.csv(svmRadial_pred_submit,"pred_svmRadial_013006.csv",row.names = F)

# SVM (best model for now)
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q - intr_age_sex - intr_age_fare - intr_age_pclass
                                   - intr_sex_pclass - tree_var1 - tree_var2,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control))
mod_svmRadial

svmRadialGrid = expand.grid(C=c(0.75,0.9,1,1.1,1.25),
                            sigma=c(0.05,0.1,0.12,0.15,0.2))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q - intr_age_sex - intr_age_fare - intr_age_pclass
                                   - intr_sex_pclass - tree_var1 - tree_var2,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid))
mod_svmRadial
varImp(mod_svmRadial)
plot(varImp(mod_svmRadial))

svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=c(0.75,1),
                                  sigma=c(0.12,0.15))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q - intr_age_sex - intr_age_fare - intr_age_pclass
                                   - intr_sex_pclass - tree_var1 - tree_var2,
                                   data=allvars_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid_final))
mod_svmRadial
svmRadial_pred_final = predict(mod_svmRadial,newdata=allvars_val,type="raw")
svmRadial_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = svmRadial_pred_final)
svmRadial_pred_submit = svmRadial_pred_submit[order(svmRadial_pred_submit$PassengerId),]
svmRadial_pred_submit$Survived = ifelse(svmRadial_pred_submit$Survived=="Dead",0,1)
prop.table(table(svmRadial_pred_submit$Survived))
write.csv(svmRadial_pred_submit,"pred_svmRadial_013007.csv",row.names = F)

