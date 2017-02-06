########### Kaggle Competition ###########
#### Titanic Survival Project ####
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


################ Check the data quality ####################
str(all)

# Check the distribution
# Survived
table(all$Survived)
prop.table(table(all$Survived))

# Pclass
table(all$Pclass)
prop.table(table(all$Pclass))

# Name
# Grab title from passenger names
all$Title = gsub('(.*, )|(\\..*)', '', all$Name)
table(all$Title)
table(all$Title,all$Sex)
unique(all$Title)
rare_Title = c("Don","Rev","Dr","Mme","Major","Mlle","Col","Jonkheer","Ms",
               "Lady","Sir","Capt","the Countess","Dona")
all[all$Title %in% rare_Title,]$Title = "Rare_Title"
table(all$Title)

# Sex
table(all$Sex)
prop.table(table(all$Sex))

# Age
hist(all$Age, freq=F, main='Age',col='darkgreen', ylim=c(0,0.04))
quantile(all$Age,na.rm = T,probs = (seq(0,1,by=0.1)))

# SibSp
table(all$SibSp)
prop.table(table(all$SibSp))

# Parch
table(all$Parch)
prop.table(table(all$Parch))

# Fare
hist(all$Fare, freq=F, main='Age',col='darkgreen')
quantile(all$Fare,na.rm = T,probs = (seq(0,1,by=0.1)))

# Embarked
table(all$Embarked)
prop.table(table(all$Embarked))

# Cabin
all$cabin_num = substr(all$Cabin,1,1)
table(all$cabin_num)

##################### Impute the missing value ########################
# create missing value summary
na_summary = data.frame(num_na = colSums(is.na(all)))
na_summary = data.frame(Variable = rownames(na_summary),na_summary,
                        na_ratio = na_summary$num_na/nrow(all))

# Survived has 418 missing values, it is the target variable needs to be predicted
# Age has 263 missing values, impute the missing by median controlling pclass and sex
all = as.data.table(all)
age_summary = all[,.(Age = median(Age,na.rm=T)),by=c("Sex","Pclass")]
age_summary$Age_abs = round(age_summary$Age,0)

all$Age[all$Sex == "male" & all$Pclass == 1 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "male"
                                        & age_summary$Pclass == 1,Age_abs]

all$Age[all$Sex == "male" & all$Pclass == 2 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "male"
                                        & age_summary$Pclass == 2,Age_abs]

all$Age[all$Sex == "male" & all$Pclass == 3 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "male"
                                        & age_summary$Pclass == 3,Age_abs]

all$Age[all$Sex == "female" & all$Pclass == 1 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "female"
                                        & age_summary$Pclass == 1,Age_abs]

all$Age[all$Sex == "female" & all$Pclass == 2 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "female"
                                        & age_summary$Pclass == 2,Age_abs]

all$Age[all$Sex == "female" & all$Pclass == 3 
        & is.na(all$Age)] = age_summary[age_summary$Sex == "female"
                                        & age_summary$Pclass == 3,Age_abs]

# Fare has 1 missing value, impute the missing by median controlling pclass and sex
all[is.na(all$Fare),]
all[,.(mean(Fare,na.rm = T)),by=c("Sex","Pclass")]
all$Fare[all$Sex == "male" & all$Pclass == 3 
         & is.na(all$Fare)] = 12.42

# Cabin has 1014 missing values (77%), impute a new category to indicate missing value category
all[is.na(all$Cabin)]$Cabin = 'Z'

# Embark has 2 missing values, impute with mode controlling pclass and sex
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

all[,.(Embarked = Mode(Embarked)),by=c("Sex","Pclass")]

all[is.na(all$Embarked)]$Embarked = 'C'

# cabin_num, same as Cabin
all[is.na(all$cabin_num)]$cabin_num = 'Z'

# check if there is still missing value 
colSums(is.na(all))


################# Create synthetic variables ###################
all$fam_size = all$SibSp + all$Parch + 1


################# Transform factor variables into numeric indicators #####################
all$cabin_num = as.factor(all$cabin_num)
dummies = dummyVars(Survived~.-Name-Ticket-Cabin-SPLIT,data=all)
all_dummy = predict(dummies,newdata=all)
all_dummy = as.data.frame(all_dummy)

# merge back Passenger Id and SPLIT
all_sub = subset(all,select=c(PassengerId,Survived,SPLIT))
allvars = merge(all_sub,all_dummy,by="PassengerId",all=T)

##################### Correlation between target variable and predictors #########################
allvars_trn = allvars[allvars$SPLIT=="TRN"|allvars$SPLIT=="TST",]
allvars_val = allvars[allvars$SPLIT == "VAL",]
allvars_cor = cor(subset(allvars_trn,select=-c(PassengerId,SPLIT,Survived)),allvars_trn$Survived)
# transform correlation matrix to three column data frame
cor_summary = as.data.frame(as.table(allvars_cor))
colnames(cor_summary)[3] = "correlation"
# create absolute correlation
cor_summary$abs_cor = abs(cor_summary$correlation)
# sort by absolute correlation
cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]


#################### Univariate Analysis - Visualize the correlation using rainbow chart #######################
# Pclass
mosaicplot(~ Pclass + Survived, data = allvars_trn, color = TRUE)
# Sex.female
mosaicplot(~ Sex.female + Survived, data = allvars_trn, color = TRUE)
# Sex.male
mosaicplot(~ Sex.male + Survived, data = allvars_trn, color = TRUE)
# Age (may work with interaction variables)
allvars_trn = mutate(allvars_trn,age_decile = ntile(Age,10))
mosaicplot(~ age_decile + Survived, data = allvars_trn, color = TRUE)
# SibSp
mosaicplot(~ SibSp + Survived, data = allvars_trn, color = TRUE)
# Parch
mosaicplot(~ Parch + Survived, data = allvars_trn, color = TRUE)
# fam_size
mosaicplot(~ fam_size + Survived, data = allvars_trn, color = TRUE)
# Fare (may work with interaction variables)
allvars_trn = mutate(allvars_trn,fare_decile = ntile(Fare,10))
mosaicplot(~ fare_decile + Survived, data = allvars_trn, color = TRUE)
# Embarked.C
mosaicplot(~ Embarked.C + Survived, data = allvars_trn, color = TRUE)
# Embarked.S
mosaicplot(~ Embarked.S + Survived, data = allvars_trn, color = TRUE)
# Embarked.Q
mosaicplot(~ Embarked.Q + Survived, data = allvars_trn, color = TRUE)
# TitleMaster
mosaicplot(~ TitleMaster + Survived, data = allvars_trn, color = TRUE)
# TitleMiss
mosaicplot(~ TitleMiss + Survived, data = allvars_trn, color = TRUE)
# TitleMrs
mosaicplot(~ TitleMrs + Survived, data = allvars_trn, color = TRUE)
# TitleMr
mosaicplot(~ TitleMr + Survived, data = allvars_trn, color = TRUE)
# TitleRare_Title
mosaicplot(~ TitleRare_Title + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_A
mosaicplot(~ cabin_num.A + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_B
mosaicplot(~ cabin_num.B + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_C
mosaicplot(~ cabin_num.C + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_D
mosaicplot(~ cabin_num.D + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_E
mosaicplot(~ cabin_num.E + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_F
mosaicplot(~ cabin_num.F + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_G
mosaicplot(~ cabin_num.G + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_T
mosaicplot(~ cabin_num.T + Survived, data = allvars_trn, color = TRUE)
# Cabin_num_Z
mosaicplot(~ cabin_num.Z + Survived, data = allvars_trn, color = TRUE)

########################### Pre-modeling Process ##########################
str(allvars_trn)
str(allvars_val)
allvars_trn$fare_decile = NULL
# Keep one sex indicator is enough
allvars_trn$Sex.male = NULL
allvars_val$Sex.male = NULL

## Check the correlation between predictors
cor = cor(subset(allvars_trn,select=c(4:ncol(allvars_trn))))
# transform correlation matrix to three column data frame
cor_summary_pred = as.data.frame(as.table(cor))
colnames(cor_summary_pred)[3] = "correlation"
# exclude if var1 is same as var2
cor_summary_pred = cor_summary_pred[cor_summary_pred$Var1!=cor_summary_pred$Var2,]
# create absolute correlation
cor_summary_pred$abs_cor = abs(cor_summary_pred$correlation)
# sort by absolute correlation
cor_summary_pred = cor_summary_pred[order(cor_summary_pred$abs_cor,decreasing = T),]

allvars_trn$SPLIT = NULL
allvars_val$SPLIT = NULL
allvars_trn$Survived = ifelse(allvars_trn$Survived==0,"Dead","Survived")

trn_ind = createDataPartition(allvars_trn$Survived,p=0.6,list=F)
allvars_trn_trn = allvars_trn[trn_ind,]
allvars_trn_val = allvars_trn[-trn_ind,]

########################### Modeling ##################################
control = trainControl(method="cv",number=5,classProbs = T)
## Decision Tree ##
system.time(mod_rpart <- train(as.factor(Survived)~.-PassengerId,
                               data=allvars_trn_trn,method="rpart",
                               trControl = control,tuneLength=20))
mod_rpart

rpartGrid = expand.grid(cp=seq(0,0.1,0.005))
set.seed(12345) # System time: 
system.time(mod_rpart <- train(as.factor(Survived)~.-PassengerId,
                               data=allvars_trn_trn,method="rpart",
                               trControl = control,tuneGrid=rpartGrid))
mod_rpart
varImp(mod_rpart)
plot(varImp(mod_rpart))

rpart_pred = predict(mod_rpart,newdata=allvars_trn_val,type="raw")
confusionMatrix(rpart_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rpartGrid_final = expand.grid(cp=0.02)
set.seed(12345) # System time: 
system.time(mod_rpart <- train(as.factor(Survived)~.-PassengerId,
                               data=allvars_trn,method="rpart",
                               trControl = control,tuneGrid=rpartGrid_final))
mod_rpart
rpart_pred_final = predict(mod_rpart,newdata=allvars_val,type="raw")
rpart_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rpart_pred_final)
rpart_pred_submit = rpart_pred_submit[order(rpart_pred_submit$PassengerId),]
rpart_pred_submit$Survived = ifelse(rpart_pred_submit$Survived=="Dead",0,1)
prop.table(table(rpart_pred_submit$Survived))
write.csv(rpart_pred_submit,"pred_012601.csv",row.names = F)


#################### Random Forest #####################
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId,
                            data=allvars_trn_trn,method="rf",
                            trControl = control))
mod_rf

rfGrid = expand.grid(mtry=seq(2,20,1))
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
rfGrid_final = expand.grid(mtry=3)
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
write.csv(rf_pred_submit,"pred_012602.csv",row.names = F)


#################### GBM #####################
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title,
                             data=allvars_trn_trn,method="gbm",
                             trControl = control,tuneLength=5))
mod_gbm

gbmGrid = expand.grid(interaction.depth=c(1,2,3),
                      n.trees=c(100,200),
                      shrinkage=c(0.05,0.1),
                      n.minobsinnode=c(5,10))
                      
set.seed(1234567) # System time: 
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title,
                             data=allvars_trn_trn,method="gbm",
                             trControl = control,tuneGrid=gbmGrid))
mod_gbm
varImp(mod_gbm)
plot(varImp(mod_gbm))

gbm_pred = predict(mod_gbm,newdata=allvars_trn_val,type="raw")
confusionMatrix(gbm_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
gbmGrid_final = expand.grid(n.trees=200,
                            interaction.depth=2,
                            shrinkage=0.1,
                            n.minobsinnode=10)
set.seed(123456) # System time: 
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title,
                             data=allvars_trn,method="gbm",
                             trControl = control,tuneGrid=gbmGrid_final))
mod_gbm
gbm_pred_final = predict(mod_gbm,newdata=allvars_val,type="raw")
gbm_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = gbm_pred_final)
gbm_pred_submit = gbm_pred_submit[order(gbm_pred_submit$PassengerId),]
gbm_pred_submit$Survived = ifelse(gbm_pred_submit$Survived=="Dead",0,1)
prop.table(table(gbm_pred_submit$Survived))
write.csv(gbm_pred_submit,"pred_gbm_012701.csv",row.names = F)

########## Jan 27th ##############
# Random Forest
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
write.csv(rf_pred_submit,"pred_rf_012702.csv",row.names = F)

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
varImp(mod_svmRadial)
plot(varImp(mod_svmRadial))

svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=0.75,
                                  sigma=0.15)
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
write.csv(svmRadial_pred_submit,"pred_svmRadial_012703.csv",row.names = F)

# NNET
system.time(mod_nnet <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                              -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                              -cabin_num.C-Embarked.Q,
                              data=allvars_trn_trn,method="nnet",
                              trControl = control))
mod_nnet

nnetGrid = expand.grid(size=c(2,3,5,10),
                       decay=c(0.01,0.05,0.1,0.12,0.15,0.2,0.5))
set.seed(123456) # System time: 
system.time(mod_nnet <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                              -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                              -cabin_num.C-Embarked.Q,
                              data=allvars_trn_trn,method="nnet",
                              trControl = control,tuneGrid=nnetGrid))
mod_nnet
varImp(mod_nnet)
plot(varImp(mod_nnet))

nnet_pred = predict(mod_nnet,newdata=allvars_trn_val,type="raw")
confusionMatrix(nnet_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
nnetGrid_final = expand.grid(size=2,
                             decay=0.01)
set.seed(123456) # System time: 
system.time(mod_nnet <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                              -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                              -cabin_num.C-Embarked.Q,
                              data=allvars_trn,method="nnet",
                              trControl = control,tuneGrid=nnetGrid_final))
mod_nnet
nnet_pred_final = predict(mod_nnet,newdata=allvars_val,type="raw")
nnet_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = nnet_pred_final)
nnet_pred_submit = nnet_pred_submit[order(nnet_pred_submit$PassengerId),]
nnet_pred_submit$Survived = ifelse(nnet_pred_submit$Survived=="Dead",0,1)
prop.table(table(nnet_pred_submit$Survived))
write.csv(nnet_pred_submit,"pred_nnet_012704.csv",row.names = F)



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

# Random Forest
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
rfGrid_final = expand.grid(mtry=c(2,3,4,5))
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
write.csv(rf_pred_submit,"pred_rf_012901.csv",row.names = F)

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
svmRadialGrid_final = expand.grid(C=0.9,
                                  sigma=0.1)
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
write.csv(svmRadial_pred_submit,"pred_svmRadial_012902.csv",row.names = F)


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
                            -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                            -intr_sex_pclass,
                            data=allvars_trn_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid))
mod_rf
varImp(mod_rf)
plot(varImp(mod_rf))

rf_pred = predict(mod_rf,newdata=allvars_trn_val,type="raw")
confusionMatrix(rf_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
rfGrid_final = expand.grid(mtry=3)
system.time(mod_rf <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                            -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                            -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                            -intr_sex_pclass,
                            data=allvars_trn,method="rf",
                            trControl = control,tuneGrid=rfGrid_final))
mod_rf
rf_pred_final = predict(mod_rf,newdata=allvars_val,type="raw")
rf_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = rf_pred_final)
rf_pred_submit = rf_pred_submit[order(rf_pred_submit$PassengerId),]
rf_pred_submit$Survived = ifelse(rf_pred_submit$Survived=="Dead",0,1)
prop.table(table(rf_pred_submit$Survived))
write.csv(rf_pred_submit,"pred_rf_012903.csv",row.names = F)


# SVM
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                                   -intr_sex_pclass,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control))
mod_svmRadial

svmRadialGrid = expand.grid(C=c(0.75,0.9,1,1.1,1.25),
                            sigma=c(0.05,0.1,0.12,0.15,0.2))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                                   -intr_sex_pclass-tree_var2,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid))
mod_svmRadial

svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=c(0.75,0.9),
                                  sigma=c(0.1,0.12,0.15))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q-intr_age_sex-intr_age_fare-intr_age_pclass
                                   -intr_sex_pclass,
                                   data=allvars_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid_final))
mod_svmRadial
svmRadial_pred_final = predict(mod_svmRadial,newdata=allvars_val,type="raw")
svmRadial_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = svmRadial_pred_final)
svmRadial_pred_submit = svmRadial_pred_submit[order(svmRadial_pred_submit$PassengerId),]
svmRadial_pred_submit$Survived = ifelse(svmRadial_pred_submit$Survived=="Dead",0,1)
prop.table(table(svmRadial_pred_submit$Survived))
write.csv(svmRadial_pred_submit,"pred_svmRadial_012904.csv",row.names = F)


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
svmRadialGrid_final = expand.grid(C=c(0.75,0.9),
                                  sigma=c(0.1,0.12,0.15))
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
write.csv(svmRadial_pred_submit,"pred_svmRadial_012905.csv",row.names = F)


# GBM
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                             -cabin_num.C-Embarked.Q,
                             data=allvars_trn_trn,method="gbm",
                             trControl = control,tuneLength=5))
mod_gbm

gbmGrid = expand.grid(interaction.depth=c(1,2,3),
                      n.trees=c(100,200),
                      shrinkage=c(0.05,0.1),
                      n.minobsinnode=c(5,10))

set.seed(1234567) # System time: 
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                             -cabin_num.C-Embarked.Q,
                             data=allvars_trn_trn,method="gbm",
                             trControl = control,tuneGrid=gbmGrid))
mod_gbm
varImp(mod_gbm)
plot(varImp(mod_gbm))

gbm_pred = predict(mod_gbm,newdata=allvars_trn_val,type="raw")
confusionMatrix(gbm_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
gbmGrid_final = expand.grid(n.trees=100,
                            interaction.depth=1,
                            shrinkage=0.05,
                            n.minobsinnode=5)
set.seed(123456) # System time: 
system.time(mod_gbm <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                             -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                             -cabin_num.C-Embarked.Q,
                             data=allvars_trn,method="gbm",
                             trControl = control,tuneGrid=gbmGrid_final))
mod_gbm
gbm_pred_final = predict(mod_gbm,newdata=allvars_val,type="raw")
gbm_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = gbm_pred_final)
gbm_pred_submit = gbm_pred_submit[order(gbm_pred_submit$PassengerId),]
gbm_pred_submit$Survived = ifelse(gbm_pred_submit$Survived=="Dead",0,1)
prop.table(table(gbm_pred_submit$Survived))
write.csv(gbm_pred_submit,"pred_gbm_012906.csv",row.names = F)









