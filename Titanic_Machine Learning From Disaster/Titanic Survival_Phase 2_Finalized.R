########### Finalized Code for Titanic Survival Project ##########
# Set-up and Preparation
rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Titanic_Machine Learning From Disaster/")

# Read in necessary packages
library(caret) # model building
library(ROCR) # model evaluation - ROC
library(data.table) # data table format for manipulation
library(ggplot2) # visualization
library(plyr) # data manipulation
library(dplyr) # data manipulation
library(kernlab) # SVM

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


############ Data Manipulation ##############
# Extract title from Name
all$Title = gsub('(.*, )|(\\..*)', '', all$Name)
rare_Title = c("Don","Rev","Dr","Mme","Major","Mlle","Col","Jonkheer","Ms",
               "Lady","Sir","Capt","the Countess","Dona")
all[all$Title %in% rare_Title,]$Title = "Rare_Title"
table(all$Title)

###### Impute the missing values
# Create missing value summary table
naSummary = function(data){
  na_summary = data.frame(num_na = colSums(is.na(data)))
  na_summary = data.frame(Variable = rownames(na_summary),na_summary,
                          na_ratio = na_summary$num_na/nrow(data))
}

na_summary = naSummary(all)

# Age has 263 missing values, impute the missing Age by median controlling pclass and sex
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
all$Fare[is.na(all$Fare)] = all[Sex=="male" & Pclass==3,.(Fare = mean(Fare,na.rm = T)),by=c("Sex","Pclass")]$Fare

# Cabin has 1014 missing values (77%), impute a new category to indicate missing value category
all[is.na(all$Cabin)]$Cabin = 'Z'
# cabin_num, first letter of Cabin
all$cabin_num = substr(all$Cabin,1,1)
all$cabin_num = as.factor(all$cabin_num)

# Embark has 2 missing values, impute with mode controlling pclass and sex
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

all[is.na(all$Embarked)]$Embarked = all[Sex=="female" & Pclass==1,.(Embarked = Mode(Embarked)),by=c("Sex","Pclass")]$Embarked

# Check if there is still missing value 
colSums(is.na(all))

# Create synthetic variables - family size
all$fam_size = all$SibSp + all$Parch + 1

# Transform factor variables into numeric indicators
dummies = dummyVars(Survived~.-Name-Ticket-Cabin-SPLIT,data=all)
all_dummy = predict(dummies,newdata=all)
all_dummy = as.data.frame(all_dummy)
# merge back Passenger Id and SPLIT
all_sub = subset(all,select=c(PassengerId,Survived,SPLIT))
allvars = merge(all_sub,all_dummy,by="PassengerId",all=T)

#################### Univariate Analysis #######################
# Divide allvars dataset into TRNTST and VAL
allvars_trn = allvars[allvars$SPLIT=="TRN"|allvars$SPLIT=="TST",]
allvars_val = allvars[allvars$SPLIT == "VAL",]
# Generate correlation summary
allvars_cor = cor(subset(allvars_trn,select=-c(PassengerId,SPLIT,Survived)),allvars_trn$Survived)
# transform correlation matrix to three column data frame
cor_summary = as.data.frame(as.table(allvars_cor))
colnames(cor_summary)[3] = "correlation"
cor_summary$Var2 = "Survived"
# create absolute correlation
cor_summary$abs_cor = abs(cor_summary$correlation)
# sort by absolute correlation
cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]
head(cor_summary,10)

# Visualize through mosaic plot
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

# Final cleansing
# Remove decile indicators
allvars_trn$fare_decile = NULL
allvars_trn$age_decile = NULL
# Keep one sex indicator is enough
allvars_trn$Sex.male = NULL
allvars_val$Sex.male = NULL
# Combine the cabin_num to two group, Z, non-Z
allvars_trn$cabin_cat = ifelse(allvars_trn$cabin_num.Z==1,0,1)
allvars_val$cabin_cat = ifelse(allvars_val$cabin_num.Z==1,0,1)
# Remove SPLIT
allvars_trn$SPLIT = NULL
allvars_val$SPLIT = NULL
# Format the target variable to be model viable
allvars_trn$Survived = ifelse(allvars_trn$Survived==0,"Dead","Survived")

############################ Modeling ###############################
# Pre-processing
set.seed(12345)
trn_ind = createDataPartition(allvars_trn$Survived,p=0.6,list=F)
allvars_trn_trn = allvars_trn[trn_ind,]
allvars_trn_val = allvars_trn[-trn_ind,]
# Set up train control
control = trainControl(method="cv",number=5,classProbs = T)

# SVM
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-cabin_num.Z-Embarked.Q,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control))
mod_svmRadial

svmRadialGrid = expand.grid(C=c(0.5,0.75,0.9,1,1.25),
                            sigma=c(0.01,0.02,0.03,0.04,0.05,0.1,0.12,0.15,0.2))
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q-cabin_num.Z,
                                   data=allvars_trn_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid))
mod_svmRadial
plot(mod_svmRadial)
svmRadial_pred = predict(mod_svmRadial,newdata=allvars_trn_val,type="raw")
confusionMatrix(svmRadial_pred,allvars_trn_val$Survived)

# build on the whole trn dataset
svmRadialGrid_final = expand.grid(C=0.75,
                                  sigma=0.15)
set.seed(123456) # System time: 
system.time(mod_svmRadial <- train(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                   -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                   -cabin_num.C-Embarked.Q-cabin_num.Z,
                                   data=allvars_trn,method="svmRadial",
                                   trControl = control,tuneGrid=svmRadialGrid_final))
mod_svmRadial
svmRadial_pred_final = predict(mod_svmRadial,newdata=allvars_val,type="raw")
svmRadial_pred_submit = data.frame(PassengerId = allvars_val$PassengerId,Survived = svmRadial_pred_final)
svmRadial_pred_submit = svmRadial_pred_submit[order(svmRadial_pred_submit$PassengerId),]
svmRadial_pred_submit$Survived = ifelse(svmRadial_pred_submit$Survived=="Dead",0,1)
prop.table(table(svmRadial_pred_submit$Survived))
write.csv(svmRadial_pred_submit,"pred_svmRadial_013106.csv",row.names = F)




