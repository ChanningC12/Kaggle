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

# Load in datasets, use na.strings to convert blank as missing
training = read.csv("train.csv",na.strings=c("","NA"))
testing = read.csv("test.csv",na.strings=c("","NA"))

# Check the structure of the data
str(training)

# Drop Name, Ticket
training$Name=NULL
testing$Name=NULL
training$Ticket=NULL
testing$Ticket=NULL

# Distribution of the variables
# Survived
hist(training$Survived)
table(training$Survived)
prop.table(table(training$Survived))

# Pclass
hist(training$Pclass)
table(training$Pclass)
prop.table(table(training$Pclass))

# Sex
table(training$Sex)
prop.table(table(training$Sex))

# Age
hist(training$Age)

# SibSp
table(training$SibSp)

# parch
table(training$Parch)

# fare
hist(training$Fare)

# cabin
unique(training$Cabin)

# embark
table(training$Embarked)

# create missing value summary
na_summary = data.frame(num_na = colSums(is.na(training)))
na_summary = data.frame(Variable = rownames(na_summary),na_summary,
                        na_ratio = na_summary$num_na/nrow(training))

# Drop the Cabin variable
training = training[,na_summary$na_ratio<0.7]

# Impute the missing ages based on average age by sex and Pclass
training = as.data.table(training)
age_summary = training[,.(Age = mean(Age,na.rm=T)),by=c("Sex","Pclass")]
age_summary$Age_abs = round(age_summary$Age,0)

training$Age[training$Sex == "male" & training$Pclass == 1 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 1,Age_abs]

training$Age[training$Sex == "male" & training$Pclass == 2 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 2,Age_abs]

training$Age[training$Sex == "male" & training$Pclass == 3 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 3,Age_abs]

training$Age[training$Sex == "female" & training$Pclass == 1 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 1,Age_abs]

training$Age[training$Sex == "female" & training$Pclass == 2 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 2,Age_abs]

training$Age[training$Sex == "female" & training$Pclass == 3 
             & is.na(training$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 3,Age_abs]

  
# Impute the missing embarked with mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

training$Embarked[is.na(training$Embarked)] = Mode(training$Embarked)

# change to the Survived name to correct format for modeling purpose
table(training$Survived)
training$Survived = ifelse(training$Survived==0,"Dead","Survived")

# check if there is still missing value
colSums(is.na(training))

############# Simple Modeling ##############
# divide into trn/tst/val
set.seed(1234)
ind = createDataPartition(training$Survived,p=0.6,list=F)
training_trn = training[ind,]
training_val = training[-ind,]
control = trainControl(method="cv",number=5,classProbs = T)

## Decision Tree ##
rpartGrid = expand.grid(cp=seq(0.001,0.03,0.001))
set.seed(12345) # System time: 
system.time(mod_rpart <- train(as.factor(Survived)~.-PassengerId,
                               data=training_trn,method="rpart",
                               trControl = control,tuneGrid=rpartGrid))
mod_rpart
varImp(mod_rpart)

rpart_pred = predict(mod_rpart,newdata=training_val,type="raw")
confusionMatrix(rpart_pred,training_val$Survived)


## GLM ##
set.seed(123456) # System time: 
system.time(mod_glm <- train(as.factor(Survived)~.-PassengerId,
                             data=training_trn,method="glm",family="binomial",
                             trControl = control))
mod_glm
summary(mod_glm)

glm_pred = predict(mod_glm,newdata=training_val,type="raw")
confusionMatrix(glm_pred,training_val$Survived)


