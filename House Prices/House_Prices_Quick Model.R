##### House Prices Prediction #######
rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/House Prices/")
library(caret)
library(mice)

# Load in data #
train = read.csv("train.csv")
test = read.csv("test.csv")
dim(train)
str(train)

# Combine train and test to impute missing values
test$SalePrice = NA
all = rbind(train,test)
dim(all)

############### Missing Value Imputation ##################
### Remove variables with over 80% missing values
all_miss = data.frame(num_miss = colSums(is.na(all)))
all_miss = data.frame(Variable = row.names(all_miss),
    num_miss = all_miss)
all_miss = all_miss[order(all_miss$num_miss,decreasing = T),]
all_miss$miss_percent = all_miss$num_miss/nrow(all)

# Variables to remove, missing values over 80%
variable_to_rm = row.names(all_miss[all_miss$miss_percent>=0.8,])

### Remove variables with over 95% one level factor, near zero variance
# Frequency ratio, the most prevalent value over the second most frequent value
# Percent of unique values, number of unique values divided by the total number of sample
nzv <- nearZeroVar(all,saveMetrics = T, freqCut = 98/2, uniqueCut = 10)
nzv <- nzv[order(nzv$nzv,decreasing = T),]
variable_to_rm_nzv = row.names(nzv[nzv$freqRatio>98/2 & nzv$percentUnique<10,])

all = all[,!(names(all) %in% variable_to_rm) & !(names(all) %in% variable_to_rm_nzv)]
all$FireplaceQu = NULL
str(all)

### Impute missing values
all_miss = data.frame(num_miss = colSums(is.na(all)))
all_miss = data.frame(Variable = row.names(all_miss),
                      num_miss = all_miss)
all_miss = all_miss[order(all_miss$num_miss,decreasing = T),]
all_miss$miss_percent = all_miss$num_miss/nrow(all)

# MICE random forest missing imputation
estimateMissingVariables <- function(data){
    predictors = names(all[,3:ncol(all)-1])
    set.seed(2)
    capture.output(model <- mice(data[,names(data) %in% predictors],method="rf"))
    output <- complete(model)
    data$LotFrontage <- output$LotFrontage
    data$GarageYrBlt <- output$GarageYrBlt
    data$GarageFinish <- output$GarageFinish
    data$GarageQual <- output$GarageQual
    data$GarageCond <- output$GarageCond
    data$GarageType <- output$GarageType
    data$BsmtCond <- output$BsmtCond
    data$BsmtExposure <- output$BsmtExposure
    data$BsmtQual <- output$BsmtQual
    data$BsmtFinType2 <- output$BsmtFinType2
    data$BsmtFinType1 <- output$BsmtFinType1
    data$MasVnrType <- output$MasVnrType
    data$MasVnrArea <- output$MasVnrArea
    data$MSZoning <- output$MSZoning
    data$BsmtFullBath <- output$BsmtFullBath
    data$Functional <- output$Functional
    data$Exterior1st <- output$Exterior1st
    data$Exterior2nd <- output$Exterior2nd
    data$BsmtFinSF1 <- output$BsmtFinSF1
    data$BsmtUnfSF <- output$BsmtUnfSF
    data$TotalBsmtSF <- output$TotalBsmtSF
    data$Electrical <- output$Electrical
    data$KitchenQual <- output$KitchenQual
    data$GarageCars <- output$GarageCars
    data$GarageArea <- output$GarageArea
    data$SaleType <- output$SaleType
    return(data)
}

system.time(
fixedAll <- estimateMissingVariables(all)
)

fixedAll[is.na(fixedAll$BsmtHalfBath),]$BsmtHalfBath = 0
colSums(is.na(fixedAll))


##### Fast model ######
train <- fixedAll[fixedAll$Id <= 1460,]
test <- fixedAll[fixedAll$Id > 1460,]

ind = createDataPartition(train$SalePrice,p=0.7,list=F)
house_trn = train[ind,]
house_val = train[-ind,]

# Random Forest
colSums(is.na(house_trn))
ctrl = trainControl(method = "cv", number = 5)
system.time(
    house_rf <- train(SalePrice ~.-Id, data=house_trn, 
                     method="rf", trControl = ctrl, metric="RMSE",
                     tuneLength=10)
)

house_rf

house_val_pred = predict(house_rf,newdata=house_val)
house_val_pred = data.frame(house_val,pred=house_val_pred)

# RMSE
sqrt(sum((house_val_pred$pred - house_val_pred$SalePrice)^2)/nrow(house_val))

# Train on thw entire dataset
rf_Grid = expand.grid(mtry=139)
system.time(
    house_rf_all <- train(SalePrice ~.-Id, 
                       data=train, 
                       method="rf", trControl = ctrl, metric="RMSE", 
                       tuneGrid=rf_Grid)
)

house_rf_all

# Predict on the test set
test_pred = predict(house_rf_all,newdata=test,type="raw")
submission = data.frame(Id = test$Id,
                        SalePrice = test_pred)
write.csv(submission,"submission_fast_model_0331.csv",row.names=F)

# GBM
system.time(
    house_gbm <- train(SalePrice ~.-Id, data=house_trn, 
                      method="gbm", trControl = ctrl, metric="RMSE",
                      tuneLength=10)
)

house_gbm

house_val_pred = predict(house_gbm,newdata=house_val)
house_val_pred = data.frame(house_val,pred=house_val_pred)

# RMSE
sqrt(sum((house_val_pred$pred - house_val_pred$SalePrice)^2)/nrow(house_val))

# Train on thw entire dataset
gbm_Grid = expand.grid(n.trees=100,interaction.depth = 4, shrinkage = 0.1, n.minobsinnode = 10)
system.time(
    house_gbm_all <- train(SalePrice ~.-Id, 
                          data=train, 
                          method="gbm", trControl = ctrl, metric="RMSE", 
                          tuneGrid=gbm_Grid)
)

house_gbm_all

# Predict on the test set
test_pred = predict(house_gbm_all,newdata=test,type="raw")
submission = data.frame(Id = test$Id,
                        SalePrice = test_pred)
write.csv(submission,"submission_gbm_0331.csv",row.names=F)



