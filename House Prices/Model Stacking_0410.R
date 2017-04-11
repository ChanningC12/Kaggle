##### House Prices Prediction #######
rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/House Prices/")
library(caret)
library(mice)
library(xgboost)

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

# 1337s
system.time(
  fixedAll <- estimateMissingVariables(all)
)

colSums(is.na(fixedAll))
fixedAll[is.na(fixedAll$BsmtHalfBath),]$BsmtHalfBath = 0
colSums(is.na(fixedAll))


################# Feature Engineering ######################
# Switch variables to the right class
str(fixedAll)
fixedAll$MSSubClass = as.factor(fixedAll$MSSubClass)

# Categorical: ANOVA
# Numerical: Correlation Summary
# Use Random Forest, CART, GBM, KNN, NNET, SVM to generate variable importance summary and look at overlap

### Correlation Report ###
# keep only numeric columns
num <- sapply(fixedAll,is.numeric)
# generate correlation
cor = cor(fixedAll[!(is.na(fixedAll$SalePrice)),num],fixedAll[!(is.na(fixedAll$SalePrice)),num]$SalePrice,
          use="complete.obs",method="pearson")
# convert the correlation report to data.frame
cor_summary = as.data.frame(as.table(cor))
cor_summary$Var2 = "SalePrice"
# change the column name to "correlation"
colnames(cor_summary)[3] = "correlation"
# create absolute correlation
cor_summary$abs_cor = abs(cor_summary$correlation)
# sort by absolute correlation
cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]

# Variables to keep, absolute correlation is greater than 0.1
variable_to_rm_cor = cor_summary[cor_summary$abs_cor<=0.1 & cor_summary$Var1!="Id",]$Var1

### ANOVA ###
fac <- sapply(fixedAll,is.factor)

for (i in 1:ncol(fixedAll[,fac])) {
  summary[i] <- summary(aov(fixedAll$SalePrice~fixedAll[,fac][,i],data=fixedAll[,fac]))
  p_value[i] <- summary[i][[1]][["Pr(>F)"]][[1]]
}

# summary table for ANOVA analysis
p_value_summary = data.frame(variable = names(fixedAll[,fac]),p_value)
p_value_summary$importance = ifelse(p_value_summary$p_value<0.05,"H",ifelse(p_value_summary$p_value>0.2,"L","M"))
table(p_value_summary$importance)

variable_to_rm_aov = p_value_summary[p_value_summary$p_value>0.05,]$variable

# Remove identified variables
all = fixedAll[,!(names(fixedAll) %in% variable_to_rm_cor) & !(names(fixedAll) %in% variable_to_rm_aov)]
str(all)

### Variable Importance ###
### Variable Importance ###
ctrl = trainControl(method = "cv", number = 5)
# CART
system.time(
  all_rpart <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                     method="rpart", trControl = ctrl, metric="RMSE",tuneLength=10)
)
all_rpart
var_imp_rpart = varImp(all_rpart)
var_imp_rpart = data.frame(importance = var_imp_rpart$importance)
var_imp_rpart$percent = var_imp_rpart$Overall / sum(var_imp_rpart$Overall)
var_imp_rpart = var_imp_rpart[order(var_imp_rpart$percent,decreasing = T),]


# Random Forest
rf_grid = expand.grid(mtry=c(30,50,70,90))
system.time(
  all_rf <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                  method="rf", trControl = ctrl, metric="RMSE",
                  tuneGrid = rf_grid)
)

all_rf
var_imp_rf = data.frame(importance(all_rf$finalModel))
var_imp_rf$percent = var_imp_rf$IncNodePurity / sum(var_imp_rf$IncNodePurity)
var_imp_rf = var_imp_rf[order(var_imp_rf$percent,decreasing = T),]

# GBM
system.time(
  all_gbm <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                   method="gbm", trControl = ctrl, metric="RMSE",
                   tuneLength=5)
)

all_gbm
var_imp_gbm = varImp(all_gbm)
var_imp_gbm = data.frame(importance = var_imp_gbm$importance)
var_imp_gbm$percent = var_imp_gbm$Overall / sum(var_imp_gbm$Overall)
var_imp_gbm = var_imp_gbm[order(var_imp_gbm$percent,decreasing = T),]

# KNN
system.time(
  all_knn <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                   method="knn", trControl = ctrl, metric="RMSE",
                   tuneLength=10)
)

all_knn
var_imp_knn = varImp(all_knn)
var_imp_knn = data.frame(importance = var_imp_knn$importance)
var_imp_knn$percent = var_imp_knn$Overall / sum(var_imp_knn$Overall)
var_imp_knn = var_imp_knn[order(var_imp_knn$percent,decreasing = T),]

# NNET
system.time(
  all_nnet <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                    method="nnet", trControl = ctrl, metric="RMSE",
                    tuneLength=5)
)

all_nnet
var_imp_nnet = varImp(all_nnet)
var_imp_nnet = data.frame(importance = var_imp_nnet$importance)
var_imp_nnet$percent = var_imp_nnet$Overall / sum(var_imp_nnet$Overall)
var_imp_nnet = var_imp_nnet[order(var_imp_nnet$percent,decreasing = T),]

# SVM
system.time(
  all_svm <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                   method="svmLinear", trControl = ctrl, metric="RMSE",
                   tuneLength=5)
)

all_svm
var_imp_svm = varImp(all_svm)
var_imp_svm = data.frame(importance = var_imp_svm$importance)
var_imp_svm$percent = var_imp_svm$Overall / sum(var_imp_svm$Overall)
var_imp_svm = var_imp_svm[order(var_imp_svm$percent,decreasing = T),]

### Get top predictors
rf_top = row.names(var_imp_rf[c(1:20),])
rpart_top = row.names(var_imp_rpart[c(1:20),])
gbm_top = row.names(var_imp_gbm[c(1:20),])
nnet_top = row.names(var_imp_nnet[c(1:20),])

top_predictor = union(rf_top,rpart_top)
top_predictor = union(top_predictor,gbm_top)

knn_top = row.names(var_imp_knn[c(1:20),])
svm_top = row.names(var_imp_svm[c(1:20),])
top_predictor_2 = union(knn_top,svm_top)

top_predictor_final = union(top_predictor,top_predictor_2)
top_predictor_final = c(top_predictor_final,"CentralAir","Neighborhood","SaleType")


##################### Modeling ############################
# Random Forest
rf_grid = expand.grid(mtry=c(20,35,50,65,80,100))
system.time(
  all_rf <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                  method="rf", trControl = ctrl, metric="RMSE",
                  tuneGrid = rf_grid)
)

all_rf
# Predict on the test set
rf_pred = predict(all_rf,newdata=all[all$Id>1460,],type="raw")
submission = data.frame(Id = all[all$Id>1460,]$Id,
                        SalePrice = rf_pred)
write.csv(submission,"submission_rf_0407.csv",row.names=F)

# GBM
system.time(
  all_gbm <- train(SalePrice ~.-Id, data=all[all$Id<=1460,], 
                   method="gbm", trControl = ctrl, metric="RMSE",
                   tuneLength=10)
)

all_gbm
gbm_pred = predict(all_gbm,newdata=all[all$Id>1460,],type="raw")
submission = data.frame(Id = all[all$Id>1460,]$Id,
                        SalePrice = gbm_pred)
write.csv(submission,"submission_gbm_0407.csv",row.names=F)

# XGBoost
dmy <- dummyVars(" ~ .",data=all)
all_dmy <- predict(dmy, newdata=all)
all_dmy <- as.data.frame(all_dmy)

xgb_tune <-  expand.grid(nrounds=100, 
                                max_depth=10, 
                                eta=0.1, 
                                gamma=0, 
                                colsample_bytree=0.6,
                                min_child_weight = 1)

system.time(
  all_xgb <- train(SalePrice ~.-Id, data=all_dmy[all_dmy$Id<=1460,], 
                   method="xgbTree", trControl = ctrl, metric="RMSE",
                   tuneGrid = xgb_tune)
)

all_xgb
xgb_pred = predict(all_xgb,newdata=all_dmy[all_dmy$Id>1460,],type="raw")
xgb_submission <- data.frame(Id = all_dmy[all_dmy$Id>1460,]$Id,
                          SalePrice = xgb_pred)
write.csv(xgb_submission,"submission_xgb_0410_3.csv",row.names = F)


###################### Model Stacking using NNet ##########################
rf_pred_orig = predict(all_rf,newdata=all[all$Id<=1460,],type="raw")
gbm_pred_orig = predict(all_gbm,newdata=all[all$Id<=1460,],type="raw")
xgb_pred_orig = predict(all_xgb,newdata=all_dmy[all_dmy$Id<=1460,],type="raw")

all_stack = data.frame(
  rf_pred = rf_pred_orig,
  gbm_pred = gbm_pred_orig,
  xgb_pred = xgb_pred_orig
)

all_stack <- all_stack %>%
  mutate(rf_rank = rank(rf_pred,ties.method = "first"),
         gbm_rank = rank(gbm_pred,ties.method = "first"),
         xgb_rank = rank(xgb_pred,ties.method = "first"))

# Add response variable
all_stack$SalePrice <- all[all$Id<=1460,]$SalePrice

# GBM
system.time(
  all_stack <- train(SalePrice ~., data=all_stack, 
                   method="gbm", trControl = ctrl, metric="RMSE", tuneLength=5)
)

all_stack
varImp(all_stack)

# Prepare predicted dataset
all_stack_pred <- data.frame(
  rf_pred = rf_pred,
  gbm_pred = gbm_pred,
  xgb_pred = xgb_pred
)

all_stack_pred <- all_stack_pred %>%
  mutate(rf_rank = rank(rf_pred,ties.method = "first"),
         gbm_rank = rank(gbm_pred,ties.method = "first"),
         xgb_rank = rank(xgb_pred,ties.method = "first"))

stack_pred = predict(all_stack,newdata=all_stack_pred,type="raw")
stack_submission <- data.frame(Id = all_dmy[all_dmy$Id>1460,]$Id,
                             SalePrice = stack_pred)
write.csv(stack_submission,"submission_stacking_0410_2.csv",row.names = F)



