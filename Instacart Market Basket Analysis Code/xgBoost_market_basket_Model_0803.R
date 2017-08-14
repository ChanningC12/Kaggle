############################# Iteration 080401 #########################################
# Note: Incorporate Clustering Analysis, use 0730 5 clusters, all variables
# Tweak variables in each cluster model, examine ROC

rm(list=ls())
gc()

library(dplyr)
library(caret)
library(data.table)
library(ROCR)
library(xgboost)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
train <- data[data$eval_set=="train",]
rm(data)
gc()

# read in cluster label dataset
clust <- fread("../user_clust5.csv")

# merge cluster number to the training set
train <- train %>% left_join(clust,by="user_id")
train <- train %>% select(-eval_set,-user_id,-product_id,-order_id)
rm(clust)
gc()
# impute missing as 0
train[is.na(train)] <- 0


#################### Train the model 5 times based on cluster label #########################
params <- list(
  "objective"           = "reg:logistic", # defines the loss function to be minimized
  "eval_metric"         = "logloss", # The metric to be used for the validation data, rmse, mae, logloss, error, merror, mlogloss, auc
  "eta"                 = 0.1, # learning rate
  "max_depth"           = 8, # make splits upto the max_depth and then start pruning the trees backward, GBM simply stops when it gets negative gain
  "min_child_weight"    = 10, # minimum sum of weights of all observations required in a child
  "gamma"               = 0.70, # minimum loss reduction to make a split
  "subsample"           = 0.76, # fraction of observations to be randomly samples for each tree
  "colsample_bytree"    = 0.95, # fraction of columns to be randomly samples for each tree
  "alpha"               = 2e-05, # L1 regularization
  "lambda"              = 10 # L2 regularization
)


train <- as.data.frame.matrix(train)

# Separate training set based on cluster label
source("../Instacart Market Basket Analysis Code/model_var_0803.R")
train.1 <- train[train$cluster==1,model_var_1] %>% select(-cluster)
train.2 <- train[train$cluster==2,model_var_2] %>% select(-cluster)
train.3 <- train[train$cluster==3,model_var_3] %>% select(-cluster)
train.4 <- train[train$cluster==4,model_var_4] %>% select(-cluster)
train.5 <- train[train$cluster==5,model_var_5] %>% select(-cluster)

rm(train)
gc()

# Cluster 1
# Seperate into train and test dataset within train
set.seed(08030101)
trn.ind.1 = createDataPartition(train.1$reordered,p=0.7,list=F)
train.trn.1 = train.1[trn.ind.1,]
train.val.1 = train.1[-trn.ind.1,]
rm(train.1)
gc()

# Seperate into train and test dataset within train
X.trn.1 <- xgb.DMatrix(as.matrix(train.trn.1 %>% dplyr::select(-reordered)), label = train.trn.1$reordered)
model.1 <- xgboost(data = X.trn.1, params = params, nrounds = 100) # logloss: 0.252051
importance <- xgb.importance(colnames(X.trn.1), model = model.1) # http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#feature-importance
write.csv(importance,"../Model Output/080301/variable_importance_xgb_080301_clust1.csv",row.names = F)
xgb.ggplot.importance(importance)

# Apply training model to validation set
X.val.1 <- xgb.DMatrix(as.matrix(train.val.1 %>% dplyr::select(-reordered)),
                     label = train.val.1$reordered)
train.val.1$pred <- predict(model.1,X.val.1)
summary(train.val.1$pred)

# ROC
# ROC curve - RBO RF
pred <- prediction(train.val.1$pred,train.val.1$reordered)
roc <- performance(pred,measure="tpr",x.measure="fpr")
plot(roc,main="XGBoost Model ROC Curve",lwd=3,col="dark green")
abline(0,1,lty=2,col="red")
# AUC
auc = performance(pred,measure="auc")
unlist(auc@y.values) # 0.826
# F Score
f1.perf <- performance(pred,"f")
plot(f1.perf)
f_score <- data.frame(f1.perf@x.values,f1.perf@y.values)
names(f_score) <- c("x","y")
f_score[f_score$y==max(f_score[!(is.na(f_score$y)),]$y),] # 0.2364098 0.4658314
abline(v = 0.2364098,col="red",lty=2)


# Cluster 2
# Seperate into train and test dataset within train
set.seed(08030102)
trn.ind.2 = createDataPartition(train.2$reordered,p=0.7,list=F)
train.trn.2 = train.2[trn.ind.2,]
train.val.2 = train.2[trn.ind.2,]
rm(train.2)
gc()

# Seperate into train and test dataset within train
X.trn.2 <- xgb.DMatrix(as.matrix(train.trn.2 %>% dplyr::select(-reordered)), label = train.trn.2$reordered)
model.2 <- xgboost(data = X.trn.2, params = params, nrounds = 100) # logloss: 0.297646
importance <- xgb.importance(colnames(X.trn.2), model = model.2) # http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#feature-importance
write.csv(importance,"../Model Output/080301/variable_importance_xgb_080301_clust2.csv",row.names = F)
xgb.ggplot.importance(importance)

# Apply training model to validation set
X.val.2 <- xgb.DMatrix(as.matrix(train.val.2 %>% dplyr::select(-reordered)),
                       label = train.val.2$reordered)
train.val.2$pred <- predict(model.2,X.val.2)
summary(train.val.2$pred)

pred <- prediction(train.val.2$pred,train.val.2$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
f_score <- data.frame(f1.perf@x.values,f1.perf@y.values)
names(f_score) <- c("x","y")
f_score[f_score$y==max(f_score[!(is.na(f_score$y)),]$y),] # 0.2312099 0.4740378
abline(v = 0.2312099,col="red",lty=2)


# Cluster 3
# Seperate into train and test dataset within train
set.seed(08030103)
trn.ind.3 = createDataPartition(train.3$reordered,p=0.7,list=F)
train.trn.3 = train.3[trn.ind.3,]
train.val.3 = train.3[trn.ind.3,]
rm(train.3)
gc()

# Seperate into train and test dataset within train
X.trn.3 <- xgb.DMatrix(as.matrix(train.trn.3 %>% dplyr::select(-reordered)), label = train.trn.3$reordered)
model.3 <- xgboost(data = X.trn.3, params = params, nrounds = 100) # logloss: 0.170145
importance <- xgb.importance(colnames(X.trn.3), model = model.3) # http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#feature-importance
write.csv(importance,"../Model Output/080301/variable_importance_xgb_080301_clust3.csv",row.names = F)
xgb.ggplot.importance(importance)

# Apply training model to validation set
X.val.3 <- xgb.DMatrix(as.matrix(train.val.3 %>% dplyr::select(-reordered)),
                       label = train.val.3$reordered)
train.val.3$pred <- predict(model.3,X.val.3)
summary(train.val.3$pred)

pred <- prediction(train.val.3$pred,train.val.3$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
f_score <- data.frame(f1.perf@x.values,f1.perf@y.values)
names(f_score) <- c("x","y")
f_score[f_score$y==max(f_score[!(is.na(f_score$y)),]$y),] # 0.2345407 0.5820312
abline(v = 0.2345407,col="red",lty=2)


# Cluster 4
# Seperate into train and test dataset within train
set.seed(08030104)
trn.ind.4 = createDataPartition(train.4$reordered,p=0.7,list=F)
train.trn.4 = train.4[trn.ind.4,]
train.val.4 = train.4[trn.ind.4,]
rm(train.4)
gc()

# Seperate into train and test dataset within train
X.trn.4 <- xgb.DMatrix(as.matrix(train.trn.4 %>% dplyr::select(-reordered)), label = train.trn.4$reordered)
model.4 <- xgboost(data = X.trn.4, params = params, nrounds = 100) # logloss: 0.185443
importance <- xgb.importance(colnames(X.trn.4), model = model.4) # http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#feature-importance
write.csv(importance,"../Model Output/080301/variable_importance_xgb_080301_clust4.csv",row.names = F)
xgb.ggplot.importance(importance)

# Apply training model to validation set
X.val.4 <- xgb.DMatrix(as.matrix(train.val.4 %>% dplyr::select(-reordered)),
                       label = train.val.4$reordered)
train.val.4$pred <- predict(model.4,X.val.4)
summary(train.val.4$pred)

pred <- prediction(train.val.4$pred,train.val.4$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
f_score <- data.frame(f1.perf@x.values,f1.perf@y.values)
names(f_score) <- c("x","y")
f_score[f_score$y==max(f_score[!(is.na(f_score$y)),]$y),] # 0.2366466 0.4685698
abline(v = 0.2366466,col="red",lty=2)


# Cluster 5
# Seperate into train and test dataset within train
set.seed(08030105)
trn.ind.5 = createDataPartition(train.5$reordered,p=0.7,list=F)
train.trn.5 = train.5[trn.ind.5,]
train.val.5 = train.5[trn.ind.5,]
rm(train.5)
gc()

# Seperate into train and test dataset within train
X.trn.5 <- xgb.DMatrix(as.matrix(train.trn.5 %>% dplyr::select(-reordered)), label = train.trn.5$reordered)
model.5 <- xgboost(data = X.trn.5, params = params, nrounds = 100) # logloss: 0.274148
importance <- xgb.importance(colnames(X.trn.5), model = model.5) # http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html#feature-importance
write.csv(importance,"../Model Output/080301/variable_importance_xgb_080301_clust5.csv",row.names = F)
xgb.ggplot.importance(importance)

# Apply training model to validation set
X.val.5 <- xgb.DMatrix(as.matrix(train.val.5 %>% dplyr::select(-reordered)),
                       label = train.val.5$reordered)
train.val.5$pred <- predict(model.5,X.val.5)
summary(train.val.5$pred)

pred <- prediction(train.val.5$pred,train.val.5$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
f_score <- data.frame(f1.perf@x.values,f1.perf@y.values)
names(f_score) <- c("x","y")
f_score[f_score$y==max(f_score[!(is.na(f_score$y)),]$y),] # 0.2228316 0.4316837
abline(v = 0.2228316,col="red",lty=2)



############################# Predict on Test set #######################################
rm(list=setdiff(ls(),c("model.1","model.2","model.3","model.4","model.5","model_var")))
gc()

# read in user and product level datasets
test <- fread("../Processed Data/data_pre_model_0723.csv")
test <- test[test$eval_set=="test",]
test <- as.data.frame.matrix(test)

# read in cluster label dataset
clust <- fread("../user_clust5.csv")
test_var <- c(model_var,"order_id", "product_id")
test <- test %>% left_join(clust,by="user_id")
test <- test[,test_var]

gc()
test[is.na(test)] <- 0

# Separate test set based on cluster label
test.1 <- test[test$cluster==1,] %>% select(-cluster)
test.2 <- test[test$cluster==2,] %>% select(-cluster)
test.3 <- test[test$cluster==3,] %>% select(-cluster)
test.4 <- test[test$cluster==4,] %>% select(-cluster)
test.5 <- test[test$cluster==5,] %>% select(-cluster)

rm(test)
gc()

# Predict on cluster 1
X <- xgb.DMatrix(as.matrix(test.1 %>% select(-order_id, -product_id)))
test.1$reordered_prob <- predict(model.1, X) # get the probability
summary(test.1$reordered_prob)
test.1$reordered <- (test.1$reordered_prob > 0.2182838) * 1

# Predict on cluster 2
X <- xgb.DMatrix(as.matrix(test.2 %>% select(-order_id, -product_id)))
test.2$reordered_prob <- predict(model.2, X) # get the probability
summary(test.2$reordered_prob)
test.2$reordered <- (test.2$reordered_prob > 0.2312099) * 1

# Predict on cluster 3
X <- xgb.DMatrix(as.matrix(test.3 %>% select(-order_id, -product_id)))
test.3$reordered_prob <- predict(model.3, X) # get the probability
summary(test.3$reordered_prob)
test.3$reordered <- (test.3$reordered_prob > 0.2345407) * 1

# Predict on cluster 4
X <- xgb.DMatrix(as.matrix(test.4 %>% select(-order_id, -product_id)))
test.4$reordered_prob <- predict(model.4, X) # get the probability
summary(test.4$reordered_prob)
test.4$reordered <- (test.4$reordered_prob > 0.2366466) * 1

# Predict on cluster 5
X <- xgb.DMatrix(as.matrix(test.5 %>% select(-order_id, -product_id)))
test.5$reordered_prob <- predict(model.5, X) # get the probability
summary(test.5$reordered_prob)
test.5$reordered <- (test.5$reordered_prob > 0.2228316) * 1

test <- do.call("rbind", list(test.1, test.2, test.3, test.4, test.5))

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " "))

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None")

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_01",".csv",
                                   sep = ""), row.names = F)
