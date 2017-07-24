########################### Instacart Market Basket Analysis ##################################
rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)
library(caret)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")

train <- data[data$eval_set=="train",]
rm(data)
gc()

train <- train %>% select(-eval_set,-user_id,-product_id,-order_id)
gc()
train[is.na(train)] <- 0

#################### Train the model #########################
library(xgboost)

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

# Seperate into train and test dataset within train
set.seed(072301)
trn_ind = createDataPartition(train$reordered,p=0.6,list=F)
train_trn = train[trn_ind,]
train_val = train[-trn_ind,]
rm(train)
gc()

X.trn <- xgb.DMatrix(as.matrix(train_trn %>% dplyr::select(-reordered)), label = train_trn$reordered)
model <- xgboost(data = X.trn, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X.trn), model = model)
write.csv(importance,"../variable_importance_072301.csv",row.names = F)
xgb.ggplot.importance(importance)

# apply training model to validation set
X.val <- xgb.DMatrix(as.matrix(train_val %>% dplyr::select(-reordered)),
                     label = train_val$reordered)
train_val$pred <- predict(model,X.val)

library(ROCR)
pred <- prediction(train_val$pred,train_val$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
abline(v = 0.21,col="red",lty=2) # choose 0.22

rm(list=setdiff(ls(),"model"))
gc()

################### Predict on Test Set #########################
# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
test <- data[data$eval_set=="test",]
rm(data)
gc()

test <- test %>% select(-eval_set,-user_id)
test[is.na(test)] <- 0

X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)
hist(test$reordered)

test$reordered <- (test$reordered > 0.22) * 1 # need to stress test the threshold

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " "))

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None")

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_021",".csv",
                                   sep = ""), row.names = F)


############################# Iteration 072302 #########################################
rm(list=ls())
gc()
# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
train <- data[data$eval_set=="train",]
rm(data)
gc()

train <- train %>% select(-eval_set,-user_id,-product_id,-order_id)
gc()
train[is.na(train)] <- 0

#################### Train the model #########################
library(xgboost)

params <- list(
  "objective"           = "reg:logistic", # defines the loss function to be minimized
  "eval_metric"         = "logloss", # The metric to be used for the validation data, rmse, mae, logloss, error, merror, mlogloss, auc
  "eta"                 = 0.08, # learning rate
  "max_depth"           = 6, # make splits upto the max_depth and then start pruning the trees backward, GBM simply stops when it gets negative gain
  "min_child_weight"    = 10, # minimum sum of weights of all observations required in a child
  "gamma"               = 0.70, # minimum loss reduction to make a split
  "subsample"           = 0.76, # fraction of observations to be randomly samples for each tree
  "colsample_bytree"    = 0.95, # fraction of columns to be randomly samples for each tree
  "alpha"               = 2e-05, # L1 regularization
  "lambda"              = 10 # L2 regularization
)

# Seperate into train and test dataset within train
X.trn <- xgb.DMatrix(as.matrix(train %>% dplyr::select(-reordered)), label = train$reordered)
model <- xgboost(data = X.trn, params = params, nrounds = 100)

importance <- xgb.importance(colnames(X.trn), model = model)
write.csv(importance,"../variable_importance_072302.csv",row.names = F)
xgb.ggplot.importance(importance)


################### Predict on Test Set #########################
rm(list=setdiff(ls(),"model"))
gc()
# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
test <- data[data$eval_set=="test",]
rm(data)
gc()

test <- test %>% select(-eval_set,-user_id)
test[is.na(test)] <- 0

X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)
hist(test$reordered)

test$reordered <- (test$reordered > 0.21) * 1 # need to stress test the threshold

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " "))

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None")

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_02",".csv",
                                   sep = ""), row.names = F)


############################# Iteration 072303 #########################################
rm(list=ls())
gc()
# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
train <- data[data$eval_set=="train",]
rm(data)
gc()

train <- train %>% select(-eval_set,-user_id,-product_id,-order_id)
gc()
train[is.na(train)] <- 0

#################### Train the model #########################
library(xgboost)

params <- list(
  "objective"           = "reg:logistic", # defines the loss function to be minimized
  "eval_metric"         = "logloss", # The metric to be used for the validation data, rmse, mae, logloss, error, merror, mlogloss, auc
  "eta"                 = 0.1, # learning rate
  "max_depth"           = 6, # make splits upto the max_depth and then start pruning the trees backward, GBM simply stops when it gets negative gain
  "min_child_weight"    = 10, # minimum sum of weights of all observations required in a child
  "gamma"               = 0.70, # minimum loss reduction to make a split
  "subsample"           = 0.76, # fraction of observations to be randomly samples for each tree
  "colsample_bytree"    = 0.95, # fraction of columns to be randomly samples for each tree
  "alpha"               = 2e-05, # L1 regularization
  "lambda"              = 10 # L2 regularization
)


model_var <- c("up_order_rate",
               "up_orders_since_last_order",
               "up_order_rate_since_first_order",
               "days_since_prior_order",
               "up_orders",
               "up_avg_interval",
               "prod_reorder_probability",
               "prod_reorder_times",
               "time_since_last_order",
               "user_reorder_ratio",
               "up_last_order_prod",
               "user_average_distinct_prod",
               "prod_orders",
               "up_days_interval_sd_grp",
               "reordered"
)

train <- train[,..model_var]

# Seperate into train and test dataset within train
X.trn <- xgb.DMatrix(as.matrix(train %>% dplyr::select(-reordered)), label = train$reordered)
model <- xgboost(data = X.trn, params = params, nrounds = 100)

importance <- xgb.importance(colnames(X.trn), model = model)
write.csv(importance,"../variable_importance_072303.csv",row.names = F)
xgb.ggplot.importance(importance)


################### Predict on Test Set #########################
rm(list=setdiff(ls(),"model"))
gc()
# read in user and product level datasets
data <- fread("../Processed Data/data_pre_model_0723.csv")
test <- data[data$eval_set=="test",]
rm(data)
gc()

test <- test %>% select(-eval_set,-user_id)
test[is.na(test)] <- 0

X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)
hist(test$reordered)

test$reordered <- (test$reordered > 0.21) * 1 # need to stress test the threshold

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " "))

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None")

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_03",".csv",
                                   sep = ""), row.names = F)











