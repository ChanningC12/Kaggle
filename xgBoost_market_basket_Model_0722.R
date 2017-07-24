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

# Manipulate training set to remove redundant variables
train <- fread("../Processed Data/data_pre_model_train_0715.csv")

train <- train %>% select(-total_orders_A,-total_orders_E,-total_orders_M,-total_orders_N,
                          -total_orders_0,-total_orders_1,-total_orders_2,-total_orders_3,-total_orders_4,
                          -total_orders_5,-total_orders_6)
fwrite(train,"../Processed Data/data_pre_model_train_0722.csv")

# Model -------------------------------------------------------------------
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
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

# vars_xgboost <- c("time_since_last_order",
#                   "prod_orders",
#                   "prod_reorder_probability",
#                   "days_since_prior_order",
#                   "user_reorder_ratio",
#                   "prod_reorder_times",
#                   "up_orders_since_last_order",
#                   "up_order_rate",
#                   "user_average_distinct_prod",
#                   "up_orders",
#                   "up_order_rate_since_first_order",
#                   "user_mean_days_since_prior",
#                   "add_to_cart_pct",
#                   "user_period",
#                   "user_average_basket",
#                   "user_distinct_products",
#                   "user_orders",
#                   "up_last_order_prod",
#                   "up_first_order",
#                   "up_average_cart_position",
#                   "up_average_order_size_prod",
#                   "up_last_order",
#                   "user_total_products",
#                   "top_aisle_id_ind",
#                   "reordered")
# train <- train[,..vars_xgboost]

# Seperate into train and test dataset within train
set.seed(072201)
trn_ind = createDataPartition(train$reordered,p=0.6,list=F)
train_trn = train[trn_ind,]
train_val = train[-trn_ind,]
rm(train)
gc()

X.trn <- xgb.DMatrix(as.matrix(train_trn %>% dplyr::select(-reordered)), label = train_trn$reordered)
model <- xgboost(data = X.trn, params = params, nrounds = 100) # 0.241787

importance <- xgb.importance(colnames(X.trn), model = model)
write.csv(importance,"../variable_importance_072201.csv",row.names = F)
xgb.ggplot.importance(importance)

# apply training model to validation set
X.val <- xgb.DMatrix(as.matrix(train_val %>% dplyr::select(-reordered)),
                     label = train_val$reordered)
train_val$pred <- predict(model,X.val)
summary(train_val$pred)

library(ROCR)
pred <- prediction(train_val$pred,train_val$reordered)
f1.perf <- performance(pred,"f")
plot(f1.perf)
abline(v = 0.21,col="red",lty=2) # choose 0.22

rm(list=setdiff(ls(),"model"))
gc()

# Apply model -------------------------------------------------------------
test <- fread("../Processed Data/data_pre_model_test_0715.csv")
test <- test %>% select(-total_orders_A,-total_orders_E,-total_orders_M,-total_orders_N,
                          -total_orders_0,-total_orders_1,-total_orders_2,-total_orders_3,-total_orders_4,
                          -total_orders_5,-total_orders_6,
                          -eval_set,-user_id,-reordered)
test[is.na(test)] <- 0
fwrite(test,"../Processed Data/data_pre_model_train_0722.csv")

X <- xgb.DMatrix(as.matrix(test %>% dplyr::select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)

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
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_021",".csv",
                                   sep = ""), row.names = F)
