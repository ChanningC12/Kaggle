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

# Load Data ---------------------------------------------------------------
aisles <- fread("aisles.csv/aisles.csv", key = "aisle_id")
departments <- fread("departments.csv/departments.csv", key = "department_id")
products <- fread("products.csv/products.csv", key = c("product_id","aisle_id", "department_id")) 
orderp <- fread("order_products__prior.csv/order_products__prior.csv") # order_id and product_id level
ordert <- fread("order_products__train.csv/order_products__train.csv") # order_id level
orders <- fread("orders.csv/orders.csv") # order_id level

# Clean data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

# Merge aisle, department information with products into one table
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments)
rm(aisles, departments)

# Map user_id to order train dataset
n_distinct(orders$order_id) # 3,421,083 distinct total orders: 3,214,874 prior, 131,209 train, 75,000 test
table(orders$eval_set)
n_distinct(orders[orders$eval_set=="train",]$order_id) # 131,209 distinct orders in train
n_distinct(ordert$order_id) # 131,209
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)] # match the user_id for ordert table

# Merge user_id, order information to order_prior dataset, only keep prior orders to build variables
orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()

View(head(orders_products,100))

############################ Variable Creation ##################################
# Products ----------------------------------------------------------------
# product level metrics definition
# 1. prod_orders: for prior orders, total # products being purchased
# 2. prod_reorder_probability: Ratio that second order reordered the product in the first order
# 3. prod_reorder_times: times that a product being reordered overall
# 4. prod_reorder_ratio: reorders ratio by product

prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2))

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders # same as average products purchased by user who made the purchase
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% dplyr::select(-prod_reorders, -prod_first_orders, -prod_second_orders)

# Users -------------------------------------------------------------------
# user level metrics definition
# 1. prod_orders: for prior orders, total # products being purchased
# 2. prod_reorder_probability: Ratio that second order reordered the product in the first order
# 3. prod_reorder_times: times that a product being reordered overall
# 4. prod_reorder_ratio: reorders ratio by product

users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T))

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id))

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  dplyr::select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

# user total products / user distinct products, indicates the degree of reorder for a user
users$user_average_distinct_prod <- users$user_total_products / users$user_distinct_products

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

# calcualte average order size with the products
data2 <- orders_products %>%
  group_by(user_id, order_id) %>% 
  dplyr::mutate(
    products_order = max(add_to_cart_order)) %>% ungroup() %>% group_by_(.dots = c("user_id","product_id")) %>%
  dplyr::summarize(
    order_product = n(),
    order_size_products = sum(products_order))
data2$up_average_order_size_prod <- data2$order_size_products/data2$order_product

data <- data %>% inner_join(data2[,c("user_id","product_id","up_average_order_size_prod")],by=c("user_id","product_id"))

rm(data2)
gc()

# calculate add_to_cart percentage
data3 <- orders_products %>%
  group_by(user_id, order_id) %>% 
  dplyr::mutate(
    products_order = max(add_to_cart_order)) %>% 
  dplyr::mutate(
    add_to_cart_pct = add_to_cart_order / products_order) %>% ungroup() %>% group_by_(.dots = c("user_id","product_id")) %>%
  dplyr::summarize(
    add_to_cart_pct = mean(add_to_cart_pct,na.rm=T))

data <- data %>% inner_join(data3,by=c("user_id","product_id"))

rm(data3)
gc()

# calcualte the days since last time ordered the product
data4 <- orders_products %>%
  group_by(user_id) %>% 
  dplyr::summarize(
    last_order = max(order_number)) 

data4 <- data4 %>% right_join(data[,c("user_id","product_id","up_last_order")],by="user_id")

rm(orders_products)
gc()

library(sqldf)
data5 <- sqldf("select user_id,product_id,sum(days_since_prior_order) as days_since_prior_order
              from
              (select data4.user_id, data4.product_id, data4.last_order, data4.up_last_order, 
              orders.order_number,orders.days_since_prior_order
              from data4 left join orders on (
              (data4.up_last_order<orders.order_number) and
              (data4.last_order>=orders.order_number) and
              (data4.user_id=orders.user_id)
              ))
              group by user_id,product_id")
data5[is.na(data5$days_since_prior_order),]$days_since_prior_order <- 0
data <- data %>% inner_join(data5,by=c("user_id","product_id"))

rm(orders,data4,data5)
gc()

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_last_order_prod <- with(data,ifelse(up_last_order == user_orders,1,0)) # whether or not last order contains the product
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

# add reorder label from training set
data <- data %>% 
  left_join(ordert %>% dplyr::select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(ordert, prd, users)
gc()

dim(distinct(data[,c("user_id","product_id")]))
View(head(data,100))
fwrite(data,"../Processed Data/data_pre_model.csv",row.names = F)

###################### Variable Creation 2: Top Department / Aisle, DOW, HOD ##########################
rm(list=setdiff(ls(),"products"))
gc()

# read in top department / aisle datasets
data <- fread("../Processed Data/data_pre_model.csv")
top_depart <- read.csv("../Processed Data/top_3_department_user.csv")
top_aisle <- read.csv("../Processed Data/top_3_aisle_user.csv")

# merge user_id + product_id with top department / aisle to add indicator variable
top_depart_aisle <- data %>% select(user_id,product_id) %>% 
  left_join(products %>% select(product_id,aisle_id,department_id),by="product_id") %>%
  left_join(top_aisle,by="user_id") %>% 
  left_join(top_depart,by="user_id")

# add indicator variable
top_depart_aisle <- top_depart_aisle %>%
  dplyr::mutate(
    top_aisle_id_ind = ifelse(aisle_id == aisle_id.1 | aisle_id == aisle_id.2 | aisle_id == aisle_id.3 | aisle_id == aisle_id.4 
                            | aisle_id == aisle_id.5 | aisle_id == aisle_id.6 | aisle_id == aisle_id.7 | aisle_id == aisle_id.8
                            | aisle_id == aisle_id.9 | aisle_id == aisle_id.10 | aisle_id == aisle_id.11 | aisle_id == aisle_id.12
                            | aisle_id == aisle_id.13 | aisle_id == aisle_id.14 | aisle_id == aisle_id.15 | aisle_id == aisle_id.16
                            | aisle_id == aisle_id.17 | aisle_id == aisle_id.18 | aisle_id == aisle_id.19 | aisle_id == aisle_id.20
                            | aisle_id == aisle_id.21 | aisle_id == aisle_id.22 | aisle_id == aisle_id.23 | aisle_id == aisle_id.24,
                            1,0),
    top_depart_ind = ifelse(department_id == department_id.1 | department_id == department_id.2 | department_id == department_id.3
                            | department_id == department_id.4 | department_id == department_id.5 | department_id == department_id.6
                            | department_id == department_id.7 | department_id == department_id.8 | department_id == department_id.9
                            | department_id == department_id.10 | department_id == department_id.11 | department_id == department_id.12,
                            1,0))
# fill NA as 0
top_depart_aisle <- top_depart_aisle %>% select(user_id,product_id,top_aisle_id_ind,top_depart_ind)
top_depart_aisle[is.na(top_depart_aisle$top_aisle_id_ind),]$top_aisle_id_ind <- 0
top_depart_aisle[is.na(top_depart_aisle$top_depart_ind),]$top_depart_ind <- 0

# merge back with the master dataset
data <- data %>% inner_join(top_depart_aisle,by=c("user_id","product_id"))

rm(list=setdiff(ls(),"data"))
gc()

# read in HOD variables
orders_products_hour <- fread("../Processed Data/orders_products_hour.csv")

orders_products_hour$user_product_id <- paste(orders_products_hour$user_id,"_",orders_products_hour$product_id,sep="")

orders_products_hour_reshape <- data.table::dcast(setDT(orders_products_hour[,c("user_product_id","order_hour_of_day_grp","hr_pct","orders","total_orders")]),
                                            user_product_id~order_hour_of_day_grp,value.var=c("hr_pct","orders","total_orders"))

orders_products_hour_reshape <- orders_products_hour_reshape %>% 
  tidyr::separate(user_product_id,c("user_id","product_id"),"_")                                            

rm(orders_products_hour)
gc()

orders_products_hour_reshape$user_id <- as.numeric(orders_products_hour_reshape$user_id)
orders_products_hour_reshape$product_id <- as.numeric(orders_products_hour_reshape$product_id)

data <- data %>% inner_join(orders_products_hour_reshape,by=c("user_id","product_id"))

rm(list=setdiff(ls(),"data"))
gc()

# read in DOW dataset
orders_products_dow <- fread("../Processed Data/orders_products_dow.csv")

orders_products_dow$user_product_id <- paste(orders_products_dow$user_id,"_",orders_products_dow$product_id,sep="")

orders_products_dow_reshape <- data.table::dcast(setDT(orders_products_dow[,c("user_product_id","order_dow","dow_pct","orders","total_orders")]),
                                                  user_product_id~order_dow,value.var=c("dow_pct","orders","total_orders"))

orders_products_dow_reshape <- orders_products_dow_reshape %>% 
  tidyr::separate(user_product_id,c("user_id","product_id"),"_")                                            

rm(orders_products_dow)
gc()

orders_products_dow_reshape$user_id <- as.numeric(orders_products_dow_reshape$user_id)
orders_products_dow_reshape$product_id <- as.numeric(orders_products_dow_reshape$product_id)

# merge to the master dataset
# split train/test
train <- data[data$eval_set=="train",]
fwrite(train,"../Processed Data/data_pre_model_train_0715.csv")
test <- data[data$eval_set=="test",]
fwrite(test,"../Processed Data/data_pre_model_test_0715.csv")

rm(list=setdiff(ls(),"train"))
gc()

train <- train %>% left_join(orders_products_dow_reshape,by=c("user_id","product_id"))
fwrite(train,"../Processed Data/data_pre_model_train_0715.csv",row.names = F)

rm(train)
gc()

test <- fread("../Processed Data/data_pre_model_test_0715.csv")
test <- test %>% left_join(orders_products_dow_reshape,by=c("user_id","product_id"))
fwrite(test,"../Processed Data/data_pre_model_test_0715.csv",row.names = F)

rm(list=ls())
gc()

train <- fread("../Processed Data/data_pre_model_train_0715.csv")

# Train / Test datasets ---------------------------------------------------
train <- train %>% select(-eval_set,-user_id,-product_id,-order_id)
gc()
train[is.na(train)] <- 0
fwrite(train,"../Processed Data/data_pre_model_train_0715.csv",row.names = F)


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
trn_ind = createDataPartition(train$reordered,p=0.6,list=F)
train_trn = train[trn_ind,]
train_val = train[-trn_ind,]
rm(train)
gc()

X.trn <- xgb.DMatrix(as.matrix(train_trn %>% dplyr::select(-reordered)), label = train_trn$reordered)
model <- xgboost(data = X.trn, params = params, nrounds = 100)

importance <- xgb.importance(colnames(X.trn), model = model)
write.csv(importance,"../variable_importance_072001.csv",row.names = F)
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


# Apply model -------------------------------------------------------------
test <- fread("../Processed Data/data_pre_model_test_0715.csv")
test <- test %>% select(-eval_set,-user_id,-reordered)
test[is.na(test)] <- 0

X <- xgb.DMatrix(as.matrix(test %>% dplyr::select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)
hist(test$reordered)

fwrite(test,"../Processed Data/test_0715.csv",row.names = F)

test$reordered <- (test$reordered > 0.42) * 1 # need to stress test the threshold

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " "))

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None")

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"_042",".csv",
                                   sep = ""), row.names = F)
