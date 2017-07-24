rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)

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

# Merge aisle, department information with products
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  dplyr::select(-aisle_id, -department_id)
rm(aisles, departments)

# Map user_id to order train dataset
n_distinct(orders$order_id) # 3,421,083
n_distinct(orders[orders$eval_set=="train",]$order_id) # 131,209
n_distinct(ordert$order_id) # 131,209
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

# Merge user_id, order information to order_prior dataset, only keep prior orders
orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()

View(head(orders_products,100))



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
    prod_second_orders = sum(product_time == 2)
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders # same as average products purchased by user who made the purchase
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% dplyr::select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()


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
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

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

# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
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
  "alpha"               = 2e-05, #L1 regularization
  "lambda"              = 10 # L2 regularization
)

# subtrain <- train %>% sample_frac(0.5)
X <- xgb.DMatrix(as.matrix(train %>% dplyr::select(-reordered)), label = train$reordered)
system.time(model <- xgboost(data = X, params = params, nrounds = 80))

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance)
gc()


# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% dplyr::select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability
summary(test$reordered)

fwrite(test,"../Processed Data/test_0708.csv",row.names = F)

test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  dplyr::summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = paste("../Submission/submit_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""), row.names = F)
