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

# Reshape data ------------------------------------------------------------
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

# User + Products ---------------------------------------------------------
# user + product metrics definition
# 1. product_department
# 2. product_aisle
orders_products <- orders_products %>% inner_join(products[,c("product_id","aisle","department")], by="product_id")

# 3. aisle_top_3_ind and department_top_3_ind:

# aisle count table
orders_products_aisle <- orders_products %>% group_by_(.dots = c("user_id","aisle")) %>% 
  dplyr::summarize(
    aisle_count = n()) %>% arrange(user_id,-aisle_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::top_n(3,wt=aisle_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>% 
  dplyr::mutate(rank = row_number())

# Reshape the data
class(orders_products_aisle)
orders_products_aisle <- as.data.frame(orders_products_aisle)
orders_products_aisle_reshape <- reshape(orders_products_aisle[,c("user_id","aisle","rank")],direction = "wide",
                                         idvar = "user_id", timevar = "rank", ids = "aisle")
fwrite(orders_products_aisle_reshape,"../Processed Data/top_3_aisle_user.csv",row.names = F)

# department count table
orders_products_department <- orders_products %>% group_by_(.dots = c("user_id","department")) %>% 
  dplyr::summarize(
    department_count = n()) %>% arrange(user_id,-department_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::top_n(3,wt=department_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::mutate(rank = row_number())

# Reshape the data
class(orders_products_department)
orders_products_department <- as.data.frame(orders_products_department)
orders_products_department_reshape <- reshape(orders_products_department[,c("user_id","department","rank")],direction = "wide",
                                         idvar = "user_id", timevar = "rank", ids = "department")
fwrite(orders_products_department_reshape,"../Processed Data/top_3_department_user.csv",row.names = F)


# Day of Week and Hour of Day variables -----------------------------------
# product and user level metrics
# 1. % of purchase on each day
# 2. % of purchase on hour of the day, 4 groups, 0-5, 6-11, 12-17, 18-23
table(orders_products$order_dow)
table(orders_products$order_hour_of_day)

# group the hours
orders_products <- orders_products %>% 
  dplyr::mutate(order_hour_of_day_grp = ifelse(order_hour_of_day<6,"N",
                                                ifelse(order_hour_of_day<12,"M",
                                                       ifelse(order_hour_of_day<18,"A","E"))))

# orders by hour of the day
orders_products_hour <- orders_products %>% group_by_(.dots = c("user_id","product_id","order_hour_of_day_grp")) %>%
  dplyr::summarize(
    orders = n()
  ) %>% ungroup() %>% group_by_(.dots = c("user_id","product_id")) %>% 
  dplyr::mutate(
    total_orders = sum(orders)
  ) %>% dplyr::mutate(
    hr_pct = orders / total_orders
  )
fwrite(orders_products_hour,"../Processed Data/orders_products_hour.csv",row.names = F)

# orders by day of the week
orders_products_dow <- orders_products %>% group_by_(.dots = c("user_id","product_id","order_dow")) %>%
  dplyr::summarize(
    orders = n()
  ) %>% ungroup() %>% group_by_(.dots = c("user_id","product_id")) %>% 
  dplyr::mutate(
    total_orders = sum(orders)
  ) %>% dplyr::mutate(
    hr_pct = orders / total_orders
  )
fwrite(orders_products_dow,"../Processed Data/orders_products_dow.csv",row.names = F)



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
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
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

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders # whether or not last order contains the product
data$up_orders_since_last_order <- data$user_orders - data$up_last_order # also get the days between these two
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% dplyr::select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(ordert, prd, users)
gc()

dim(distinct(data[,c("user_id","product_id")]))
View(head(data,100))

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

subtrain <- train %>% sample_frac(0.5)
X <- xgb.DMatrix(as.matrix(train %>% dplyr::select(-reordered)), label = train$reordered)
system.time(model <- xgboost(data = X, params = params, nrounds = 80))

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% dplyr::select(-order_id, -product_id)))
test$reordered <- predict(model, X) # get the probability

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
write.csv(submission, file = "../Submission/submit_062801.csv", row.names = F)
