######################### Stability Variables Creation ##################################
rm(list=ls())
gc()

library(data.table)
library(dplyr)
setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")
orderp <- fread("order_products__prior.csv/order_products__prior.csv") # order_id and product_id level
ordert <- fread("order_products__train.csv/order_products__train.csv") # order_id level
orders <- fread("orders.csv/orders.csv") # order_id level

# Map user_id to order train dataset
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)] # match the user_id for ordert table
# Merge user_id, order information to order_prior dataset, only keep prior orders to build variables
orders_products <- orders %>% inner_join(orderp, by = "order_id")

# Variable 1: For each user, calculate the std of order quantity for each order
order_quantity <- orders_products %>% group_by(user_id, order_id) %>%
  dplyr::summarize(order_quantity = max(add_to_cart_order,na.rm = T))

order_quantity_std <- order_quantity %>% group_by(user_id) %>%
  dplyr::summarize(user_order_quantity_sd = sd(order_quantity,na.rm = T))

fwrite(order_quantity_std,"../Processed Data/order_quantity_std.csv",row.names = F)

# Variable 2: For each user by product, calculate the std of order quantity for each order containing the product
orders_products <- orders_products %>% group_by(user_id, order_id) %>%
  dplyr::mutate(order_quantity = max(add_to_cart_order,na.rm = T))

order_prod_quantity_std <- orders_products %>% group_by(user_id,product_id) %>%
  dplyr::summarize(user_prod_order_quantity_sd = sd(order_quantity,na.rm = T))

fwrite(order_prod_quantity_std,"../Processed Data/order_prod_quantity_std.csv",row.names = F)


# Variable 3: Standard deviation of add_to_cart position
# Variable 4: Standard deviation of add_to_cart percentage
user_prod_addToCart_sd <- orders_products %>% 
  dplyr::mutate(add_to_cart_pct = add_to_cart_order / order_quantity) %>% group_by(user_id,product_id) %>%
  dplyr::summarize(user_prod_addToCart_sd = sd(add_to_cart_order,na.rm = T),
                   user_prod_addToCart_pct_sd = sd(add_to_cart_pct,na.rm = T))
fwrite(user_prod_addToCart_sd,"../Processed Data//user_prod_addToCart_sd.csv",row.names = F)  

# Variable 5: Standard deviation of user purchase interval
user_interval_sd <- orders %>% group_by(user_id) %>%
  dplyr::summarize(user_interval_sd = sd(days_since_prior_order,na.rm = T))
fwrite(user_interval_sd,"../Processed Data/user_interval_sd.csv",row.names = F)

# Variable 6: Average days interval by user and product since first time ordering the product
orders[is.na(orders$days_since_prior_order),]$days_since_prior_order <- 0
orders <- orders %>% arrange(user_id,order_number) %>% group_by(user_id) %>%
  dplyr::mutate(days_since_prior_order_cum = cumsum(days_since_prior_order))

orders_products <- orders_products %>% left_join(orders %>% select(user_id,order_id,days_since_prior_order_cum),
                                                 by=c("user_id","order_id"))
orders_products <- orders_products %>% arrange(user_id,product_id,order_number)

user_prod_interval <- orders_products %>% group_by(user_id,product_id) %>%
  dplyr::summarize(up_avg_interval = (max(days_since_prior_order_cum) - min(days_since_prior_order_cum)) / n())
fwrite(user_prod_interval,"../Processed Data/user_prod_interval.csv",row.names = F)

# Variable 7: Standard deviation of user product purchase interval
orders_products <- orders_products %>% select(user_id,product_id,order_number,days_since_prior_order_cum)
orders_products <- orders_products %>% group_by(user_id,product_id) %>%
  dplyr::mutate(days_since_prior_order_cum_lag = dplyr::lag(days_since_prior_order_cum,n=1,default = NA))
orders_products$up_days_interval <- orders_products$days_since_prior_order_cum - orders_products$days_since_prior_order_cum_lag

up_days_interval_sd <- orders_products %>% group_by(user_id,product_id) %>%
  dplyr::summarize(up_days_interval_sd = sd(up_days_interval,na.rm = T))
fwrite(up_days_interval_sd,"../Processed Data/up_days_interval_sd.csv",row.names = F)


##################### Compile all metrics ##########################
rm(list=ls())
gc()

order_quantity_std <- fread("../Processed Data/order_quantity_std.csv")
user_interval_sd <- fread("../Processed Data/user_interval_sd.csv")
order_prod_quantity_std <- fread("../Processed Data/order_prod_quantity_std.csv")
user_prod_addToCart_sd <- fread("../Processed Data//user_prod_addToCart_sd.csv")  
user_prod_interval <- fread("../Processed Data/user_prod_interval.csv")
up_days_interval_sd <- fread("../Processed Data/up_days_interval_sd.csv")

# user_prod_order_quantity_sd, if only one order made by a user for a product, sd is missing, impute them as separate group
order_prod_quantity_std$up_order_quantity_sd_grp <- 
  ifelse(order_prod_quantity_std$user_prod_order_quantity_sd < quantile(order_prod_quantity_std$user_prod_order_quantity_sd,na.rm = T,probs = 0.25),1,
         ifelse(order_prod_quantity_std$user_prod_order_quantity_sd < quantile(order_prod_quantity_std$user_prod_order_quantity_sd,na.rm = T,probs = 0.5),2,
                ifelse(order_prod_quantity_std$user_prod_order_quantity_sd < quantile(order_prod_quantity_std$user_prod_order_quantity_sd,na.rm = T,probs = 0.75),3,4)))
order_prod_quantity_std[is.na(order_prod_quantity_std$up_order_quantity_sd_grp),]$up_order_quantity_sd_grp <- 5

# user_prod_addToCart_sd, if only one order made by a user for a product, sd is missing, impute them as separate group
user_prod_addToCart_sd$up_addToCart_sd_grp <- 
  ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.25),1,
         ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.5),2,
                ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.75),3,4)))
user_prod_addToCart_sd$up_addToCart_sd_grp <- ifelse(is.na(user_prod_addToCart_sd$up_addToCart_sd_grp),5,
                                                         user_prod_addToCart_sd$up_addToCart_sd_grp)

user_prod_addToCart_sd$up_addToCart_pct_sd_grp <- 
  ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.25),1,
         ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.5),2,
                ifelse(user_prod_addToCart_sd$user_prod_addToCart_pct_sd < quantile(user_prod_addToCart_sd$user_prod_addToCart_pct_sd,na.rm = T,probs = 0.75),3,4)))
user_prod_addToCart_sd$up_addToCart_pct_sd_grp <- ifelse(is.na(user_prod_addToCart_sd$up_addToCart_pct_sd_grp),5,
                                                         user_prod_addToCart_sd$up_addToCart_pct_sd_grp)

# up_days_interval_sd, if only two orders containing the products being made by user, the average interval sd is missing, impute as separate group
up_days_interval_sd$up_days_interval_sd_grp <- 
  ifelse(up_days_interval_sd$up_days_interval_sd < quantile(up_days_interval_sd$up_days_interval_sd,na.rm = T,probs = 0.25),1,
         ifelse(up_days_interval_sd$up_days_interval_sd < quantile(up_days_interval_sd$up_days_interval_sd,na.rm = T,probs = 0.5),2,
                ifelse(up_days_interval_sd$up_days_interval_sd < quantile(up_days_interval_sd$up_days_interval_sd,na.rm = T,probs = 0.75),3,4)))
up_days_interval_sd$up_days_interval_sd_grp <- ifelse(is.na(up_days_interval_sd$up_days_interval_sd_grp),5,
                                                         up_days_interval_sd$up_days_interval_sd_grp)

# merge all together
stability <- order_prod_quantity_std %>% inner_join(up_days_interval_sd,by=c("user_id","product_id")) %>%
  inner_join(user_prod_addToCart_sd,by=c("user_id","product_id")) %>% 
  inner_join(user_prod_interval,by=c("user_id","product_id")) %>% 
  left_join(order_quantity_std,by="user_id") %>%
  left_join(user_interval_sd,by="user_id")
stability <- stability %>% select(-user_prod_order_quantity_sd,-up_days_interval_sd,-user_prod_addToCart_sd,-user_prod_addToCart_pct_sd)
summary(stability)
fwrite(stability,"../Processed Data/stability_metrics.csv",row.names = F)








