# User + Products ---------------------------------------------------------
######################### Stability Variables Creation ##################################
rm(list=ls())
gc()

library(data.table)
library(dplyr)
setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")
orderp <- fread("order_products__prior.csv/order_products__prior.csv") # order_id and product_id level
ordert <- fread("order_products__train.csv/order_products__train.csv") # order_id level
orders <- fread("orders.csv/orders.csv") # order_id level
aisles <- fread("aisles.csv/aisles.csv", key = "aisle_id")
departments <- fread("departments.csv/departments.csv", key = "department_id")
products <- fread("products.csv/products.csv", key = c("product_id","aisle_id", "department_id")) 

# Clean data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
products$product_name <- as.factor(products$product_name)

# Merge aisle, department information with products into one table
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments)
rm(aisles, departments)

# Map user_id to order train dataset
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)] # match the user_id for ordert table
# Merge user_id, order information to order_prior dataset, only keep prior orders to build variables
orders_products <- orders %>% inner_join(orderp, by = "order_id")

# user + product metrics definition
# 1. product_department
# 2. product_aisle
orders_products <- orders_products %>% inner_join(products[,c("product_id","aisle_id","department_id")], by="product_id")

# 3. aisle_top_3_ind and department_top_3_ind:

# aisle count table
orders_products_aisle <- orders_products %>% group_by_(.dots = c("user_id","aisle_id")) %>% 
  dplyr::summarize(
    aisle_count = n()) %>% arrange(user_id,-aisle_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::top_n(3,wt=aisle_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>% 
  dplyr::mutate(rank = row_number())

# Reshape the data
class(orders_products_aisle)
orders_products_aisle <- as.data.frame(orders_products_aisle)
orders_products_aisle_reshape <- reshape(orders_products_aisle[,c("user_id","aisle_id","rank")],direction = "wide",
                                         idvar = "user_id", timevar = "rank", ids = "aisle_id")
fwrite(orders_products_aisle_reshape,"../Processed Data/top_3_aisle_user.csv",row.names = F)

# department count table
orders_products_department <- orders_products %>% group_by_(.dots = c("user_id","department_id")) %>% 
  dplyr::summarize(
    department_count = n()) %>% arrange(user_id,-department_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::top_n(3,wt=department_count) %>% ungroup() %>% group_by_(.dots = c("user_id")) %>%
  dplyr::mutate(rank = row_number())

# Reshape the data
class(orders_products_department)
orders_products_department <- as.data.frame(orders_products_department)
orders_products_department_reshape <- reshape(orders_products_department[,c("user_id","department_id","rank")],direction = "wide",
                                              idvar = "user_id", timevar = "rank", ids = "department_id")
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
    dow_pct = orders / total_orders
  )
fwrite(orders_products_dow,"../Processed Data/orders_products_dow.csv",row.names = F)

# Clean up all metrics to be on user_id and product_id level ------------------------------
# read in top department / aisle datasets
data <- fread("../Processed Data/data_pre_model.csv",select = 1:2)
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

orders_products_hour_reshape <- data.table::dcast(setDT(orders_products_hour[,c("user_product_id","order_hour_of_day_grp","hr_pct","orders")]),
                                                  user_product_id~order_hour_of_day_grp,value.var=c("hr_pct","orders"))

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

orders_products_dow_reshape <- data.table::dcast(setDT(orders_products_dow[,c("user_product_id","order_dow","dow_pct","orders")]),
                                                 user_product_id~order_dow,value.var=c("dow_pct","orders"))

orders_products_dow_reshape <- orders_products_dow_reshape %>% 
  tidyr::separate(user_product_id,c("user_id","product_id"),"_")                                            

rm(orders_products_dow)
gc()

orders_products_dow_reshape$user_id <- as.numeric(orders_products_dow_reshape$user_id)
orders_products_dow_reshape$product_id <- as.numeric(orders_products_dow_reshape$product_id)

data <- data %>% inner_join(orders_products_dow_reshape,by=c("user_id","product_id"))
fwrite(data,"../Processed Data/top_aisle_depart_hod_dow_metrics.csv",row.names = F)














