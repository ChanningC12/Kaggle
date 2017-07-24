# Explore the raw datasets
rm(list=ls())
gc()

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

library(data.table)

# 1. aisle.csv, aisle id and category description
# 134 obs, 2 columns
aisle <- fread("aisles.csv/aisles.csv",showProgress = T)
str(aisle)

# 2. department.csv, department id and description
# 21 obs, 2 columns
department <- fread("departments.csv/departments.csv",showProgress = T)
str(department)

# 3. order_products.csv, item level order information
# 32,434,489 obs, 4 columns. Order information, order_id, product_id, add_to_cart_order, reordered
order_products_prior <- fread("order_products__prior.csv/order_products__prior.csv",showProgress = T)
str(order_products_prior)
View(head(order_products_prior,1000))

# 4. order_products_train.csv, item level order information
# 1,384,617 obs, 4 columns. Same structure as above
order_products_train <- fread("order_products__train.csv/order_products__train.csv",showProgress = T)
str(order_products_train)

# 5. orders.csv, order level order information
# 3,421,083 obs, 7 columns. order_id, user_id, eval_set (prior, train, test), order_num, order_dow (day of week), order_hour_of_day, days_since_prior_order
orders <- fread("orders.csv/orders.csv",showProgress = T)
str(orders)
View(head(orders,1000))
table(orders$eval_set)

# 6. products.csv, product information
# 49,688 obs, 4 columns. product_id, product_name, aisle_id, department_id
products <- read.csv("products.csv/products.csv")
str(products)

# 7. sample submission
sample_submit <- fread("sample_submission.csv",showProgress = T)
str(sample_submit)








