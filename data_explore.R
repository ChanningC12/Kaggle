rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(ggplot2)
library(treemap)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

# Source the data loading R code
source("../data_load.R")

################################### Univariate Data Exploration #########################################
# 1-3. Product, Aisle and Department information
n_distinct(aisle$aisle) # 134 distinct aisle name
n_distinct(department$department) # 21 distinct department name
n_distinct(products$product_name) # 49,688 distinct product name

# merge the aisle, department and products dataset
products_all <- merge(products,aisle,by="aisle_id",all.x=T)
products_all <- merge(products_all,department,by="department_id",all.x=T)

# distribution of aisle, department
dev.off()
fwrite(as.data.frame(table(products_all$aisle)[order(table(products_all$aisle),decreasing = T)]),
       "../Exploratory Result/product_by_aisle.csv",showProgress = F,row.names = T)
ggplot(products_all,aes(aisle)) + geom_bar()

fwrite(as.data.frame(table(products_all$department)[order(table(products_all$department),decreasing = T)]),
       "../Exploratory Result/product_by_department.csv",showProgress = F,row.names = T)
ggplot(products_all,aes(department)) + geom_bar()

# Relation between aisle and department
aisle_department <- products_all %>% group_by_(.dots = c("department","aisle")) %>%
  dplyr::summarize(
    count = n()
  )

aisle_department <- aisle_department %>% group_by(department) %>%
  dplyr::mutate(
    count_department = sum(count)
  )

fwrite(as.data.frame(table(aisle_department$department)[order(table(aisle_department$department),decreasing = T)]),
       "../Exploratory Result/aisle_by_department.csv",showProgress = F,row.names = T)

# visualize product portfolio
treemap::treemap(aisle_department,index="department",vSize="count_department",
                 title="Unique Products Offered in each Department/Aisle",
                 palette="Set3",border.col="#FFFFFF")

treemap::treemap(aisle_department,index=c("department","aisle"),vSize="count",
                 title="Unique Products Offered in each Department/Aisle",
                 palette="Set3",border.col="#FFFFFF")


# 4-5. How many unique customers / orders?
n_distinct(orders$order_id) # 3,421,083 unique orders
n_distinct(orders$user_id) # 206,209 unique customers

# 6. Average order size, distribution of order size
# prior
order_prior <- order_products_prior %>% group_by(order_id) %>%
  dplyr::summarize(
    order_size = max(add_to_cart_order),
    reorder = mean(reordered,na.rm = T)
  )
mean(order_prior$order_size,na.rm=T)
ggplot(order_prior,aes(order_size)) + geom_histogram()

# train
order_train <- order_products_train %>% group_by(order_id) %>%
  dplyr::summarize(
    order_size = max(add_to_cart_order),
    reorder = mean(reordered,na.rm = T)
  )
mean(order_train$order_size,na.rm=T)
ggplot(order_train,aes(order_size)) + geom_histogram()


# 7. Distribution of order size by order_num
# combine prior and train order size summary
order_prior_train <- rbind(order_prior,order_train)
orders <- merge(orders,order_prior_train,by="order_id",all=T)

order_num_size <- orders %>% group_by_(.dots = c("eval_set","order_number")) %>%
  dplyr::summarize(
    order_size = mean(order_size,na.rm=T),
    order_count = n()
  )

ggplot(order_num_size[order_num_size$eval_set %in% c("train","prior")], 
       aes(x=order_number, y=order_size, colour=eval_set)) + geom_line() + ylim(0,20)

# 8. Distribution of order by day of the week
order_dow_size <- orders %>% group_by_(.dots = c("eval_set","order_dow")) %>%
  dplyr::summarize(
    order_size = mean(order_size,na.rm=T),
    order_count = n()
  )

ggplot(order_dow_size[order_dow_size$eval_set %in% c("train","prior")], 
       aes(x=order_dow, y=order_size, colour=eval_set)) + geom_line() + ylim(0,20)

# 9. Distribution of order by hour of the day
order_hod_size <- orders %>% group_by_(.dots = c("eval_set","order_hour_of_day")) %>%
  dplyr::summarize(
    order_size = mean(order_size,na.rm=T),
    order_count = n()
  )

ggplot(order_hod_size[order_hod_size$eval_set %in% c("train","prior")], 
       aes(x=order_hour_of_day, y=order_size, colour=eval_set)) + geom_line() + ylim(0,20)


# 10. Average day since prior order
summary(orders$days_since_prior_order,na.rm=T)
table(orders$days_since_prior_order)
ggplot(orders,aes(days_since_prior_order)) + geom_histogram()
View(head(orders[orders$days_since_prior_order==30,],10000))

# 11. Average day since prior order by order number
order_dspo_num <- orders %>% group_by_(.dots = c("eval_set","order_number")) %>%
  dplyr::summarize(
    days_since_prior_order = mean(days_since_prior_order,na.rm=T),
    order_count = n()
  )

ggplot(order_dspo_num, aes(x=order_number, y=days_since_prior_order, colour=eval_set)) + geom_line()

# 12. Top products added to the cart
order_products_prior_agg <- order_products_prior %>% group_by(product_id) %>%
  dplyr::summarize(
    product_count = n(),
    product_order_avg = mean(add_to_cart_order)
  )

# 13. Average percentage of reordering in each order
orders[orders$eval_set %in% c("train","prior")] %>% group_by(eval_set) %>%
  dplyr::summarize(
    reorder = mean(reorder,na.rm = T)
  )

ggplot(orders[orders$eval_set %in% c("train","prior")]) + 
  geom_bar(aes(x=eval_set, y=reorder, fill=as.factor(eval_set)), position = "dodge", stat = "summary", fun.y="mean")
       
# 14. Average percentage of reordering in each order by order number
reorder_order_num <- orders[orders$eval_set %in% c("train","prior")] %>% group_by_(.dots = c("eval_set","order_number")) %>%
  dplyr::summarize(
    reorder = mean(reorder,na.rm = T)
  )

ggplot(reorder_order_num, aes(x=order_number, y=reorder, colour=eval_set)) + geom_line()
















