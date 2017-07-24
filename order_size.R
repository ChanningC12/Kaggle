rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(ggplot2)
library(caret)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

# Source the data loading R code
source("../data_load.R")

################################### Predict Order Size #########################################
# the approach is similar to persuasion modeling that for a specific user, given dow, hour, and days since last order,
# to determine the average order size, give more weights to the recent orders

# data structure should be at user_id and multiple conditions level

# 1. determine the order size for each order
order_products_prior$eval_set <- "prior"
order_products_train$eval_set <- "train"

# combine the prior and train datasets
order_products_prior_train <- rbind(order_products_prior,order_products_train)

# determine the order size for each order
order_size <- order_products_prior_train %>% group_by(order_id) %>%
  dplyr::summarize(
    order_size = max(add_to_cart_order),
    eval_set = max(eval_set)
  )

# merge the order size with the orders
# split prior+train and test
orders_prior_train <- orders[orders$eval_set %in% c("prior","train"),]
orders_test <- orders[orders$eval_set=="test",]

orders_prior_train <- merge(orders_prior_train, order_size, by=c("order_id","eval_set"))

# cut the order_dow, order_hour_of_day, days_since_prior_order into intervals
# check the distribution of each columns
# day of week
quantile(orders_prior_train$order_dow,probs = seq(0,1,by = 0.1))
ggplot(orders_prior_train,aes(order_dow)) + geom_histogram()
orders_prior_train$dow_grp <- with(orders_prior_train,
                                    ifelse(order_dow==0 | order_dow>=5, "WE","WD"))
table(orders_prior_train$dow_grp,orders_prior_train$order_dow)

# hour of day
quantile(orders_prior_train$order_hour_of_day,probs = seq(0,1,by = 0.1))
ggplot(orders_prior_train,aes(order_hour_of_day)) + geom_histogram()

orders_prior_train$hour_grp <- with(orders_prior_train,
                                    ifelse(order_hour_of_day<6,"N",
                                           ifelse(order_hour_of_day<12,"M",
                                                  ifelse(order_hour_of_day<18,"A","E"))))
table(orders_prior_train$hour_grp,orders_prior_train$order_hour_of_day)

# days since prior order
quantile(orders_prior_train[!(is.na(orders_prior_train$days_since_prior_order)),]$days_since_prior_order,probs = seq(0,1,by = 0.1))
ggplot(orders_prior_train,aes(days_since_prior_order)) + geom_histogram()

orders_prior_train$days_prior_grp <- with(orders_prior_train,
                                    ifelse(days_since_prior_order==0,"0",
                                           ifelse(days_since_prior_order<=3,"1-3",
                                                  ifelse(days_since_prior_order<=6,"4-6",
                                                         ifelse(days_since_prior_order==7,"7",
                                                                ifelse(days_since_prior_order<14,"8-13",
                                                                       ifelse(days_since_prior_order==14,"14",
                                                                              ifelse(days_since_prior_order<30,"15-29","30"))))))))
table(orders_prior_train$days_prior_grp,orders_prior_train$days_since_prior_order)


# group by dow, hours and days since prior order
orders_size_avg <- orders_prior_train %>% 
  group_by_(.dots = c("user_id","dow_grp","hour_grp","days_prior_grp")) %>%
  dplyr::summarize(
    order_size_avg = mean(order_size),
    order_count = n()
  )


# match the order to the test data
orders_test$dow_grp <- with(orders_test,
                                   ifelse(order_dow==0 | order_dow>=5, "WE","WD"))
orders_test$hour_grp <- with(orders_test,
                                    ifelse(order_hour_of_day<6,"N",
                                           ifelse(order_hour_of_day<12,"M",
                                                  ifelse(order_hour_of_day<18,"A","E"))))
orders_test$days_prior_grp <- with(orders_test,
                                          ifelse(days_since_prior_order==0,"0",
                                                 ifelse(days_since_prior_order<=3,"1-3",
                                                        ifelse(days_since_prior_order<=6,"4-6",
                                                               ifelse(days_since_prior_order==7,"7",
                                                                      ifelse(days_since_prior_order<14,"8-13",
                                                                             ifelse(days_since_prior_order==14,"14",
                                                                                    ifelse(days_since_prior_order<30,"15-29","30"))))))))

orders_test <- merge(orders_test, orders_size_avg[,c(1:5)], by=names(orders_size_avg)[1:4], all.x=T)


# 2. decision tree to determine the pattern of order size
orders_prior_train[is.na(orders_prior_train$days_prior_grp),]$days_prior_grp <- "first"
str(orders_prior_train)
orders_prior_train <- orders_prior_train %>% 
  dplyr::mutate_if(is.character,as.factor)
orders_prior_train$user_id <- as.factor(orders_prior_train$user_id)


## Decision Tree ##
orders_train <- orders_prior_train[orders_prior_train$eval_set=="train",]
control = trainControl(method="cv",number=3)
set.seed(060201)
system.time(mod_rpart <- train(order_size~user_id+dow_grp+hour_grp+days_prior_grp,
                               data=orders_train,method="rpart",
                               trControl = control))
mod_rpart








