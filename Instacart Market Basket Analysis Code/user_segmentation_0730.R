################################ Instacart Market Basket Analysis #####################################
# User Segmentation, pre-modeling process
# The goal is to segment the users based on purchase behaviors and apply the model separately to predict the basket

rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape)
library(caret)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/")

# Source the data loading R code
source("Instacart Market Basket Analysis Code/data_load.R")


#################### Segmentation Variables Creation #######################
# merge products data with aisle and department
products <- products %>% left_join(department,by="department_id") %>% left_join(aisle,by="aisle_id")

# merge order_product_prior with orders
order_products_prior <- order_products_prior %>% left_join(products %>% select(product_id,department),by="product_id") %>%
  left_join(orders %>% select(user_id,order_id),by="order_id")

order_product_prior_agg <- order_products_prior %>% group_by(order_id) %>% dplyr::summarize(
  order_products = max(add_to_cart_order,na.rm = T),
  order_reordered = sum(reordered,na.rm = T))

# User level metrics
user <- orders %>% filter(eval_set %in% c("prior")) %>% 
  inner_join(order_product_prior_agg,by="order_id") %>% 
  group_by(user_id) %>% dplyr::summarize(
  user_total_orders = max(order_number,na.rm = T),
  user_tenure = sum(days_since_prior_order,na.rm = T),
  user_avg_products = mean(order_products,na.rm=T),
  reorder_ratio = sum(order_reordered[order_number!=1],na.rm = T)/sum(order_products[order_number!=1],na.rm = T)) %>% 
  ungroup() %>% dplyr::mutate(
  user_avg_days_since_prior = user_tenure / (user_total_orders-1))

# User level department metrics
user_depart <- order_products_prior %>% group_by(user_id,department) %>% dplyr::summarize(count = n()) %>%
  ungroup() %>% group_by(user_id) %>% dplyr::mutate(product_depart_ratio = count/sum(count,na.rm = T))
user_depart <- cast(user_depart %>% select(user_id,department,product_depart_ratio),user_id~department)
user_depart[is.na(user_depart)] <- 0

user <- user %>% inner_join(user_depart)
summary(user)
fwrite(user,"../Processed Data/user_segmentation.csv",row.names = F)

################################ Clustering ########################################
# Build clustering
set.seed(1)
user_clust <- kmeans(scale(user[,2:27]),centers = 3,nstart = 50,iter.max = 20)
user$cluster <- user_clust$cluster
table(user$cluster)

# Summary statistics
user_summary <- user %>% group_by(cluster) %>% dplyr::summarize(count=n()) %>% ungroup() %>% inner_join(
  user %>% group_by(cluster) %>% dplyr::summarise_all(funs(mean(.,na.rm = T))) %>% 
  select(-user_id),by="cluster")
fwrite(user_summary,"~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/summary_clust3.csv",row.names = F)
fwrite(user %>% select(user_id,cluster),"~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/user_clust3.csv")


# Build clustering
set.seed(2)
user_clust <- kmeans(scale(user[,2:27]),centers = 5,nstart = 50,iter.max = 20)
user$cluster <- user_clust$cluster
table(user$cluster)

# Summary statistics
user_summary <- user %>% group_by(cluster) %>% dplyr::summarize(count=n()) %>% ungroup() %>% inner_join(
  user %>% group_by(cluster) %>% dplyr::summarise_all(funs(mean(.,na.rm = T))) %>% 
    select(-user_id),by="cluster")
fwrite(user_summary,"~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/summary_clust5.csv",row.names = F)
fwrite(user %>% select(user_id,cluster),"~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/user_clust5.csv")



################################ Variable Importace #####################################
# create cluster label
user$cluster_1 <- ifelse(user$cluster==1,"Cluster1","Other")
user$cluster_2 <- ifelse(user$cluster==2,"Cluster2","Other")
user$cluster_3 <- ifelse(user$cluster==3,"Cluster3","Other")
user$cluster_4 <- ifelse(user$cluster==4,"Cluster4","Other")
user$cluster_5 <- ifelse(user$cluster==5,"Cluster5","Other")

# build decision tree model to generate variable importance
control = trainControl(method="cv",number=5,classProbs = T)

# Cluster 1
mod_clust1 <- train(as.factor(user$cluster_1)~.,
                   data=user[,c(2:27)],
                   method="rf",
                   trControl = control,
                   tuneLength=3)











