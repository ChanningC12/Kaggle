rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape)

setwd("~/../Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Raw Data/")

# Source the data loading R code
source("../data_load.R")

################################### User Segmentation #########################################

# 1. Pre-Processing
order_products_prior_train <- merge(order_products_prior_train, orders_prior_train, by="order_id", all.x=T)
order_products_prior_train <- order_products_prior_train[,c("user_id","order_id","product_id")]

# product level data with user_id, order_id and department_id
order_products_prior_train <- merge(order_products_prior_train, products[,c("product_id","department_id")],
                                    by="product_id",all.x=T)
n_distinct(order_products_prior_train$product_id)
n_distinct(order_products_prior_train$user_id)
n_distinct(order_products_prior_train$order_id)
n_distinct(order_products_prior_train$department_id)

order_pre_kmean <- order_products_prior_train %>% group_by_(.dots = c("user_id","department_id")) %>% 
  dplyr::summarize(
    count = n()
  )

order_kmean <- cast(order_pre_kmean,user_id~department_id)
order_kmean[is.na(order_kmean)] <- 0
fwrite(order_kmean,"../Processed Data/user_kmeans.csv",row.names = F)

# 2. Kmeans
order_kmean <- fread("../Processed Data/user_kmeans.csv",header = T)
names(order_kmean) <- c("user_id",department$department)

# Determine the number of clusters
set.seed(060301)
k.max <- 15
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(order_kmean[,2:ncol(order_kmean)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kmeans_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(kmeans_wss,paste("../Processed Data/wss_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Assign k=5 for clustering analysis
k1=5

# K-Means Cluster Analysis
set.seed(060401)
fit_kmean <- kmeans(as.matrix(order_kmean[,2:ncol(order_kmean)]),centers = k1,nstart=50,iter.max=15)
fit_kmean

# append cluster assignment
order_kmean_cluster <- data.frame(order_kmean, cluster = fit_kmean$cluster)
table(order_kmean_cluster$cluster)









