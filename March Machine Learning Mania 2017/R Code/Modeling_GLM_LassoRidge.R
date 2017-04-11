#################### GLM Model with Stepwise, Ridge and Lasso ########################
rm(list=ls())
gc()
getwd()
# setwd()

library(data.table)
library(dplyr)
library(DataCombine)
library(caret)
library(ROCR)
library(MASS)
library(glmnet)

modeling_glm = read.csv("modeling_glm_0213.csv")

predictors = as.matrix(modeling_glm[,3:32])
target = as.double(as.matrix(modeling_glm[,34]))

# Ridge Regression
set.seed(999)
system.time(mod_ridge <- cv.glmnet(predictors,target,family="binomial",
                                   alpha=0,parallel=T,standardize=T,type.measure="auc"))
plot(mod_ridge)
mod_ridge$lambda.min
mod_ridge$lambda.1se
coef(mod_ridge, s=mod_ridge$lambda.min)

# predict by hand, we need to add the constant 1 to be associated with the constant coefficient of the linear mod
ridge_pred <- as.matrix(cbind(const=1,modeling_glm[,3:32])) %*% coef(mod_ridge)
class(ridge_pred)
summ = summary(ridge_pred)
str(ridge_pred)
ridge_pred = data.frame(id = modeling_glm$id, pred = ridge_pred@x, 
                        prob = exp(ridge_pred@x)/(1+exp(ridge_pred@x)))

# Lasso Regression
set.seed(999)
system.time(mod_lasso <- cv.glmnet(predictors,target,family="binomial",
                                   alpha=1,parallel=T,standardize=T,type.measure="auc"))
plot(mod_lasso)
mod_lasso$lambda.min
mod_lasso$lambda.1se
coef(mod_lasso, s=mod_lasso$lambda.min)

# predict by hand, we need to add the constant 1 to be associated with the constant coefficient of the linear mod
lasso_pred <- as.matrix(cbind(const=1,modeling_glm[,3:32])) %*% coef(mod_lasso)
class(lasso_pred)
summ = summary(lasso_pred)
str(lasso_pred)
lasso_pred = data.frame(id = modeling_glm$id, pred = lasso_pred@x, 
                        prob = exp(lasso_pred@x)/(1+exp(lasso_pred@x)))


