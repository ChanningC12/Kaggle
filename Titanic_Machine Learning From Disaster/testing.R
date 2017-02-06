# create missing value summary
na_summary = data.frame(num_na = colSums(is.na(testing)))
na_summary = data.frame(Variable = rownames(na_summary),na_summary,
                        na_ratio = na_summary$num_na/nrow(testing))

# Drop the Cabin variable
testing = testing[,na_summary$na_ratio<0.7]

# Impute the missing ages based on average age by sex and Pclass
testing = as.data.table(testing)
age_summary = testing[,.(Age = mean(Age,na.rm=T)),by=c("Sex","Pclass")]
age_summary$Age_abs = round(age_summary$Age,0)

testing$Age[testing$Sex == "male" & testing$Pclass == 1 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 1,Age_abs]

testing$Age[testing$Sex == "male" & testing$Pclass == 2 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 2,Age_abs]

testing$Age[testing$Sex == "male" & testing$Pclass == 3 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "male"
                                                  & age_summary$Pclass == 3,Age_abs]

testing$Age[testing$Sex == "female" & testing$Pclass == 1 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 1,Age_abs]

testing$Age[testing$Sex == "female" & testing$Pclass == 2 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 2,Age_abs]

testing$Age[testing$Sex == "female" & testing$Pclass == 3 
             & is.na(testing$Age)] = age_summary[age_summary$Sex == "female"
                                                  & age_summary$Pclass == 3,Age_abs]


# Impute the missing fare
testing$Fare[is.na(testing$Fare)] = mean(testing$Fare[testing$Sex == "male" & testing$Pclass == 3],na.rm=T)


# check if there is still missing value
colSums(is.na(testing))

# Prediction
rpart_pred_test = predict(mod_rpart,newdata=testing,type="raw")
glm_pred_test = predict(mod_glm,newdata=testing,type="raw")

# Write the output
rpart_output = data.frame(PassengerId = testing$PassengerId,Survived = rpart_pred_test)
rpart_output$Survived = ifelse(rpart_output$Survived=="Dead",0,1)
glm_output = data.frame(PassengerId = testing$PassengerId,Survived = glm_pred_test)
glm_output$Survived = ifelse(glm_output$Survived=="Dead",0,1)
write.csv(rpart_output,"rpart_simple.csv",row.names = F)
write.csv(glm_output,"glm_simple.csv",row.names = F)

