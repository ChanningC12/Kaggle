################ Create Decision Tree Variables #####################
library(rattle)
library(rpart)
# running time: 
system.time(AI_tree <- rpart(as.factor(Survived)~.-PassengerId-cabin_num.B-cabin_num.A-cabin_num.D
                                      -cabin_num.E-cabin_num.T-cabin_num.G-cabin_num.F-TitleRare_Title
                                      -cabin_num.C-Embarked.Q,
                             data=allvars_trn,
                             method="class",cp=0.001))
# summary of the tree model
AI_tree
# prune the tree
str(AI_tree)
# use cptable to determine a reasonable complexity for pruning
AI_tree$cptable
# choose 0.003 cp to prune the tree
AI_tree_prune = prune(AI_tree,cp=0.005)
AI_tree_prune
plot(AI_tree_prune)
text(AI_tree_prune,cex=0.5)
# FancyRpartPlot
fancyRpartPlot(AI_tree_prune)

# Identify decision tree variables
allvars_trn_trn$tree_var1 = ifelse(allvars_trn_trn$TitleMr>=0.5 & allvars_trn_trn$Fare<26,0,1)
allvars_trn_trn$tree_var2 = ifelse(allvars_trn_trn$TitleMr<0.5 & allvars_trn_trn$fam_size<4.5,1,0)



