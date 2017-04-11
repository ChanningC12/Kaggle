rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/2017 Prediction/")

Team = read.csv("Raw Data/Teams.csv")
Pred_Tournament_Top = read.csv("Pred_Tournament_Top_xgb_Elo_0313.csv")

Pred_Tournament_Top_Detail = Pred_Tournament_Top
Pred_Tournament_Top_Detail$Team1 = substr(Pred_Tournament_Top_Detail$id,6,9)
Pred_Tournament_Top_Detail$Team2 = substr(Pred_Tournament_Top_Detail$id,11,14)

Detail = merge(Pred_Tournament_Top_Detail,Team,by.x="Team1",by.y="Team_Id",all.x=T)
names(Detail)[5] = "Team1_Name"
Detail = merge(Detail,Team,by.x="Team2",by.y="Team_Id",all.x=T)
names(Detail)[6] = "Team2_Name"

Detail = Detail[,c(3,2,5,1,6,4)]

write.csv(Detail,"Pred_Tournament_Top_xgb_Elo_0313_detail.csv",row.names = F)

Detail[Detail$Team1_Name=="Duke" & Detail$Team2_Name=="Troy",]
Detail[Detail$Team1_Name=="Northwestern",]

Detail[Detail$Team1_Name=="North Carolina" | Detail$Team2_Name=="North Carolina",]

