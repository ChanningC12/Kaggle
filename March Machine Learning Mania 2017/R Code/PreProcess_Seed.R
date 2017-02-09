rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/Kaggle Projects/March Machine Learning Mania 2017/")

library(data.table)
library(dplyr)
library(DataCombine)

# Explore the Seed dataset
str(TS)
unique(TS$Seed)
TS = as.data.table(TS)
# Seed range 01 - 16
TS[,.(.N),by=Seed]
# 64-68 teams in each year
TS[,.(.N),by=Season]
# For year with multiple teams on the same seed, consider those teams the same seed
TS[TS$Seed=="Y11a" | TS$Seed=="Y11b",]

############# Create Master Seed Table #################
# Create UID for TS dataset
TS$Team_ID = paste(TS$Season,"_",TS$Team,sep = "")
# Validate if Team_ID is unique
length(unique(TS$Team_ID))
# Switch the column position
TS = setcolorder(TS,c(4,1:3))

# Extract the numeric seed from "Seed" column
TS$Seed_num = as.numeric(substr(TS$Seed,2,3))

############## One way to create lag variable using data.table #############
# Extract the numeric seed from "Seed" column
# TS$Seed_num = as.numeric(substr(TS$Seed,2,3))
# TS[,.(.N),by=Seed_num]
# Create lag variable on Seed
# TS$Season = ts(TS$Season)
# TS = TS[order(TS$Team),]
# TS[, Seed_pre:=c(NA, Seed_num[-.N]), by=Team]
############### End ###################


# Lag the seed
# Create a expand.grid for ts lag creation purpose
Season = sort(unique(TS$Season))
Team = sort(unique(TS$Team))
TS_blank = expand.grid(Season = Season,Team = Team)
class(TS_blank)
TS_blank$Team_ID = paste(TS_blank$Season,"_",TS_blank$Team,sep = "")

# Merge the blank TS table with original TS table
TS_all = merge(TS,TS_blank,by="Team_ID",all.y=T)
TS_all = as.data.frame.matrix(TS_all)
TS_all = TS_all[,c(1,6,7,3,5)]
colnames(TS_all) = c("Team_ID","Season","Team","Seed","Seed_num")

# Create lag variable on Seed
TS_all$Season = ts(TS_all$Season)
TS_all = TS_all[order(TS_all$Team),]
TS_Pre = DataCombine::slide(TS_all,Var="Seed_num",GroupVar = "Team", slideBy = -1)
names(TS_Pre)[6] = "Seed_Pre"

# Create master seed table
Seed = TS_Pre[!is.na(TS_Pre$Seed_num),]
sum(is.na(Seed$Seed_Pre))
table(Seed$Seed_Pre)
Seed$Seed_Diff = Seed$Seed_Pre - Seed$Seed_num

# Assumption: For team that did not have seed in the previous year, assign 16 to Seed_Diff
Seed$Seed_Pre[is.na(Seed$Seed_Pre)] = 20
Seed$Seed_Diff = Seed$Seed_Pre - Seed$Seed_num

# Output Seed table
write.csv(Seed,"Seed.csv",row.names = F)





