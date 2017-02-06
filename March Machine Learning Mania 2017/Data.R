rm(list=ls())
gc()
getwd()
setwd("../Desktop/Kaggle & Coursera/March Machine Learning Mania 2017/")

# Read in Team
# Team level
team = read.csv("Teams.csv")
dim(team)
str(team)
head(team)

# Read in Season
# Season level, 1985-2017
season = read.csv("Seasons.csv")
dim(season)
str(season)
head(season)

# Read in Regular Season Compact Result.csv
# Game level
# Unique key: Season + Daynum + Wteam + Lteam
RSCR = read.csv("RegularSeasonCompactResults.csv")
dim(RSCR)
str(RSCR)
head(RSCR)
tail(RSCR)
table(RSCR$Wloc)

# Read in RegularSeasonDetailedResults data (2003 and onward)
# Game level
# Unique key: Season + Daynum + Wteam + Lteam
RSDR = read.csv("RegularSeasonDetailedResults.csv")
str(RSDR)
head(RSDR)
range(RSDR$Season)

# Read in TourneyCompactResults data
# Game level
# Unique key: Season + Daynum + Wteam + Lteam
TCR = read.csv("TourneyCompactResults.csv")
str(TCR)
range(TCR$Daynum)

# Read in TourneyDetailedResults
# Game level
# Unique key: Season + Daynum + Wteam + Lteam
TDR = read.csv("TourneyDetailedResults.csv")
str(TDR)

# Read in TourneySeeds
# Season seed level
# Unique key: Season+Seed
TS = read.csv("TourneySeeds.csv")
str(TS)
head(TS)
unique(TS$Seed)

# Read in TourneySlots
# Game level
# Unique key: Season + Slot
TSlot = read.csv("TourneySlots.csv")
str(TSlot)
head(TSlot)
tail(TSlot)









