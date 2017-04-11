library(PlayerRatings)

Regular = read.csv("Raw Data/RegularSeasonDetailedResults.csv")
Regular$ID_SeasonTeam1 = paste(Regular$Season,"_",Regular$Wteam,sep="")
Regular$ID_SeasonTeam2 = paste(Regular$Season,"_",Regular$Lteam,sep="")
Regular$Result_num = 1

Elo = Regular[,names(Regular) %in% c("Daynum","ID_SeasonTeam1","ID_SeasonTeam2","Result_num")]
Elo$ID_SeasonTeam1 = as.character(Elo$ID_SeasonTeam1)
Elo$ID_SeasonTeam2 = as.character(Elo$ID_SeasonTeam2)
Elo_all = elo(Elo)
Elo_all = as.data.frame(Elo_all$ratings)
Elo_all$Win_percent = Elo_all$Win/Elo_all$Games
cor(Elo_all$Win_percent,Elo_all$Rating)
write.csv(Elo_all,"Elo_rating_0303.csv")











