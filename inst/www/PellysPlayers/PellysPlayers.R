library(knitr)
library(tools)
library(RCurl)
url<-getURL("https://docs.google.com/spreadsheet/pub?key=0AiV4WHc7_JnQdHVweEF3dV9CcUlNWm43MHJ5SXUwSlE&output=csv",ssl.verifypeer=FALSE)
stats301<-read.csv(textConnection(url))
url<-getURL("https://docs.google.com/spreadsheet/pub?key=0AiV4WHc7_JnQdG0yVUdFYVNjdnM1cGhxLWNlZ19xU1E&single=true&gid=0&output=csv",ssl.verifypeer=FALSE)
statsCricket<-read.csv(textConnection(url))
stats301_noNA<-na.omit(stats301[stats301$Name!="Forfeit",])
stats301_noNA[,2]<-as.Date(stats301_noNA[,2], "%m/%d/%Y")
statsCricket_noNA<-na.omit(statsCricket[statsCricket$Name1!="Forfeit"||statsCricket$Name2!="Forfeit",])
statsCricket_noNA[,2]<-as.Date(statsCricket_noNA[,2], "%m/%d/%Y")

winPercent301<-function(myDF)
{
   wins<-length(myDF$PointsRemaining[myDF$PointsRemaining==0])
   total<-length(myDF$PointsRemaining)
   return(data.frame(wins=wins,total=total,percent=wins/total))
}

winPercentCricket<-function(myDF)
{
   wins<-length(myDF$Won[myDF$Won==1])
   total<-length(myDF$Won)
   return(data.frame(wins=wins,total=total,percent=wins/total))
} 



avg301<-function(myDF)
{
   startscore<-myDF$StartingScore
   pointsremain<-myDF$PointsRemaining
   totaldarts<-myDF$TotalDarts
   return(data.frame(avgStartScore=mean(startscore),avgPointsRemain=mean(pointsremain),
                     avgTotalDarts=mean(totaldarts),
                     avgPPD=(sum(startscore)-sum(pointsremain))/sum(totaldarts),
                     highPPD=max((startscore-pointsremain)/totaldarts),
                     highOut=max(myDF$Out)))
}
                                    

cricketTeams<-paste(statsCricket_noNA$Name1,statsCricket_noNA$Name2,sep=" ")
cricketTeams<-gsub("  "," ",cricketTeams)
cricketTeamsNum<-as.numeric(unlist(lapply(cricketTeams,function(x) sum(strtoi(charToRaw(x),base=16)))))
cricketTeamsName<-cricketTeams[order(cricketTeamsNum)][!duplicated(cricketTeamsNum[order(cricketTeamsNum)])]
cricketTeams<-data.frame(cricketTeamsName,cricketTeamsNum=cricketTeamsNum[order(cricketTeamsNum)][!duplicated(cricketTeamsNum[order(cricketTeamsNum)])])
statsCricket_noNA<-data.frame(statsCricket_noNA,cricketTeamsNum)
statsCricket_noNA<-merge(statsCricket_noNA,cricketTeams,by="cricketTeamsNum",all.x=T)
byPairCricket<-by(statsCricket_noNA,statsCricket_noNA$cricketTeamsName,winPercentCricket)
byPerson301<-by(stats301_noNA,stats301_noNA$Name,winPercent301)
byPerson301Avg<-by(stats301_noNA,stats301_noNA$Name,avg301)
byPairCricket<-do.call("rbind",byPairCricket)
byPerson301<-do.call("rbind",byPerson301)
byPerson301Avg<-do.call("rbind",byPerson301Avg)
byPairCricket[order(byPairCricket$percent,decreasing=T),]
byPerson301[order(byPerson301$percent,decreasing=T),]
byPerson301Avg[order(byPerson301Avg$avgPPD,decreasing=T),]

#Player Index
jeffInd<-as.character(statsCricket_noNA$Name1)=="Jeff"|as.character(statsCricket_noNA$Name2)=="Jeff"
rolandInd<-as.character(statsCricket_noNA$Name1)=="Roland"|as.character(statsCricket_noNA$Name2)=="Roland"
mikeInd<-as.character(statsCricket_noNA$Name1)=="Mike"|as.character(statsCricket_noNA$Name2)=="Mike"
kennyInd<-as.character(statsCricket_noNA$Name1)=="Kenny"|as.character(statsCricket_noNA$Name2)=="Kenny"
edInd<-as.character(statsCricket_noNA$Name1)=="Ed"|as.character(statsCricket_noNA$Name2)=="Ed"
charlieInd<-as.character(statsCricket_noNA$Name1)=="Charlie"|as.character(statsCricket_noNA$Name2)=="Charlie"
jeffCricket<-winPercentCricket(statsCricket_noNA[jeffInd,])
rolandCricket<-winPercentCricket(statsCricket_noNA[rolandInd,])
mikeCricket<-winPercentCricket(statsCricket_noNA[mikeInd,])
kennyCricket<-winPercentCricket(statsCricket_noNA[kennyInd,])
edCricket<-winPercentCricket(statsCricket_noNA[edInd,])
charlieCricket<-winPercentCricket(statsCricket_noNA[charlieInd,])
playerCricket<-rbind(jeffCricket,rolandCricket,mikeCricket,kennyCricket,edCricket,charlieCricket)
row.names(playerCricket)<-c("Jeff","Roland","Mike","Kenny","Ed","Charlie")
playerCricket[order(playerCricket$percent,decreasing=T),]

#Total Stats
teamCricket<-winPercentCricket(statsCricket_noNA)
team301<-winPercent301(stats301_noNA)
team301Avg<-avg301(stats301_noNA)
byPersonCricket<-
byPairCricket<-by(statsCricket_noNA,statsCricket_noNA$cricketTeamsName,winPercentCricket)
byPerson301<-by(stats301_noNA,stats301_noNA$Name,winPercent301)
byPerson301Avg<-by(stats301_noNA,stats301_noNA$Name,avg301)
byPairCricket<-do.call("rbind",byPairCricket)
byPerson301<-do.call("rbind",byPerson301)
byPerson301Avg<-do.call("rbind",byPerson301Avg)
byPairCricket[order(byPairCricket$percent,decreasing=T),]
byPerson301[order(byPerson301$percent,decreasing=T),]
byPerson301Avg[order(byPerson301Avg$avgPPD,decreasing=T),]

teamCricket
team301
team301Avg


#Games in last 6 months
statsCricket_3mo<-statsCricket_noNA[statsCricket_noNA[,3]>"2012-11-01",]
stats301_3mo<-stats301_noNA[stats301_noNA[,2]>"2012-11-01",]
byPairCricket_3mo<-by(statsCricket_3mo,statsCricket_3mo$cricketTeamsName,winPercentCricket)
byPerson301_3mo<-by(stats301_3mo,stats301_3mo$Name,winPercent301)
byPerson301Avg_3mo<-by(stats301_3mo,stats301_3mo$Name,avg301)
byPairCricket_3mo<-do.call("rbind",byPairCricket_3mo)
byPerson301_3mo<-do.call("rbind",byPerson301_3mo)
byPerson301Avg_3mo<-do.call("rbind",byPerson301Avg_3mo)
write.csv(byPairCricket_3mo[order(byPairCricket_3mo$percent,decreasing=T),],"cricketWin_3mo.csv")
write.csv(byPerson301_3mo[order(byPerson301_3mo$percent,decreasing=T),],"301Win_3mo.csv")
write.csv(byPerson301Avg_3mo[order(byPerson301Avg_3mo$avgPPD,decreasing=T),],"301Avg_3mo.csv")



#boxplot(statsCricket_noNA$Marks~statsCricket_noNA$Won)
#Average Handicap
dateHandicap<-by(stats301_noNA,stats301_noNA$Date,function(x) mean(x$LeaguePointsPerDart))

dateHandicap<-do.call("rbind",list(dateHandicap))

date301Win<-by(stats301_noNA,stats301_noNA$Date,winPercent301)
date301Win<-do.call("rbind",date301Win)

#Games v All Americans - not enough games to make much sense
statsCricket_AA<-statsCricket_noNA[statsCricket_noNA$OpposingTeamName=="All Americans",]
stats301_AA<-stats301_noNA[stats301_noNA$OpposingTeamName=="All Americans",]
byPairCricket_AA<-by(statsCricket_AA,statsCricket_AA$cricketTeamsName,winPercentCricket)
byPerson301_AA<-by(stats301_AA,stats301_AA$Name,winPercent301)
byPerson301Avg_AA<-by(stats301_AA,stats301_AA$Name,avg301)
byPairCricket_AA<-do.call("rbind",byPairCricket_AA)
byPerson301_AA<-do.call("rbind",byPerson301_AA)
byPerson301Avg_AA<-do.call("rbind",byPerson301Avg_AA)
byPairCricket_AA[order(byPairCricket_AA$percent,decreasing=T),]
byPerson301_AA[order(byPerson301_AA$percent,decreasing=T),]
byPerson301Avg_AA[order(byPerson301Avg_AA$avgPPD,decreasing=T),]