###########################
# File: Yahoo Projections.R
# Description: Downloads Fantasy Football Projections from yahoo.com
# Date: 5/31/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))


setPointsForWeek_yahoo = function(week){
  
  #Suffix
  suffix <- "yahoo"
  
  #Download fantasy football projections from Yahoo.com
  projections_yahoo = NULL
  for(i in 0:14) {
    if(i==0){
      projections_yahoo = readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_W_",week,"&myteam=0&sort=PTS&sdir=1&count=0",sep=""), stringsAsFactors = FALSE)[2]$'NULL'
    } else {
      projections_yahoo = rbind(projections_yahoo,readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos=O&cut_type=9&stat1=S_W_",week,"&myteam=0&sort=PTS&sdir=1&count=",(25*i),sep=""), stringsAsFactors = FALSE)[2]$'NULL')
    }
  }
  
  #Variable Names
  names(projections_yahoo) <- c("star","player","add","owner","pts_yahoo","ownedPct","proj","actual",
                                "passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","passAtt_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","missing")
  
  #Add missing variables
  projections_yahoo$passComp_yahoo <- NA
  
  #Remove special characters(commas)
  projections_yahoo[,c("passAtt_yahoo","passComp_yahoo","passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","pts_yahoo")] <-
    apply(projections_yahoo[,c("passAtt_yahoo","passComp_yahoo","passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","pts_yahoo")], 2, function(x) gsub("\\,", "", x))
  
  #Convert variables from character strings to numeric
  projections_yahoo[,c("passAtt_yahoo","passComp_yahoo","passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","pts_yahoo")] <- 
    convert.magic(projections_yahoo[,c("passAtt_yahoo","passComp_yahoo","passYds_yahoo","passTds_yahoo","passInt_yahoo","rushAtt_yahoo","rushYds_yahoo","rushTds_yahoo","rec_yahoo","recYds_yahoo","recTds_yahoo","returnTds_yahoo","twoPts_yahoo","fumbles_yahoo","pts_yahoo")], "numeric")
  
  #Player name, position, and team
  projections_yahoo$player <- str_trim(sapply(str_split(projections_yahoo$player, "\n"), "[[", 2))
  projections_yahoo$pos <- str_trim(str_sub(projections_yahoo$player, start= -2))
  projections_yahoo$name_yahoo <- str_trim(str_sub(projections_yahoo$player, start=0, end=nchar(projections_yahoo$player)-8))
  projections_yahoo$name <- nameMerge(projections_yahoo$name_yahoo)
  projections_yahoo$team_yahoo <- toupper(str_trim(str_sub(projections_yahoo$player, start=str_locate(projections_yahoo$player, "-")[,1]-4, end=str_locate(projections_yahoo$player, "-")[,1]-2)))
  
  #Remove duplicate cases
  projections_yahoo[projections_yahoo$name %in% projections_yahoo[duplicated(projections_yahoo$name),"name"],]
  #projections_yahoo <- projections_yahoo[-which(projections_yahoo$name_yahoo=="Dexter McCluster" & projections_yahoo$pos=="RB"),]
  
  #Rename players
  projections_yahoo[projections_yahoo$name=="STEVIEJOHNSON", "name"] <- "STEVEJOHNSON"
  
  #Calculate overall rank
  projections_yahoo$pts_yahoo = as.numeric(projections_yahoo$pts_yahoo)
  projections_yahoo$overallRank_yahoo <- rank(-projections_yahoo$pts_yahoo, ties.method="min")
  
  #Calculate Position Rank
  projections_yahoo$positionRank_yahoo <- NA
  projections_yahoo[which(projections_yahoo$pos == "QB"), "positionRank_yahoo"] <- rank(-projections_yahoo[which(projections_yahoo$pos == "QB"), "pts_yahoo"], ties.method="min")
  projections_yahoo[which(projections_yahoo$pos == "RB"), "positionRank_yahoo"] <- rank(-projections_yahoo[which(projections_yahoo$pos == "RB"), "pts_yahoo"], ties.method="min")
  projections_yahoo[which(projections_yahoo$pos == "WR"), "positionRank_yahoo"] <- rank(-projections_yahoo[which(projections_yahoo$pos == "WR"), "pts_yahoo"], ties.method="min")
  projections_yahoo[which(projections_yahoo$pos == "TE"), "positionRank_yahoo"] <- rank(-projections_yahoo[which(projections_yahoo$pos == "TE"), "pts_yahoo"], ties.method="min")
  
  #Order variables in data set
  projections_yahoo <- projections_yahoo[,c(prefix, paste(varNames, suffix, sep="_"))]
  
  #Order players by overall rank
  projections_yahoo <- projections_yahoo[order(projections_yahoo$overallRank_yahoo),]
  row.names(projections_yahoo) <- 1:dim(projections_yahoo)[1]
  
  # #Density Plot
  # ggplot(projections_yahoo, aes(x=pts_yahoo)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
  # ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
  # dev.off()
  
  #Save file  
  save(projections_yahoo, file = paste(getMYFFDir(),"/Weekly Forecast/Yahoo/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_yahoo, file=paste(getMYFFDir(),"/Weekly Forecast/Yahoo/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}

getYahoo_Projections = function(week){
  return(getProjections(paste(getMYFFDir(),"/Weekly Forecast/Yahoo/Projections_Week_",week,"_Date_",sep="")));
  #write.csv(file=paste("C:/MY_FF/Weekly Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}

  