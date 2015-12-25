###########################
# File: FFtoday Projections.R
# Description: Downloads Fantasy Football Projections from fftoday.com
# Date: 6/7/2014
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

setPointsForWeek_FFtoday = function(week){
  #Suffix
  suffix <- "fftoday"
  
  #Download fantasy football projections from FFtoday.com
  qb1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=10&LeagueID=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  qb2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=10&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  rb1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=20&LeagueID=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  rb2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=20&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  wr1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=30&LeagueID=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  wr2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  wr3_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=30&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=2",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  te1_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=40&LeagueID=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  te2_fftoday <- readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=40&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  
  k1_fftoday  =  readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=",format(Sys.time(),"%Y"),"&GameWeek=",week,"&PosID=80&LeagueID=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  #k2_fftoday  =  readHTMLTable(paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2014&GameWeek=",week,"&PosID=80&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1",sep=""), stringsAsFactors = FALSE)[11]$'NULL'
  
  #Add variable names for each object
  names(qb1_fftoday) <- names(qb2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","passComp_fftoday","passAtt_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","pts_fftoday")
  names(rb1_fftoday) <- names(rb2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday")
  names(wr1_fftoday) <- names(wr2_fftoday) <- names(wr3_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday")
  names(te1_fftoday) <- names(te2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday")
  
  names(k1_fftoday)                        =  c("star_fftoday","player_fftoday","team_fftoday","opp_fftoday","fg_made_fftoday","fg_missed_fftoday","xp_made_fftoday","xp_missed_fftoday","ffpts_fftoday")
  
  #Trim dimensions
  qb1_fftoday <- qb1_fftoday[2:(dim(qb1_fftoday)[1]-1),]
  qb2_fftoday <- qb2_fftoday[2:(dim(qb2_fftoday)[1]-1),]
  rb1_fftoday <- rb1_fftoday[2:(dim(rb1_fftoday)[1]-1),]
  rb2_fftoday <- rb2_fftoday[2:(dim(rb2_fftoday)[1]-1),]
  wr1_fftoday <- wr1_fftoday[2:(dim(wr1_fftoday)[1]-1),]
  wr2_fftoday <- wr2_fftoday[2:(dim(wr2_fftoday)[1]-1),]
  wr3_fftoday <- wr3_fftoday[2:(dim(wr3_fftoday)[1]-1),]
  te1_fftoday <- te1_fftoday[2:(dim(te1_fftoday)[1]-1),]
  te2_fftoday <- te2_fftoday[2:(dim(te2_fftoday)[1]-1),]
  
  k1_fftoday <- k1_fftoday[2:(dim(k1_fftoday)[1]-1),]
  
  #Merge within position
  qb_fftoday <- rbind(qb1_fftoday,qb2_fftoday)
  rb_fftoday <- rbind(rb1_fftoday,rb2_fftoday)
  wr_fftoday <- rbind(wr1_fftoday,wr2_fftoday,wr3_fftoday)
  te_fftoday <- rbind(te1_fftoday,te2_fftoday)
  
  k_fftoday <- k1_fftoday
  
  #Add variable for player position
  qb_fftoday$pos <- as.factor("QB")
  rb_fftoday$pos <- as.factor("RB")
  wr_fftoday$pos <- as.factor("WR")
  te_fftoday$pos <- as.factor("TE")
  
  k_fftoday$pos <- as.factor("K")
  
  #Merge across positions
  projections_fftoday <- rbind.fill(qb_fftoday, rb_fftoday, wr_fftoday, te_fftoday)
  
  #Add missing variables
  projections_fftoday$returnTds_fftoday <- NA
  projections_fftoday$twoPts_fftoday <- NA
  projections_fftoday$fumbles_fftoday <- NA
  
  #Remove special characters(commas)
  projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","returnTds_fftoday","twoPts_fftoday","fumbles_fftoday")] <-
    apply(projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","returnTds_fftoday","twoPts_fftoday","fumbles_fftoday")], 2, function(x) gsub("\\,", "", x))
  k_fftoday[,c("fg_made_fftoday","fg_missed_fftoday","xp_made_fftoday","xp_missed_fftoday","ffpts_fftoday")] <-
    apply(k_fftoday[,c("fg_made_fftoday","fg_missed_fftoday","xp_made_fftoday","xp_missed_fftoday","ffpts_fftoday")], 2, function(x) gsub("\\,", "", x))
  
  #Convert variables from character strings to numeric
  projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","returnTds_fftoday","twoPts_fftoday","fumbles_fftoday")] <- 
    convert.magic(projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","pts_fftoday","returnTds_fftoday","twoPts_fftoday","fumbles_fftoday")], "numeric")
  k_fftoday[,c("fg_made_fftoday","fg_missed_fftoday","xp_made_fftoday","xp_missed_fftoday","ffpts_fftoday")] =
    convert.magic(k_fftoday[,c("fg_made_fftoday","fg_missed_fftoday","xp_made_fftoday","xp_missed_fftoday","ffpts_fftoday")], "numeric")
  
  #Player name, position, and team
  projections_fftoday$name_fftoday <- str_trim(str_sub(projections_fftoday$player, start=2))
  projections_fftoday$name <- nameMerge(projections_fftoday$name_fftoday)
  
  k_fftoday$name_fftoday <- str_trim(str_sub(k_fftoday$player, start=2))
  k_fftoday$name <- nameMerge(k_fftoday$name_fftoday)
  
  #Remove duplicate cases
  projections_fftoday[projections_fftoday$name %in% projections_fftoday[duplicated(projections_fftoday$name),"name"],]
  #projections_fftoday <- projections_fftoday[-which(projections_fftoday$name_fftoday=="Dexter McCluster" & projections_fftoday$pos=="RB"),]
  
  #Rename players
 # projections_fftoday[projections_fftoday$name=="BENWATSON", "name"] <- "BENJAMINWATSON"
  
  #Calculate overall rank
  projections_fftoday$overallRank_fftoday <- rank(-projections_fftoday$pts_fftoday, ties.method="min")
 
  k_fftoday$rank_fftoday <- rank(-k_fftoday$ffpts, ties.method="min")
  
  #Calculate Position Rank
  projections_fftoday$positionRank_fftoday <- NA
  projections_fftoday[which(projections_fftoday$pos == "QB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "QB"), "pts_fftoday"], ties.method="min")
  projections_fftoday[which(projections_fftoday$pos == "RB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "RB"), "pts_fftoday"], ties.method="min")
  projections_fftoday[which(projections_fftoday$pos == "WR"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "WR"), "pts_fftoday"], ties.method="min")
  projections_fftoday[which(projections_fftoday$pos == "TE"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "TE"), "pts_fftoday"], ties.method="min")
  
  #Order variables in data set
  projections_fftoday <- projections_fftoday[,c(prefix, paste(varNames, suffix, sep="_"))]
  k_fftoday <- k_fftoday[,c(prefix, paste(c("rank","fg_made","fg_missed","xp_made","xp_missed","ffpts"), suffix, sep="_"))]
  
  #Order players by overall rank
  projections_fftoday <- projections_fftoday[order(projections_fftoday$overallRank_fftoday),]
  row.names(projections_fftoday) <- 1:dim(projections_fftoday)[1]
  
  
  #Save file  
  save(projections_fftoday, file = paste(getMYFFDir(),"/Weekly_Forecast/FFtoday/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_fftoday, file=paste(getMYFFDir(),"/Weekly_Forecast/FFtoday/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
 
  save(k_fftoday, file = paste(getMYFFDir(),"/Weekly_Forecast/FFtoday/Kicker_Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(k_fftoday, file=paste(getMYFFDir(),"/Weekly_Forecast/FFtoday/Kicker_Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
 cat(paste("[FFToday] DONE. week[",week,"]\n",sep=,""))
}

getFFtoday_Projections = function(week){
  ret = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FFtoday/Projections_Week_",week,"_Date_",sep=""))
  ret$source = "FFtoday"
  ret$week = week
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}
