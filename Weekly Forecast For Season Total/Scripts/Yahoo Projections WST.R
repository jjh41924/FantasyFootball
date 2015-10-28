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
library("data.table")

#Functions
source(paste(getMYFFDir(),"/Functions.R", sep=""))
source(paste(getMYFFDir(),"/League Settings.R", sep=""))

setPointsForEntireSeason_yahoo = function(season=2015) {
  #Projection Info
  suffix <- "yahoo"
  
  #Download fantasy football projections from Yahoo.com
  yahoo_baseurl <- "http://football.fantasysports.yahoo.com/f1/288510/players?status=ALL&cut_type=9&myteam=0&sort=PTS&sdir=1"
  yahoo_pages <- paste0("&count=", seq(0, 150, by=25))
  yahoo_pos <- list(QB="QB", RB="RB", WR="WR", TE="TE", K="K", DST="DEF")
  yahoo_urls <- paste0(yahoo_baseurl, yahoo_pages, "&pos=", rep(yahoo_pos, each=length(yahoo_pages)), "&stat1=S_PS_", season)
  
  #Scrape
  yahoo <- lapply(yahoo_urls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)[2]$'NULL')})
#    for(i in 1:length(yahoo)) { yahoo[[i]] = yahoo[[i]][,-ncol(yahoo[[i]]), with=F] }
  yahooList <- yahoo
  
  #Clean data
  qbNames <- rbNames <- wrNames <- teNames <- c("star","player","add","owner","GP??","points","ownedPct","proj","actual","passYds","passTds","passInt","passAtt","rushYds","rushTds","recTgt","rec","recYds","recTds","returnTds","twoPts","fumbles","missing1")
  kNames <- c("star","player","add","owner","GP??","points","ownedPct","proj","actual","fg019","fg2029","fg3039","fg4049","fg50","fg","missing2")
  dstNames <- c("star","player","add","owner","GP??","points","ownedPct","proj","actual","dstPtsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstDTd","dstBlk","dstRetTd","missing3")
  
  #Clean data
  for(i in 1:length(yahooList)){
    if(nrow(yahooList[[i]]) > 0){
#       browser()
      #remove extra column that showed up for some reason
      #Add position to projection
      yahooList[[i]][,pos := rep(names(yahoo_pos), each=length(yahoo_pages))[i]]
      yahooList[[i]][,pos := as.factor(pos)]
      
      #Add variable names
      if(unique(yahooList[[i]][,pos]) == "QB"){
        setnames(yahooList[[i]], c(qbNames, "pos"))
      } else if(unique(yahooList[[i]][,pos]) == "RB"){
        setnames(yahooList[[i]], c(rbNames, "pos"))
      } else if(unique(yahooList[[i]][,pos]) == "WR"){
        setnames(yahooList[[i]], c(wrNames, "pos"))
      } else if(unique(yahooList[[i]][,pos]) == "TE"){
        setnames(yahooList[[i]], c(teNames, "pos"))
      } else if(unique(yahooList[[i]][,pos]) == "K"){
        setnames(yahooList[[i]], c(kNames, "pos"))
      } else if(unique(yahooList[[i]][,pos]) == "DST"){
        setnames(yahooList[[i]], c(dstNames, "pos"))
      }
    }
  }
  
  #Merge
  projections_yahoo <- rbindlist(yahoo, use.names=TRUE, fill=TRUE)
  
  #Remove special characters (%, comma)
  projections_yahoo <- projections_yahoo[,lapply(.SD, function(x) gsub("\\%", "", x))]
  projections_yahoo <- projections_yahoo[,lapply(.SD, function(x) gsub("\\,", "", x))]
  
  #Convert variables from character strings to numeric
  numericVars <- names(projections_yahoo)[names(projections_yahoo) %in% c(scoreCategories, "dstDTd","dstRetTd")]
  projections_yahoo[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]
  
  #Calculate variables
  projections_yahoo[,dstTd := mySum(projections_yahoo[, c("dstDTd","dstRetTd"), with=FALSE])]
  
  #Player name and team
  projections_yahoo[,player := str_trim(sapply(str_split(player, "\n"), "[", 2))]
  
  projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("(.*)\\-", "\\1", projections_yahoo$player), start=0, end=nchar(projections_yahoo$player)-4))]
  projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  R", "", name_yahoo)))]
  projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  W", "", name_yahoo)))]
  projections_yahoo[,name_yahoo := str_trim(str_sub(gsub("  Q", "", name_yahoo)))]
  projections_yahoo[,team_yahoo := cleanTeamAbbreviations(toupper(str_trim(str_sub(name_yahoo, start=nchar(name_yahoo)-2, end=nchar(name_yahoo)))))]
  projections_yahoo[,name_yahoo := str_trim(str_sub(name_yahoo, start=0, end=nchar(name_yahoo)-3))]
  projections_yahoo[which(pos == "DST"), name_yahoo := convertTeamName(projections_yahoo$team_yahoo[which(projections_yahoo$pos == "DST")])]
  projections_yahoo[,name := nameMerge(name_yahoo)]
  
  #Remove NA rows
  projections_yahoo <- projections_yahoo[!is.na(name),]
  
  #Remove duplicate cases
  duplicateCases <- projections_yahoo[duplicated(name)]$name
  projections_yahoo[which(name %in% duplicateCases),]
  
  #Rename players
  projections_yahoo[name == "STEVIEJOHNSON", name := "STEVEJOHNSON"]
  
  #Calculate Overall Rank
  projections_yahoo <- projections_yahoo[order(-as.numeric(points))][,overallRank := 1:.N]
  
  #Calculate Position Rank
  projections_yahoo <- projections_yahoo[order(-as.numeric(points))][,positionRank := 1:.N, by=list(pos)]
  
  #Add source
  projections_yahoo$sourceName <- suffix
  
  #Order variables in data set
  allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
  keepVars <- allVars[allVars %in% names(projections_yahoo)]
  projections_yahoo <- projections_yahoo[,keepVars, with=FALSE]
  
  #Order players by overall rank
  projections_yahoo <- projections_yahoo[order(projections_yahoo$overallRank),]
  
  #Density Plot
  # ggplot(projections_yahoo, aes(x=points)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
  # ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
  # dev.off()
  
  #Save file
  save(projections_yahoo, file = paste(getMYFFDir(),"/Weekly Forecast For Season Total/Yahoo/Projections_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_yahoo, file=paste(getMYFFDir(),"/Weekly Forecast For Season Total/Yahoo/Projections_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}


getYahooSeason_Projections = function(week){
  ret = getProjections(paste(getMYFFDir(),"/Weekly Forecast/Yahoo/Projections_Date_",sep=""))
  ret$source = "Yahoo"
  ret$week = week
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}