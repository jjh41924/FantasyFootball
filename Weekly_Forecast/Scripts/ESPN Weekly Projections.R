###########################
# File: ESPN Projections.R
# Description: Downloads Fantasy Football Projections from ESPN.com
# Date: 3/3/2013
# Notes:
# -ESPN projections do not include fumbles!
# To do:
###########################


setPointsForWeek_espn = function(week){
  #Load libraries
  require("XML")
  require("stringr")
  require("ggplot2")
  require("plyr")
  
  #Functions
  source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
    #Suffix
  suffix <- "espn"
  
  #Download fantasy football projections from ESPN.com
  qb_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=0&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  rb1_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  rb2_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=40",sep=""), stringsAsFactors = FALSE)$playertable_0
  rb3_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=80",sep=""), stringsAsFactors = FALSE)$playertable_0
  wr1_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  wr2_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=40",sep=""), stringsAsFactors = FALSE)$playertable_0
  wr3_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=80",sep=""), stringsAsFactors = FALSE)$playertable_0
  te_espn <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=6&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  d_espn = readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=16&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  k_espn = readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=17&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  
  #Add variable names for each object
  fileList <- c("qb_espn","rb1_espn","rb2_espn","rb3_espn","wr1_espn","wr2_espn","wr3_espn","te_espn","d_espn","k_espn")
  sapply(fileList,function(X){dim(get(X))})
  
  for(i in 1:length(fileList)){
    assign(fileList[i],get(fileList[i])[2:dim(get(fileList[i]))[1],])
    t <- get(fileList[i])
    rename.cols = c("player_espn","opposint_team","Status","passCompAtt_espn","passYds_espn","passTds_espn","passInt_espn","rushAtt_espn","rushYds_espn","rushTds_espn","rec_espn","recYds_espn","recTds_espn","pts_espn")
    names(t) <- rename.cols
    assign(fileList[i], t)
  }
  
  #Merge players within position
  rb_espn <- rbind(rb1_espn,rb2_espn,rb3_espn)
  wr_espn <- rbind(wr1_espn,wr2_espn,wr3_espn)
  
  #Add variable for player position
  qb_espn$pos <- as.factor("QB")
  rb_espn$pos <- as.factor("RB")
  wr_espn$pos <- as.factor("WR")
  te_espn$pos <- as.factor("TE")
  k_espn$pos <- as.factor("KK")
  d_espn$pos <- as.factor("DS")
  
  #Merge players across positions
  projections_espn <- rbind(qb_espn,rb_espn,wr_espn,te_espn)
  #head(projections_espn)
  
  #Replace symbols with value of zero
  projections_espn$passCompAtt_espn[projections_espn$passCompAtt_espn == "--/--"] <- "0/0"
  projections_espn$passYds_espn[projections_espn$passYds_espn == "--"] <- "0"
  projections_espn$passTds_espn[projections_espn$passTds_espn == "--"] <- "0"
  projections_espn$passInt_espn[projections_espn$passInt_espn == "--"] <- "0"
  projections_espn$rushAtt_espn[projections_espn$rushAtt_espn == "--"] <- "0"
  projections_espn$rushYds_espn[projections_espn$rushYds_espn == "--"] <- "0"
  projections_espn$rushTds_espn[projections_espn$rushTds_espn == "--"] <- "0"
  projections_espn$rec_espn[projections_espn$rec_espn == "--"] <- "0"
  projections_espn$recYds_espn[projections_espn$recYds_espn == "--"] <- "0"
  projections_espn$recTds_espn[projections_espn$recTds_espn == "--"] <- "0"
  projections_espn$pts_espn[projections_espn$pts_espn == "--"] <- "0"
  
  #Separate pass completions from attempts
  projections_espn$passComp_espn <- as.numeric(str_sub(string=projections_espn$passCompAtt_espn, end=str_locate(string=projections_espn$passCompAtt_espn, '/')[,1]-1))
  projections_espn$passAtt_espn <- as.numeric(str_sub(string=projections_espn$passCompAtt_espn, start=str_locate(string=projections_espn$passCompAtt_espn, '/')[,1]+1))
  
  #Add variables from other projection sources
  projections_espn$returnTds_espn <- NA
  projections_espn$fumbles_espn <- NA
  projections_espn$twoPts_espn <- NA
  
  #Convert variables from character strings to numeric
  projections_espn[,c("passYds_espn","passTds_espn","passInt_espn","rushAtt_espn","rushYds_espn","rushTds_espn","rec_espn","recYds_espn","recTds_espn","pts_espn","returnTds_espn","fumbles_espn","twoPts_espn")] <-
    convert.magic(projections_espn[,c("passYds_espn","passTds_espn","passInt_espn","rushAtt_espn","rushYds_espn","rushTds_espn","rec_espn","recYds_espn","recTds_espn","pts_espn","returnTds_espn","fumbles_espn","twoPts_espn")], "numeric")
  
  #Player names
  projections_espn$name_espn <- str_sub(projections_espn$player_espn, end=str_locate(string=projections_espn$player_espn, ',')[,1]-1)
  projections_espn$name_espn <- str_replace_all(projections_espn$name_espn, "\\*", "")
  projections_espn$name <- nameMerge(projections_espn$name_espn)
  
  #Player teams
  projections_espn$team_espn <- str_sub(projections_espn$player_espn, start=str_locate(string=projections_espn$player_espn, ',')[,1]+2, end = str_locate(string=projections_espn$player_espn, ',')[,1]+4)
  projections_espn$team_espn <- str_trim(projections_espn$team_espn, side="right")
  projections_espn$team_espn <- toupper(projections_espn$team_espn)
  projections_espn$team_espn[projections_espn$team_espn=="WSH"] <- "WAS"
  
  #Remove duplicate cases
  projections_espn[projections_espn$name %in% projections_espn[duplicated(projections_espn$name),"name"],]
  
  #Same player, different position
  dropNames <- c("DEXTERMCCLUSTER")
  dropVariables <- c("pos")
  dropLabels <- c("WR")
  
  projections_espn2 <- ddply(projections_espn, .(name), numcolwise(mean), na.rm=TRUE)
  
  for(i in 1:length(dropNames)){
    if(dim(projections_espn[-which(projections_espn[,"name"] == dropNames[i] & projections_espn[,dropVariables[i]] == dropLabels[i]),])[1] > 0){
      projections_espn <- projections_espn[-which(projections_espn[,"name"] == dropNames[i] & projections_espn[,dropVariables[i]] == dropLabels[i]),]
    }
  }
  
  projections_espn <- merge(projections_espn2, projections_espn[,c("name","name_espn","player_espn","pos","team_espn")], by="name")
  
  #Rename players
  
  #Calculate overall rank
  projections_espn$overallRank_espn <- rank(-projections_espn$pts_espn, ties.method="min")
  projections_espn$positionRank_espn =  rank(-projections_espn$pts_espn, ties.method="min")
  
  #Order variables in data set
  projections_espn <- projections_espn[,c(prefix, paste(varNames, suffix, sep="_"))]
  
  #Order players by overall rank
  projections_espn <- projections_espn[order(projections_espn$overallRank_espn),]
  row.names(projections_espn) <- 1:dim(projections_espn)[1]
  
  #Density Plot
  #ggplot(projections_espn, aes(x=pts_espn)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of ESPN Projected Points")
  #ggsave(paste(getwd(),"/Figures/ESPN projections.jpg", sep=""), width=10, height=10)
  #dev.off()
  
  #Save file  
  save(projections_espn, file = paste(getMYFFDir(),"/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_espn, file=paste(getMYFFDir(),"/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  
  cat(paste("[ESPN] DONE. week[",week,"]\n",sep=""))
}

getESPN_Projections = function(week){
  ret = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",sep=""))
  ret$source = "ESPN"
  ret$week = week
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}





