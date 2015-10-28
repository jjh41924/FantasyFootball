###########################
# File: CBS Projections.R
# Description: Downloads Fantasy Football Projections from cbssports.com
# Date: 3/3/2013
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


setPointsForWeek_cbs = function(week, isDave = FALSE){
  the.clarvoiant = "jamey_eisenberg"
  if(isDave) { the.clarvoiant = "dave_richard" }
  
  
  #Download fantasy football projections from cbssports.com
  #                              http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/1       /jamey_eisenberg/standard?&print_rows=9999
  qb_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/",week,"/",the.clarvoiant,"/standard?&print_rows=9999",sep=""), stringsAsFactors = FALSE)[7]$'NULL'
  rb1_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/",week,"/",the.clarvoiant,"/standard?&print_rows=9999",sep=""), stringsAsFactors = FALSE)[7]$'NULL'
  #rb2_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season?&start_row=51", stringsAsFactors = FALSE)[7]$'NULL'
  wr1_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/",week,"/",the.clarvoiant,"/standard?&print_rows=9999",sep=""), stringsAsFactors = FALSE)[7]$'NULL'
  #wr2_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season?&start_row=51", stringsAsFactors = FALSE)[7]$'NULL'
  te_cbs <- readHTMLTable(paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/TE/",week,"/",the.clarvoiant,"/standard?&print_rows=9999",sep=""), stringsAsFactors = FALSE)[7]$'NULL'
  
  #Add variable names for each object
  names(qb_cbs) <- c("player_cbs","passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs","passCompPct_cbs","passYdsPerAtt_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs","fumbles_cbs","pts_cbs")
  names(rb1_cbs) <- c("player_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","pts_cbs")
  names(wr1_cbs) <- c("player_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","pts_cbs")
  names(te_cbs) <- c("player_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","pts_cbs")
  
  #Trim dimensions
  qb_cbs <- qb_cbs[3:(dim(qb_cbs)[1]-1),]
  rb1_cbs <- rb1_cbs[3:(dim(rb1_cbs)[1]-1),]
  #rb2_cbs <- rb2_cbs[3:(dim(rb2_cbs)[1]-1),]
  wr1_cbs <- wr1_cbs[3:(dim(wr1_cbs)[1]-1),]
  #wr2_cbs <- wr2_cbs[3:(dim(wr2_cbs)[1]-1),]
  te_cbs <- te_cbs[3:(dim(te_cbs)[1]-1),]
  
  #Merge within position
  rb_cbs <- rb1_cbs #rbind(rb1_cbs,rb2_cbs)
  wr_cbs <- wr1_cbs #rbind(wr1_cbs,wr2_cbs)
  
  #Add variable for player position
  qb_cbs$pos <- as.factor("QB")
  rb_cbs$pos <- as.factor("RB")
  wr_cbs$pos <- as.factor("WR")
  te_cbs$pos <- as.factor("TE")
  
  #Merge across positions
  projections_cbs <- rbind.fill(qb_cbs, rb_cbs, wr_cbs, te_cbs)
  
  #Add variables from other projection sources
  projections_cbs$twoPts_cbs <- NA
  
  #Convert variables from character strings to numeric
  projections_cbs[,c("twoPts_cbs","fumbles_cbs","pts_cbs",
                     "rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs",
                     "rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs",
                     "passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs","passCompPct_cbs","passYdsPerAtt_cbs")] <- convert.magic(projections_cbs[,c("twoPts_cbs","fumbles_cbs","pts_cbs",
                                                                                                                                                                        "rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs",
                                                                                                                                                                        "rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs",
                                                                                                                                                                        "passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs","passCompPct_cbs","passYdsPerAtt_cbs")], "numeric")
  
  #Player names
  projections_cbs$name_cbs <- str_sub(projections_cbs$player, end=str_locate(string=projections_cbs$player, ',')[,1]-1)
  projections_cbs$name <- nameMerge(projections_cbs$name_cbs)
  
  #Remove Duplicates
  projections_cbs[projections_cbs$name %in% projections_cbs[duplicated(projections_cbs$name),"name"],]
  #projections_cbs[projections_cbs$name_cbs == "James Casey","pos"] <- "TE"
  
  #Rename Players
  #projections_cbs[projections_cbs$name_cbs=="EJ Manuel", "name_cbs"] <- "E.J. Manuel"
  
  #Player teams
  projections_cbs$team_cbs <- str_trim(str_sub(projections_cbs$player, start= -3))
  
  #Calculate overall rank
  projections_cbs$overallRank_cbs <- rank(-projections_cbs$pts_cbs, ties.method="min")
  
  #Calculate Position Rank
  projections_cbs$positionRank_cbs <- NA
  projections_cbs[which(projections_cbs$pos == "QB"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "QB"), "pts_cbs"], ties.method="min")
  projections_cbs[which(projections_cbs$pos == "RB"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "RB"), "pts_cbs"], ties.method="min")
  projections_cbs[which(projections_cbs$pos == "WR"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "WR"), "pts_cbs"], ties.method="min")
  projections_cbs[which(projections_cbs$pos == "TE"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "TE"), "pts_cbs"], ties.method="min")
  
  #Order variables in data set
  projections_cbs <- projections_cbs[,c("name","name_cbs","pos","team_cbs","positionRank_cbs","overallRank_cbs",
                                        "passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs","passCompPct_cbs","passYdsPerAtt_cbs",
                                        "rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs",
                                        "rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","twoPts_cbs","fumbles_cbs","pts_cbs")]
  
  #Order players by overall rank
  projections_cbs <- projections_cbs[order(projections_cbs$overallRank_cbs),]
  row.names(projections_cbs) <- 1:dim(projections_cbs)[1]
  
  #Save file  
  folder = "CBS_Jamey"
  if(isDave) { folder = "CBS_Dave" }
  save(projections_cbs, file = paste(getMYFFDir(),"/Weekly_Forecast/",folder,"/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_cbs, file=paste(getMYFFDir(),"/Weekly_Forecast/",folder,"/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}

getCBS_Dave_Projections = function(week){
  return(getCBS_Projections(week,TRUE))
}

getCBS_Jamey_Projections = function(week){
  return(getCBS_Projections(week,FALSE))
}

getCBS_Projections = function(week,isDave=FALSE){
  folder = "CBS_Jamey"
  if(isDave) { folder = "CBS_Dave" }
  ret = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/",folder,"/Projections_Week_",week,"_Date_",sep=""))
  ret$source = folder
  ret$week = week
  ret = ret[,c(1,3,2,4:ncol(ret))]
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}

