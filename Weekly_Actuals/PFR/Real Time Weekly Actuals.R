
#   https://football.fantasysports.yahoo.com/f1/288510/players?&sort=PR&sdir=1&status=A&pos=O&stat1=S_W_9&jsenabled=1



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


setWeekActuals_actual = function(week){
  
  #Suffix
  suffix <- "actual"
  
  #Download fantasy football projections from Yahoo.com
  actuals = NULL
  for(i in 0:14) {
    #2014#html = paste("http://football.fantasysports.yahoo.com/f1/288510/players?status=A&pos=O&cut_type=9&stat1=S_PW_",week,"&myteam=0&sort=PTS&sdir=1&count=",(25*i),sep="")
    html = paste("http://football.fantasysports.yahoo.com/f1/288510/players?status=ALL&pos=O&cut_type=9&stat1=S_W_",week,"&myteam=0&sort=R_PO&sdir=1&count=",(25*i), sep="")
    if(i==0){
      #remove the PW and replace with W for actuals
      actuals = readHTMLTable(html, stringsAsFactors = FALSE)[2]$'NULL'
    } else {
      actuals = rbind(actuals,readHTMLTable(html, stringsAsFactors = FALSE)[2]$'NULL')
    }
  }
  
  #Variable Names
  rename.cols =  c("star","player","add","owner","GP","pts_actual","ownedPct","proj","actual",
                   "passYds_actual","passTds_actual","passInt_actual","rushAtt_actual","rushYds_actual","rushTds_actual","passAtt_actual","rec_actual","recYds_actual","recTds_actual","returnTds_actual","twoPts_actual","fumbles_actual","missing")
  names(actuals) <-rename.cols
  
  #Add missing variables
  actuals$passComp_actual <- NA
  
  #Remove special characters(commas)
  actuals[,c("passAtt_actual","passComp_actual","passYds_actual","passTds_actual","passInt_actual","rushAtt_actual","rushYds_actual","rushTds_actual","rec_actual","recYds_actual","recTds_actual","returnTds_actual","twoPts_actual","fumbles_actual","pts_actual")] <-
    apply(actuals[,c("passAtt_actual","passComp_actual","passYds_actual","passTds_actual","passInt_actual","rushAtt_actual","rushYds_actual","rushTds_actual","rec_actual","recYds_actual","recTds_actual","returnTds_actual","twoPts_actual","fumbles_actual","pts_actual")], 2, function(x) gsub("\\,", "", x))
  
  #Convert variables from character strings to numeric
  actuals[,c("passAtt_actual","passComp_actual","passYds_actual","passTds_actual","passInt_actual","rushAtt_actual","rushYds_actual","rushTds_actual","rec_actual","recYds_actual","recTds_actual","returnTds_actual","twoPts_actual","fumbles_actual","pts_actual")] <- 
    convert.magic(actuals[,c("passAtt_actual","passComp_actual","passYds_actual","passTds_actual","passInt_actual","rushAtt_actual","rushYds_actual","rushTds_actual","rec_actual","recYds_actual","recTds_actual","returnTds_actual","twoPts_actual","fumbles_actual","pts_actual")], "numeric")
  
  #Player name, position, and team
  actuals$player <- str_trim(sapply(str_split(actuals$player, "\n"), "[[", 2))
  actuals$pos <- str_trim(str_sub(actuals$player, start= -2))
  actuals$name_actual <- str_trim(str_sub(actuals$player, start=0, end=nchar(actuals$player)-8))
  actuals$name <- nameMerge(actuals$name_actual)
  actuals$team_actual <- toupper(str_trim(str_sub(actuals$player, start=str_locate(actuals$player, "-")[,1]-4, end=str_locate(actuals$player, "-")[,1]-2)))
  
  #Remove duplicate cases
  actuals[actuals$name %in% actuals[duplicated(actuals$name),"name"],]
  #actuals <- actuals[-which(actuals$name_actual=="Dexter McCluster" & actuals$pos=="RB"),]
  
  #Rename players
  actuals[actuals$name=="STEVIEJOHNSON", "name"] <- "STEVEJOHNSON"
  
  #Calculate overall rank
  actuals$pts_actual = as.numeric(actuals$pts_actual)
  actuals$overallRank_actual <- rank(-actuals$pts_actual, ties.method="min")
  
  #Calculate Position Rank
  actuals$positionRank_actual <- NA
  actuals[which(actuals$pos == "QB"), "positionRank_actual"] <- rank(-actuals[which(actuals$pos == "QB"), "pts_actual"], ties.method="min")
  actuals[which(actuals$pos == "RB"), "positionRank_actual"] <- rank(-actuals[which(actuals$pos == "RB"), "pts_actual"], ties.method="min")
  actuals[which(actuals$pos == "WR"), "positionRank_actual"] <- rank(-actuals[which(actuals$pos == "WR"), "pts_actual"], ties.method="min")
  actuals[which(actuals$pos == "TE"), "positionRank_actual"] <- rank(-actuals[which(actuals$pos == "TE"), "pts_actual"], ties.method="min")
  
  #Order variables in data set
  actuals <- actuals[,c(prefix, paste(varNames, suffix, sep="_"))]
  
  #Order players by overall rank
  actuals <- actuals[order(actuals$overallRank_actual),]
  row.names(actuals) <- 1:dim(actuals)[1]
  
  # #Density Plot
  # ggplot(actuals, aes(x=pts_actual)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Yahoo Projected Points")
  # ggsave(paste(getwd(),"/Figures/Yahoo projections.jpg", sep=""), width=10, height=10)
  # dev.off()
  
  #Save file  
  save(actuals, file = paste(getMYFFDir(),"/Weekly_Actuals/Yahoo/Actuals_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d %H%M"),".RData", sep=""))
  write.csv(actuals, file=paste(getMYFFDir(),"/Weekly_Actuals/Yahoo/Actuals_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d %H%M"),".csv", sep=""), row.names=FALSE)
  
  cat(paste("[Yahoo Actuals] DONE. week[",week,"]\n",sep=""))
}
  
getActuals = function(prefix){
  sp = strsplit(prefix, "/")[[1]]
  last = sp[length(sp)]
  directory = substring(prefix,1,nchar(prefix)-nchar(last))
  files = list.files(directory, pattern="*.csv", full.names=TRUE)
  files = files[grepl(prefix,files)]
  dates = rep(Sys.time(),length(files))
  for(i in 1:length(files)) {
    dates[i] = strptime(substring(files[i], nchar(files[i])-18,nchar(files[i])-4),"%Y_%m_%d %H%M")
  }
  file = files[dates==max(dates)][1]
  return(read.table(file,sep=",",header = TRUE))
}

getWeekActuals_actual = function(week){
  ret = getActuals(paste(getMYFFDir(),"/Weekly_Actuals/Yahoo/Actuals_Week_",week,"_Date_",sep=""))
  ret$source = "Yahoo"
  ret$week = week
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}