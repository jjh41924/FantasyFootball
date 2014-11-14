

#Library
library("psy")
library("psych")
library("ggplot2")
library("forecast")
library("XML")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))


#####################
# 1. Scrape and Process Historical Actual Points
#####################
createActualsFiles = function(week){
  year <- 2014 #can only scrape Yahoo data from past 2 years (2012 or 2013), but can load data from other years if already scraped & saved
  yahooLeagueID <- 39345
  pagesToGrab <- 15

  pb <- txtProgressBar(min = 0, max = pagesToGrab, style = 3)
  #for(week in 1:weeks) {
    for(i in 1:pagesToGrab){
      #setTxtProgressBar(pb, i)
      if(i > 1){
        count <- 25*(i-1)
        assign(paste("yahoo", i, sep=""), readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/", yahooLeagueID, "/players?&stat1=S_W_",week,"&jsenabled=1&jsenabled=1&count=", count, sep=""), stringsAsFactors = FALSE)[2]$'NULL')
      } else if(i == 1){
        assign(paste("yahoo", i, sep=""), readHTMLTable(paste("http://football.fantasysports.yahoo.com/f1/", yahooLeagueID, "/players?&stat1=S_W_",week,"&jsenabled=1&jsenabled=1", sep=""), stringsAsFactors = FALSE)[2]$'NULL')
      }
    }
  #}
  
  #Merge
  actualPoints <- rbind(yahoo1,yahoo2,yahoo3,yahoo4,yahoo5,yahoo6,yahoo7,yahoo8,yahoo9,yahoo10,yahoo11,yahoo12,yahoo13,yahoo14,yahoo15)
  
  #Variable Names
  names(actualPoints) <- c("star","player","add","owner","pts","ownedPct","proj","actual","passYds","passTds","passInt","rushAtt","rushYds","rushTds","targets","rec","recYds","recTds","returnTDs","twoPts","fumbles")
  
  #Remove special characters(commas)
  actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")] <-
    apply(actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")], 2, function(x) gsub("\\,", "", x))
  
  #Convert variables from character strings to numeric
  actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")] <- 
    convert.magic(actualPoints[,c("passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts")], "numeric")
  
  #Player name, position, and team
  actualPoints$player <- str_trim(sapply(str_split(actualPoints$player, "\n"), "[[", 2))
  actualPoints$pos <- str_trim(str_sub(actualPoints$player, start= -2))
  actualPoints$name_yahoo <- str_trim(str_sub(actualPoints$player, start=0, end=str_locate(actualPoints$player, "-")[,1]-5))
  actualPoints$name <- nameMerge(actualPoints$name_yahoo)
  actualPoints$team_yahoo <- toupper(str_trim(str_sub(actualPoints$player, start=str_locate(actualPoints$player, "-")[,1]-4, end=str_locate(actualPoints$player, "-")[,1]-2)))
  
  #Select variables to keep
  actualPoints <- actualPoints[,c("name","name_yahoo","pos","team_yahoo","passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","returnTDs","twoPts","fumbles","pts","targets","rec","ownedPct","rushAtt")]
  
  #Save historical actual data
  write.csv(actualPoints, file=paste("C:/MY_FF/Weekly Actuals/Yahoo-actualpoints-Week-", week, ".csv", sep=""), row.names=FALSE)
}
getActualsFromFile = function(week){
  return(read.table(file=paste("C:/MY_FF/Weekly Actuals/Yahoo-actualpoints-Week-", week, ".csv", sep=""), sep=",",header=TRUE))
}
# 
# #####################
# # 2. Import Historical Actual Stats and Calculate Actual Fantasy Points
# #####################
# 
# actualPoints <- read.csv(paste(getwd(),"/Data/Historical Actual Points/Yahoo-actualpoints-", year, ".csv", sep=""))
# 
# #Calculate actual fantasy points for your league based on actual stats
# actualPoints$passYdsPts <- actualPoints$passYds * passYdsMultiplier
# actualPoints$passTdsPts <- actualPoints$passTds * passTdsMultiplier
# actualPoints$passIntPts <- actualPoints$passInt * passIntMultiplier
# actualPoints$rushYdsPts <- actualPoints$rushYds * rushYdsMultiplier
# actualPoints$rushTdsPts <- actualPoints$rushTds * rushTdsMultiplier
# actualPoints$recYdsPts <- actualPoints$recYds * recYdsMultiplier
# actualPoints$recTdsPts <- actualPoints$recTds * recTdsMultiplier
# actualPoints$fumblesPts <- actualPoints$fumbles * fumlMultiplier
# 
# actualPoints$actualPts <- rowSums(actualPoints[,c("passYdsPts","passTdsPts","passIntPts","rushYdsPts","rushTdsPts","recYdsPts","recTdsPts","twoPts","fumblesPts")], na.rm=T)
# 
# actualPoints <- actualPoints[,c("name","name_yahoo","pos","team_yahoo","actualPts")]
# row.names(actualPoints) <- 1:dim(actualPoints)[1]
# 
# write.csv(actualPoints, file=paste(getwd(),"/Data/Historical Actual Points/Yahoo-actualpoints-", year, "-formatted.csv", sep=""), row.names=FALSE)
