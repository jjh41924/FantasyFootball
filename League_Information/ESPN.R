 library("XML")
library("stringr")
library("ggplot2")
library("plyr")

format.espn.team = function(t,team.name) {
  char.vec = unlist(strsplit(t[1,1],split=" "))
#   team.name = paste(char.vec[1:(length(char.vec)-1)],collapse = " ")
  t = t[2:nrow(t),]
  player.names = format.espn.player.names(t[,2])
  player.team.names = format.espn.player.team.name(t[,2])
  return(cbind(as.character(team.name),player.names,player.team.names))
}

format.espn.player.names = function(nam) {
  ret = array()
  for(i in 1:length(nam)) {
    if(nam[i]!="") {
      if(grepl(",",nam[i])) {
        ret = c(ret,nameMerge(strsplit(nam[i],",")[[1]][1]))
      } else if(grepl("D/ST",nam[i])) { 
        ret = toupper(c(ret,strsplit(nam[i]," ")[[1]][1]))
      } else {
        browser()
        #DONT CARE
      }
    }
  }
  return(ret[2:length(ret)])
}

format.espn.player.team.name = function(nam) {
  ret = array()
  for(i in 1:length(nam)) {
    if(nam[i]!="") {
      if(grepl(",",nam[i])) {
        team = strsplit((strsplit(nam[i],",")[[1]][2]),"\\s")[[1]][2]
        ret = c(ret,team)
      } else if(grepl("D/ST",nam[i])) { 
        ret = c(ret,"")
      } else {
        browser()
        #DONT CARE
      }
    }
  }
  ret = toupper(ret[2:length(ret)])
  ret[ret=="WSH"] = "WAS"
  return(ret)
}

scrape.espn.league.roster = function(url="C:/My_GIT_DIR/League_Information/ESPN Rosters/20151022.html") {
  espn = readHTMLTable(url, stringsAsFactors = FALSE)
#   browser()
  team.map = as.data.frame(matrix(NA,ncol=2,nrow=0));
  team.names = espn[[1]][which("SLOT"==espn[[1]][,1])-1,1]
  for(i in 3:length(espn)) {
    team.map = rbind(team.map,format.espn.team(espn[[i]],team.names[i-2]))
  }
  team.map[,1] = as.character(team.map[,1])
  team.map[,3] = as.character(team.map[,3])
  colnames(team.map)=c("Team.Name","Player.Name","Player.Team.Name")
  return(team.map)
}


# This is not for roster, but for the viewing times of the games. It is used in the "Weekly_Lineup.R"
scrape.espn.viewing.times = function(week) {
  the.datas = list()
  the.datas[[1]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=0&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[2]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[3]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=40",sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[4]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=2&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=80",sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[5]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[6]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=40",sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[7]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=4&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),"&startIndex=80",sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[8]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=6&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[9]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=16&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  the.datas[[10]] <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?&slotCategoryId=17&scoringPeriodId=",week,"&seasonId=",format(Sys.time(),"%Y"),sep=""), stringsAsFactors = FALSE)$playertable_0
  
  all.data = data.frame(Player.Name=NA,Team=NA,Opp=NA,Game.Time=NA)
  for(i in 1:length(the.datas)) {
    temp = the.datas[[i]][,1:3]
    temp = temp[2:nrow(temp),]
    player.names = format.espn.player.names(temp[,1])
    player.team.names = format.espn.player.team.name(temp[,1])
    all.data = rbind(all.data,cbind(Player.Name=player.names,Team=player.team.names,Opp=temp[,2],Game.Time=temp[,3]))
  }
  return(all.data[-1,])
}

scrape.uniform.numbers = function() {
  urls = NA
  urls[1] = "http://www.nfl.com/players/search?category=position&filter=quarterback&playerType=current&conference=ALL"
  urls[2] = "http://www.nfl.com/players/search?category=position&filter=quarterback&playerType=current&conference=ALL&d-447263-p=2"
  urls[3] = "http://www.nfl.com/players/search?category=position&filter=runningback&conferenceAbbr=null&playerType=current&conference=ALL"
  urls[4] = "http://www.nfl.com/players/search?category=position&filter=runningback&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=2"
  urls[5] = "http://www.nfl.com/players/search?category=position&filter=runningback&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=3"
  urls[6] = "http://www.nfl.com/players/search?category=position&filter=runningback&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=4"
  urls[7] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL"
  urls[8] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=2"
  urls[9] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=3"
  urls[10] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=4"
  urls[11] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=5"
  urls[12] = "http://www.nfl.com/players/search?category=position&filter=widereceiver&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=6"
  urls[13] = "http://www.nfl.com/players/search?category=position&filter=tightend&conferenceAbbr=null&playerType=current&conference=ALL"
  urls[14] = "http://www.nfl.com/players/search?category=position&filter=tightend&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=2"
  urls[15] = "http://www.nfl.com/players/search?category=position&filter=tightend&conferenceAbbr=null&playerType=current&conference=ALL&d-447263-p=3"
  
  ret = data.frame(Pos=NA,Jersey.Number=NA,Player.Name=NA,Team=NA)
  #
  for(url in urls) {
    num = readHTMLTable(url, stringsAsFactors = FALSE)[[4]][,c(1,2,3,13)]
    colnames(num) = c("Pos","Jersey.Number", "Player.Name", "Team")
    num$Player.Name = nameMerge(sapply(num$Player.Name, function(x) { paste(str_trim(str_split(x,pattern = ",")[[1]][c(2,1)]),collapse = " ") }))
    ret = rbind(ret,num)
  }
  return(ret[-1,])
}
  



