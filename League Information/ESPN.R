library("XML")
library("stringr")
library("ggplot2")
library("plyr")

format.espn.team = function(t) {
  char.vec = unlist(strsplit(t[1,1],split=" "))
  team.name = paste(char.vec[1:(length(char.vec)-1)],collapse = " ")
  t = t[3:nrow(t),]
  player.names = format.espn.player.names(t[,2])
  return(cbind(team.name,player.names))
}

format.espn.player.names = function(nam) {
  ret = array()
  for(i in 1:length(nam)) {
    if(nam[i]!="") {
      if(grepl(",",nam[i]) & sum(strsplit(nam[i],"\\s")[[1]] =="K") == 0) {
        ret = c(ret,nameMerge(strsplit(nam[i],",")[[1]][1]))
      } else {#Defence Or Kicker
        #DONT CARE
      }
    }
  }
  return(ret[2:length(ret)])
}

scrape.espn.league.roster = function(url="C:/ESPNTEST.html") {
  espn = readHTMLTable(url, stringsAsFactors = FALSE)
  
  team.map = as.data.frame(matrix(NA,ncol=2,nrow=0));colnames(team.map)=c("Team Name","Player Name")
  for(i in 3:length(espn)) {
    team.map = rbind(team.map,format.espn.team(espn[[i]]))
  }
  return(team.map)
}


