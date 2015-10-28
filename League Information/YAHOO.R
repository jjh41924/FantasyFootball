library("XML")
library("stringr")
library("ggplot2")
library("plyr")


YAHOO.get.team.name = function(p,map) {
  team.name = unique(as.character(map[as.character(map$Player.Name) %in% as.character(p),2]))
  if(length(team.name)>1) { stop("[YAHOO.get.team.name] Unable to find a team match") }
  return(team.name)
}

YAHOO.get.defence = function(d) {
#   browser()
  toupper(unlist(lapply(str_split(d," "),function(X) {
    end = which(X=="-")-2
    return(paste(X[1:end],collapse = " "))
  })))
}

YAHOO.getTeam = function(yt,ptmap) {
  yt = yt[yt[,2]!="(Empty)",]
  ret = as.data.frame(matrix(NA,nrow=14,ncol=6))
  colnames(ret) = c("Team.Name","Player.Name","Player.Team.Name","temp","name_yahoo")
#   ret$temp = sapply(yt[,2],function(X) { 
#         return(str_trim(str_split(X, "\n")[[1]][2])) 
#     })
  ret$temp = str_trim(sapply(str_split(yt[,2], "\n"), "[[", 2))
# browser()
  ret$pos = str_trim(str_sub(ret$temp, start= regexpr("\\-[^\\-]*$", ret$temp)+2))
#   ret$pos <- str_trim(str_sub(ret$temp, start= -2))
#   ret = ret[!(ret$pos%in%c("DEF","K")),]
# browser()
  ret$name_yahoo = lapply(str_split(ret$temp," "),function(x) { end = which(x=="-")-2; return(paste(x[1:end],collapse = " ")) })
#   ret$name_yahoo <- str_trim(str_sub(ret$temp, start=0, end=nchar(ret$temp)-8))
  ret$Player.Name <- nameMerge(ret$name_yahoo)
  ret$Player.Name[ret$pos=="DEF"] = YAHOO.get.defence(ret$temp[ret$pos=="DEF"])
#   ret$Player.Name[ret$pos=="DEF"] = toupper(str_sub(ret$temp[ret$pos=="DEF"], start=1, end=str_locate(ret$temp[ret$pos=="DEF"]," ")[1]-1))
  ret$Player.Team.Name <- toupper(str_trim(str_sub(ret$temp, start=str_locate(ret$temp, "-")[,1]-4, end=str_locate(ret$temp, "-")[,1]-2)))
  ret$Team.Name = YAHOO.get.team.name(ret$Player.Name,ptmap)
  return(ret[,c("Team.Name","Player.Name","Player.Team.Name")])
}

YAHOO.get.player.to.team = function(yp) {
  yp = yp[yp[,1]!="(Empty)",]
  temp = str_trim(sapply(str_split(yp[,1], "\n"), "[[", 2))
  temp = str_trim(str_sub(temp, start=0, end=nchar(temp)-8))
  name = nameMerge(temp)
  team = yp[,2] 
  team = substr(team,1,str_length(team)-4)
  return(cbind(name,team))
}

# YAHOO.get.player.to.team = function(yp) {
#   temp = str_trim(sapply(str_split(yp[,2], "\n"), "[[", 2))
#   name = str_trim(str_sub(temp, start=0, end=nchar(temp)-8))
#   name = nameMerge(name)
#   team = sapply(temp,function(X) { str_split(X,pattern = " - ")[[1]][1] })
#   team = toupper(sapply(team,function(X) { a=str_split(X,pattern = " ")[[1]]; return(a[length(a)]) }))
#   return(cbind(name,team))
# }

YAHOO.get.player.to.team.map = function(week) {
  url = paste("http://football.fantasysports.yahoo.com/f1/288510/starters?week=",week,"&startertab=position")
  yahoo = readHTMLTable(url, stringsAsFactors = FALSE)
  map = as.data.frame(matrix(NA,nrow=0,ncol=2))
  for(i in 2:9) {
    map = rbind(map,YAHOO.get.player.to.team(yahoo[[i]]))  
  }
  colnames(map) = c("Player.Name","Player.Team.Name")
  return(map)
}

scrape.yahoo.league.roster = function(week) {
  map = YAHOO.get.player.to.team.map(week)
  url = "http://football.fantasysports.yahoo.com/f1/288510/starters"
  yahoo = readHTMLTable(url, stringsAsFactors = FALSE)
  #Team.Name           Player.Name Player.Team.Name
  teams = as.data.frame(matrix(NA,nrow=0,ncol=3)); 
  colnames(teams) = c("Team.Name","Player.Name","Player.Team.Name")
  for(i in 2:15) {
    teams = rbind(teams,YAHOO.getTeam(yahoo[[i]],map))
  }
  return(teams)
}