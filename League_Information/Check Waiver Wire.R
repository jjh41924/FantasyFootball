source('C:/My_GIT_DIR/FantasyFootballUtilities.R')
source('C:/My_GIT_DIR/League_Information/ESPN.R')
#source('C:/My_GIT_DIR/League_Information/Family League Settings.R')
source('C:/My_GIT_DIR/League_Information/Work League Settings.R')
source('C:/My_GIT_DIR/League_Information/YAHOO.R')
source('C:/My_GIT_DIR/Weekly_Forecast/Scripts/FantasyPros Rankings.R')

FF.WAIVER.check.player.names = function(suggested.pickups.v,team.map) {
#    browser()
  #test      suggested.pickups = c(as.character(team.map$Player.Name)[sample(1:nrow(team.map),3)],as.character(proj$name_lookup)[sample(1:nrow(proj),3)])
  suggested.pickups = data.frame(Player.Name=suggested.pickups.v,Rank=1:length(suggested.pickups.v))
  suggested.pickups$Rank = as.numeric(suggested.pickups$Rank)
  suggested.pickups = merge(x=suggested.pickups,y=team.map,by="Player.Name",all.x = TRUE)
  suggested.pickups[is.na(suggested.pickups)]="AVAILABLE"
  suggested.pickups = suggested.pickups[order(suggested.pickups$Rank),]
  return(suggested.pickups)
}
FF.WAIVER.check.def.names = function(suggested.pickups.v,team.map) {
#   browser()
  l = length(suggested.pickups.v)
  ret = data.frame(DEF=suggested.pickups.v, Rank = 1:l, Team.Name = rep(NA,l))
  matches = sapply(tolower(as.character(team.map$Player.Name)),function(x) { grepl(x,tolower(as.character(ret$DEF))) })
  ret$Team.Name = apply(matches,1,function(x) { paste(team.map$Team.Name[x],collapse = " ") })
  ret$Team.Name[ret$Team.Name==""] = "AVAILABLE"
  return(ret)
}

FF.WAIVER.check.availability = function(team.map, p, week) {
#     browser()
  ret = FF.WAIVER.check.player.names(p$Player.Name,team.map)
  #   browser()
  if("Rank" %in% colnames(p) && "Rank" %in% colnames(ret)) { 
    if(sum(p$Rank != ret$Rank) > 0) { stop("ERROR: Ranks dont match up") }
    else { ret = ret[,-which(colnames(ret) == "Rank")] }
  }
#   browser()
  ret = merge(ret,p,by="Player.Name")
  ret = ret[order(ret$Rank),]
  cat("\n\n\n\n")
  cat("                             ********************************************************\n")
  cat("                             ********************** Waiver Wire *********************\n")
  cat("                             ********************************************************\n\n\n")
  cat("                                                 QB - RB - WR - TE\n\n")
  print(ret[grepl("jim hamil",tolower(ret$Team.Name)) | ret$Team.Name == "AVAILABLE",])
  cat("\n\n")
  cat("                                                      Kicker\n\n")
#   browser()
  k.all = getFantasyPros_Kicker_Rankings(week)
  k = FF.WAIVER.check.player.names(k.all$Player.Name,team.map)
  if("Rank" %in% colnames(k) && "Rank" %in% colnames(k.all)) { 
    if(sum(k$Player.Name != k.all$Player.Name) > 0) { stop("ERROR: Player.Name dont match up") }
    else {
      k = cbind(k[-5],ROS_Rank=k.all$"ROS:Rank")
    }
  }
  print(k[grepl("jim hamil",tolower(k$Team.Name)) | k$Team.Name == "AVAILABLE",])
  cat("\n\n")
  cat("                                                        DST\n\n")
  def.all = getFantasyPros_DST_Rankings(week)
  def = FF.WAIVER.check.def.names(def.all$Player,team.map)
  if("Rank" %in% colnames(def) && "Rank" %in% colnames(def.all)) { 
    if(sum(def$DEF != def.all$Player) > 0) { stop("ERROR: Player.Name dont match up") }
    else {
      def = cbind(def[-5],ROS_Rank=def.all$"ROS:Rank")
    }
  }
  print(def[grepl("jim hamil",tolower(def$Team.Name)) | def$Team.Name == "AVAILABLE",])
}


WEEK=16
# ESPN
FF.WAIVER.check.availability(scrape.espn.league.roster(),getFantasyPros_PPR_ROS_Rankings(WEEK), WEEK)
# WORK
FF.WAIVER.check.availability(scrape.yahoo.league.roster(WEEK),getFantasyPros_Half_PPR_ROS_Rankings(WEEK), WEEK)
