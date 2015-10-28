source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))

FF.FantasyPros.Parse.Kicker.Table = function(p) {
  #   browser()
  p$Player = sapply(p$Player,function(X) paste(str_split(X," ")[[1]][1:2],collapse = " "))
  p$Player.Name = nameMerge(p$Player)
  p$Player.Name[p$Player.Name=="CHRISTOPHERIVORY"] = "CHRISIVORY"
  p$Player.Name[p$Player.Name=="STEVEJOHNSON"] = "STEVIEJOHNSON"
  colnames(p)[1:7] = c("Rank", "Player","DELETE","Best","Worst","Avg","Std")
  p = p[!grepl("googletag.",p[,1]),]
  p=p[,-3]
  return(p)
}
FF.FantasyPros.Parse.DST.Table = function(p) {
  colnames(p) = c("Rank", "Player","DELETE","Best","Worst","Avg","Std")
  p = p[!grepl("googletag.",p[,1]),]
  p=p[,-3]
  return(p)
}
FF.FantasyPros.Parse.Table = function(p) {
  #   browser()
  p$Player = sapply(p$Player,function(X) paste(str_split(X," ")[[1]][1:2],collapse = " "))
  p$Player.Name = nameMerge(p$Player)
  p$Player.Name[p$Player.Name=="CHRISTOPHERIVORY"] = "CHRISIVORY"
  p$Player.Name[p$Player.Name=="STEVEJOHNSON"] = "STEVIEJOHNSON"
  colnames(p)[1:8] = c("Rank", "Player","Pos","Bye","Best","Worst","Avg","Std")
  p = p[!grepl("googletag.",p[,1]),]
  return(p)
}

FF.FantasyPros.Set.Kicker = function(week) {
  num = FF.FantasyPros.Parse.Kicker.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/k.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Kicker_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Kicker_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros K] DONE. week[",week,"]\n",sep=""))
}

FF.FantasyPros.Set.DST = function(week) {
  num = FF.FantasyPros.Parse.DST.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/dst.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/DST_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/DST_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros DST] DONE. week[",week,"]\n",sep=""))
}

FF.FantasyPros.Set.ppr.rest.of.season.rankings = function(week) {
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros PPR] DONE. week[",week,"]\n",sep=""))
}
FF.FantasyPros.Set.half.ppr.rest.of.season.rankings = function(week) {
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-half-point-ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Half_PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Half_PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros Half_PPR] DONE. week[",week,"]\n",sep=""))
}


getFantasyPros_PPR_ROS_Rankings = function(week){
  FF.FantasyPros.Set.ppr.rest.of.season.rankings(week)
  return(getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/PPR_Rankings_Week_",week,"_Date_",sep="")))
}
getFantasyPros_Half_PPR_ROS_Rankings = function(week){
  FF.FantasyPros.Set.half.ppr.rest.of.season.rankings(week)
  return(getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Half_PPR_Rankings_Week_",week,"_Date_",sep="")))
}
getFantasyPros_Kicker_Rankings = function(week){
  FF.FantasyPros.Set.Kicker(week)
  return(getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/Kicker_Rankings_Week_",week,"_Date_",sep="")))
}
getFantasyPros_DST_Rankings = function(week){
  FF.FantasyPros.Set.DST(week)
  return(getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros Rankings/DST_Rankings_Week_",week,"_Date_",sep="")))
}
