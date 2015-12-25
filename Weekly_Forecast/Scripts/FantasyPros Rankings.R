source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))

FF.FantasyPros.Parse.Kicker.Table = function(p) {
#     browser()
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
#     browser()
  p$Player = sapply(p$Player,function(X) paste(str_split(X," ")[[1]][1:2],collapse = " "))
  p$Player.Name = nameMerge(p$Player)
  p$Player.Name[p$Player.Name=="CHRISTOPHERIVORY"] = "CHRISIVORY"
  p$Player.Name[p$Player.Name=="STEVEJOHNSON"] = "STEVIEJOHNSON"
  colnames(p)[1:8] = c("Rank", "Player","Pos","Bye","Best","Worst","Avg","Std")
  p = p[!grepl("googletag.",p[,1]),]
  return(p)
}

FF.FantasyPros.Set.Kicker = function(week) {
  num = FF.FantasyPros.Parse.Kicker.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-k.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_ROS_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_ROS_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  
  num = FF.FantasyPros.Parse.Kicker.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/k.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros K] DONE. week[",week,"]\n",sep=""))
}

FF.FantasyPros.Set.DST = function(week) {
  num = FF.FantasyPros.Parse.DST.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-dst.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_ROS_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_ROS_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  
  num = FF.FantasyPros.Parse.DST.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/dst.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros DST] DONE. week[",week,"]\n",sep=""))
}

FF.FantasyPros.Set.ppr.rest.of.season.rankings = function(week) {
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_NW_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_NW_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros PPR] DONE. week[",week,"]\n",sep=""))
}
FF.FantasyPros.Set.half.ppr.rest.of.season.rankings = function(week) {
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/ros-half-point-ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  
  num = FF.FantasyPros.Parse.Table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/half-point-ppr-qb-flex.php", stringsAsFactors = FALSE)$data)
  save(num, file = paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_NW_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(num, file=paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_NW_Rankings_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
  cat(paste("[FantasyPros Half_PPR] DONE. week[",week,"]\n",sep=""))
}


getFantasyPros_PPR_ROS_Rankings = function(week){
  FF.FantasyPros.Set.ppr.rest.of.season.rankings(week)
  ros = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_Rankings_Week_",week,"_Date_",sep=""))
  nw = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/PPR_NW_Rankings_Week_",week,"_Date_",sep=""))
  colnames(nw)[-c(2,9)] = paste0("NW:",colnames(nw)[-c(2,9)])
  m = merge(x=ros,y=nw,by=c("Player","Player.Name"),all.x=TRUE)
  return(m[order(m$Rank),])
}
getFantasyPros_Half_PPR_ROS_Rankings = function(week){
  FF.FantasyPros.Set.half.ppr.rest.of.season.rankings(week)
  ros = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_Rankings_Week_",week,"_Date_",sep=""))
  nw = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Half_PPR_NW_Rankings_Week_",week,"_Date_",sep=""))
  colnames(nw)[-c(2,9)] = paste0("NW:",colnames(nw)[-c(2,9)])
  m = merge(x=ros,y=nw,by=c("Player","Player.Name"),all.x=TRUE)
  return(m[order(m$Rank),])
}
getFantasyPros_Kicker_Rankings = function(week){
  FF.FantasyPros.Set.Kicker(week)
  ros = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_ROS_Rankings_Week_",week,"_Date_",sep=""))
  nw = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/Kicker_Rankings_Week_",week,"_Date_",sep=""))
  colnames(ros)[-c(2,7)] = paste0("ROS:",colnames(ros)[-c(2,7)])
  m = merge(x=nw,y=ros,by=c("Player","Player.Name"),all=TRUE)
  return(m[order(m$Rank),])
}
getFantasyPros_DST_Rankings = function(week){
  FF.FantasyPros.Set.DST(week)
  ros = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_ROS_Rankings_Week_",week,"_Date_",sep=""))
  nw = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FantasyPros_Rankings/DST_Rankings_Week_",week,"_Date_",sep=""))
  colnames(ros)[-2] = paste0("ROS:",colnames(ros)[-2])
  m = merge(x=nw,y=ros,by="Player",all=TRUE)
  return(m[order(m$Rank),])
}
