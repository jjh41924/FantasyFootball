FF.TRADE.EVAL.get.selected.players.in.roster = function(roster) {
  rownames(roster) = 1:nrow(roster)
  print(roster)
  cat("\n\n[Evaluate Trade] Please input indicies of players involved in trade\n")
  inds = as.numeric(scan())
  if(sum(duplicated(inds))) { stop("[FF.TRADE.EVAL.get.selected.players.in.roster] Duplicate Entries") }
  if(sum(inds>nrow(roster))>0 || sum(inds < 0)>0) { stop("[FF.TRADE.EVAL.get.selected.players.in.roster] Entries Out of Range") }
  return(roster[inds,])
}

FF.TRADE.EVAL.print.evaluation = function(opp,my,p) {
  p$Player = sapply(p$Player,function(X) paste(str_split(X," ")[[1]][1:2],collapse = " "))
  p$Player.Name = nameMerge(p$Player)
  p$Player.Name[p$Player.Name=="CHRISTOPHERIVORY"] = "CHRISIVORY"
  p$Player.Name[p$Player.Name=="STEVEJOHNSON"] = "STEVIEJOHNSON"
  opp = merge(opp,p,by="Player.Name", all.x=TRUE)
  my = merge(my,p,by="Player.Name", all.x=TRUE)
  opp = opp[order(opp$Avg),]
  my = my[order(my$Avg),]
  print(opp)
  cat("\n\n")
  print(my)
}

FF.TRADE.EVAL.evaluate.trade = function(rosters,ranks) {
  opp.roster = FF.TRADE.getUser.trade.index(rosters)
  browser()
  my.roster = rosters[grepl("jim hamil",tolower(rosters$Team.Name)),]
  opp.players = FF.TRADE.EVAL.get.selected.players.in.roster(opp.roster)
  my.players = FF.TRADE.EVAL.get.selected.players.in.roster(my.roster)
  cat("\n\n\n[Evaluate Trade]  Just the Trade:\n\n")
  FF.TRADE.EVAL.print.evaluation(my.players,opp.players,ranks)
  cat("\n\n[Evaluate Trade]  Both Full Teams:\n")
  FF.TRADE.EVAL.print.evaluation(my.roster,opp.roster,ranks)
}

FF.TRADE.getUser.trade.index = function(rosters) {
  uTeams = sort(unique(rosters$Team.Name))
  uTeams = uTeams[!grepl("Jim Hamilton",uTeams)]
  uTeams = data.frame(OPPONENT=uTeams)
  print(uTeams)
  cat("\n\n[Evaluate Trade] Please input opposing team index\n")
  opp.ind = as.numeric(readline())
  if(opp.ind <0 || opp.ind > nrow(uTeams)) { stop("[Evaluate Trade] Team Index Out of Range")}
  return(rosters[rosters$Team.Name==uTeams[opp.ind,1],])
}

FF.TRADE.PROPOSE.show.all.players = function(rosers,ranks,pos=c("RB","WR","TE")) {
  opp.roster = FF.TRADE.getUser.trade.index(rosters)
  my.roster = rosters[grepl("jim hamilton",tolower(rosters$Team.Name)),]
  FF.TRADE.PROPOSE.show.all.players.on.team(opp.roster,ranks,pos=c("RB","WR","TE"))
  cat("\n\n")
  FF.TRADE.PROPOSE.show.all.players.on.team(my.roster,ranks,pos=c("RB","WR","TE"))
}

FF.TRADE.PROPOSE.show.all.players.on.team = function(team.roster,ranks, pos) {
  opp = team.roster
  p = ranks
  p$Player = sapply(p$Player,function(X) paste(str_split(X," ")[[1]][1:2],collapse = " "))
  p$Player.Name = nameMerge(p$Player)
  p$Player.Name[p$Player.Name=="CHRISTOPHERIVORY"] = "CHRISIVORY"
  p$Player.Name[p$Player.Name=="STEVEJOHNSON"] = "STEVIEJOHNSON"
  opp = merge(opp,p,by="Player.Name", all.x=TRUE)
  opp = opp[order(opp$Avg),]
  opp=opp[!is.na(opp$Pos),]
  opp = opp[str_sub(opp$Pos,1,2) %in% pos,]
  print(opp[order(opp$Pos),])
}
FF.TRADE.PROPOSE.show.all.players.on.all.teams = function(rosters,ranks,pos) {
  teams = unique(rosters$Team.Name)
  for(team in teams) {
    FF.TRADE.PROPOSE.show.all.players.on.team(rosters[rosters$Team.Name==team,],ranks,pos=pos)
    cat("\n\n")
  }
}

WEEK=16
FF.TRADE.EVAL.evaluate.trade(scrape.yahoo.league.roster(WEEK),getFantasyPros_Half_PPR_ROS_Rankings(WEEK))
FF.TRADE.EVAL.evaluate.trade(scrape.espn.league.roster(),getFantasyPros_PPR_ROS_Rankings(WEEK))

FF.TRADE.PROPOSE.show.all.players.on.team()

ranks = getFantasyPros_PPR_ROS_Rankings(WEEK)
FF.TRADE.PROPOSE.show.all.players.on.all.teams(scrape.espn.league.roster(),ranks)
FF.TRADE.PROPOSE.show.all.players.on.all.teams(scrape.espn.league.roster(),ranks,pos=c("RB"))
FF.TRADE.PROPOSE.show.all.players.on.all.teams(scrape.espn.league.roster(),ranks,pos=c("WR"))
#FF.TRADE.PROPOSE.show.all.players.on.all.teams(scrape.espn.league.roster(),ranks,pos=c(""))