source('C:/My_GIT_DIR/FantasyFootballUtilities.R')
source('C:/My_GIT_DIR/League_Information/ESPN.R')
source('C:/My_GIT_DIR/League_Information/YAHOO.R')
proj=NA
# source('C:/My_GIT_DIR/League_Information/Family League Settings.R')
# source('C:/My_GIT_DIR/League_Information/Work League Settings.R')

FF.LINEUP.Family = function() {
  source('C:/My_GIT_DIR/League_Information/Family League Settings.R')
  proj = FF.EXP.PTS(proj)
  all.proj = FF.EXP.PTS(all.proj)
  team.map = scrape.espn.league.roster()
  print(unique(team.map$Team.Name))
  #prompt for teams selection
  
  opponent.name = "CONGRATS! U GET HI SCORE! (4-2)"
  my.team.name = "JIM HAMILTON'S JIM HAM... (5-1)"
  my.team = team.map[team.map$Team.Name==my.team.name,]
  opponent = team.map[team.map$Team.Name==opponent.name,]
  my.team.proj = merge(y = my.team,x=proj,by.y=c("Player.Name","Player.Team.Name"),by.x=c("name_lookup","team"))
  opponent.proj = merge(y = opponent,x=proj,by.y=c("Player.Name","Player.Team.Name"),by.x=c("name_lookup","team"))
  
  # Choosing Starting Lineup
  my.team.proj.starters = FF.GET.LINEUP(my.team.proj)
  opponent.proj.starters = FF.GET.LINEUP(opponent.proj)
  FF.MATCHUP.print(opponent.name,my.team.name,my.team.proj.starters,opponent.proj.starters,my.team.proj,opponent.proj)
  
  # Prompt For Player Printing
#   browser()
  #   FF.MATCHUP.study.user.input(my.team.proj.starters,opponent.proj.starters)
  
  # Print Football Watching Guide
  return(FF.MATCHUP.print.viewing.guide(my.team.proj.starters,opponent.proj.starters,my.team.proj,opponent.proj, "E"))
}


FF.LINEUP.Work = function() {
  source('C:/My_GIT_DIR/League_Information/Work League Settings.R')
  proj <- FF.EXP.PTS(proj)
  all.proj <- FF.EXP.PTS(all.proj)
  team.map = scrape.yahoo.league.roster(WEEK)
  print(unique(team.map$Team.Name))
  #prompt for teams selection
  
  opponent.name = "Mike Horn's Team"
  my.team.name = "The Jim Hamiltons"  
  my.team = team.map[team.map$Team.Name==my.team.name,]
  opponent = team.map[team.map$Team.Name==opponent.name,]  
  my.team.proj = merge(y = my.team,x=proj,by.y=c("Player.Name","Player.Team.Name"),by.x=c("name_lookup","team"))
  opponent.proj = merge(y = opponent,x=proj,by.y=c("Player.Name","Player.Team.Name"),by.x=c("name_lookup","team"))
  
  # Choosing Starting Lineup
  my.team.proj.starters = FF.GET.LINEUP(my.team.proj)
  opponent.proj.starters = FF.GET.LINEUP(opponent.proj)
  FF.MATCHUP.print(opponent.name,my.team.name,my.team.proj.starters,opponent.proj.starters,my.team.proj,opponent.proj)
  
  # Prompt For Player Printing
#   FF.MATCHUP.study.user.input(my.team.proj.starters,opponent.proj.starters)
  
  # Print Football Watching Guide
  return(FF.MATCHUP.print.viewing.guide(my.team.proj.starters,opponent.proj.starters,my.team.proj,opponent.proj, "Y"))
}

FF.MATCHUP.print.viewing.guide = function(my.starters, his.starters, my.whole.team, his.whole.team, league) {
  viewing.time = scrape.espn.viewing.times(WEEK)
  jersy.numbers = scrape.uniform.numbers()
  my.bench = my.whole.team[!(my.whole.team$name_lookup %in% my.starters$name_lookup),]
  his.bench = his.whole.team[!(his.whole.team$name_lookup %in% his.starters$name_lookup),]
  
  watch.list = rbind(my.whole.team[,c("name_lookup","team","pos","name","pts")],his.whole.team[,c("name_lookup","team","pos","name","pts")])
  watch.list$FF_Team = c(rep(paste("MINE",league,sep="_"),nrow(my.whole.team)), rep(paste("HIS",league,sep="_"),nrow(his.whole.team)))
  watch.list$IsStarter = watch.list$name_lookup %in% my.starters$name_lookup | watch.list$name_lookup %in% his.starters$name_lookup
  watch.list = merge(x=watch.list,y=viewing.time,by.x="name_lookup",by.y="Player.Name",all.x=TRUE)
#   if(sum(as.character(watch.list$team) != watch.list$Team)>0) { stop("[FF.MATCHUP.print.viewing.guide] ERROR: Teams dont match up between ESPN and Lineup") }
  watch.list = watch.list[,colnames(watch.list) != "team"]
  watch.list = merge(x=watch.list,y=jersy.numbers,by.x="name_lookup",by.y="Player.Name",all.x=TRUE)
#   if(sum(watch.list$Team.x != watch.list$Team.y)>0) { stop("[FF.MATCHUP.print.viewing.guide] ERROR: Teams dont match up between NFL and Lineup") }
#   if(sum(as.character(watch.list$pos) != watch.list$Pos)>0) { stop("[FF.MATCHUP.print.viewing.guide] ERROR: Pos dont Lineup") }
  watch.list$Team = watch.list$Team.x
  watch.list$Pos = watch.list$pos
  watch.list = watch.list[,!(colnames(watch.list) %in% c("Team.x","Team.y","pos"))]
  watch.list$Opp = toupper(watch.list$Opp)
  FF.MATCHUP.print.watch.list(watch.list)
  return(watch.list)
}
FF.MATCHUP.print.watch.list = function(watch.list,starters.only = FALSE, my.starters.only = FALSE, ignore.namelookup = NA) {
#    browser()
  if(!missing(ignore.namelookup)) { watch.list=watch.list[!(watch.list$name_lookup %in% ignore.namelookup),]}
  if(my.starters.only) { watch.list = watch.list[!(grepl("MINE",watch.list$FF_Team) & !watch.list$IsStarter),] }
  watch.list$IsStarterBool = watch.list$IsStarter
  watch.list$IsStarter = ifelse(watch.list$IsStarter,"STARTER","bench")
  watch.list$No = watch.list$Jersey.Number
  watch.list$Game.Time = ifelse(!is.na(as.numeric(str_sub(watch.list$Game.Time,1,1))),"IN PROGRESS",watch.list$Game.Time)
  watch.list$Game.Time = ifelse(str_sub(watch.list$Game.Time,1,1)=="L" | str_sub(watch.list$Game.Time,1,1)=="W","OVER",watch.list$Game.Time)
  watch.list$Game.Time[ watch.list$Opp=="** BYE **"] = NA
#   uTimes = unique(watch.list$Game.Time)
  uTimes.orderd = c("IN PROGRESS",sort(unique(watch.list$Game.Time[str_sub(watch.list$Game.Time,1,3)=="Thu"]))
                    ,sort(unique(watch.list$Game.Time[str_sub(watch.list$Game.Time,1,3)=="Sun"]))
                    ,sort(unique(watch.list$Game.Time[str_sub(watch.list$Game.Time,1,3)=="Mon"])),"OVER")
  for(uTime in uTimes.orderd) {
    rp = watch.list[watch.list$Game.Time == uTime & !is.na(watch.list$Game.Time),]
    if(starters.only) { rp = rp[rp$IsStarterBool,] }
    away.rp = ifelse(grepl("@",rp$Opp),rp$Team,rp$Opp)
    cat(paste("[",uTime,"]\n",sep=""))
    
    rp = rp[order(rp$Team,rp$FF_Team,rp$IsStarter,rp$pts,decreasing = TRUE),]
    print(rp[,c("Team","Opp","name","Pos","No","FF_Team","IsStarter","pts")])
    cat("\n\n")
  }
  if(sum(is.na(watch.list$Game.Time))>0) {
    rp = watch.list[is.na(watch.list$Game.Time),]
    cat(paste("[UNKONW PLAYING TIME]\n",sep=""))
    rp = rp[order(rp$Team,rp$FF_Team,rp$IsStarter,rp$pts,decreasing = TRUE),]
    print(rp[,c("name","No","Pos","Team","Opp","FF_Team","IsStarter","pts")])
    cat("\n\n")
  }
}

FF.MATCHUP.study.user.input = function() {
  browser()
  a = readline(prompt="Enter an integer: ") 
  
  # Checking on individual players
  rbind(all.proj[all.proj$name_lookup == "MICHAELCRABTREE",],proj[proj$name_lookup  == "MICHAELCRABTREE",])
  rbind(all.proj[all.proj$name_lookup == "ANQUANBOLDIN",],proj[proj$name_lookup  == "ANQUANBOLDIN",])
  
  cols=c("name_lookup","pos","pts","source")
  rbind(all.proj[all.proj$name_lookup == "ELIMANNING",cols],proj[proj$name_lookup  == "ELIMANNING",cols])
  rbind(all.proj[all.proj$name_lookup == "MATTHEWSTAFFORD",cols],proj[proj$name_lookup  == "MATTHEWSTAFFORD",cols])
}

FF.MATCHUP.print = function(opponent.name, my.team.name, my.starters, his.starters, my.whole.team, his.whole.team) {
  cat("[Optimal Lineup Analysis]\n\n")
  cat(paste("\t",my.team.name,": Expected Total Pts =",sum(my.starters$pts),"\n"))
  cat(paste("\t",opponent.name,": Expected Total Pts =",sum(his.starters$pts),"\n\n"))
  cat(paste("\t",ifelse(sum(my.starters$pts)>sum(his.starters$pts),my.team.name,opponent.name),"Projected to Win by",abs(sum(my.starters$pts)-sum(his.starters$pts)),"points\n\n\n\n"))
  
  # Looking at Starting Lineup
  cat("[Player Projections]\n\n")
  cat(paste(my.team.name,": Expected Pts by Starting Player:\n"))
  print(my.starters[,!(colnames(my.starters) %in% c("overallRank","name_lookup","passAtt","passComp","passCompPct","passYdsPerAtt","rushAtt","rushYdsPerAtt","recYdsPerRec","Team.Name"))]  )
  cat(paste("\n\n",opponent.name,": Expected Pts by Starting Player:\n"))
  print(his.starters[,!(colnames(his.starters) %in% c("overallRank","name_lookup","passAtt","passComp","passCompPct","passYdsPerAtt","rushAtt","rushYdsPerAtt","recYdsPerRec","Team.Name"))])
  
  my.bench = my.whole.team[!(my.whole.team$name_lookup %in% my.starters$name_lookup),]
  his.bench = his.whole.team[!(his.whole.team$name_lookup %in% his.starters$name_lookup),]
  cat(paste("\n\n\n",my.team.name,": Expected Pts by Bench Player:\n"))
  print(my.bench[,!(colnames(my.bench) %in% c("overallRank","name_lookup","passAtt","passComp","passCompPct","passYdsPerAtt","rushAtt","rushYdsPerAtt","recYdsPerRec","Team.Name"))] ) 
  cat(paste("\n\n",opponent.name,": Expected Pts by Bench Player:\n"))
  print(his.bench[,!(colnames(his.bench) %in% c("overallRank","name_lookup","passAtt","passComp","passCompPct","passYdsPerAtt","rushAtt","rushYdsPerAtt","recYdsPerRec","Team.Name"))])
}




FF.MATCHUP.set.globals = function(w) {
  WEEK      <<- w
#   unlockBinding("proj", .GlobalEnv)
  proj      <<- getAverageProjectionsForAGivenWeek(WEEK,FF.stat.median)# getAllProjectionsForAGivenWeek(1)
  all.proj  <<- getAllProjectionsForAGivenWeek(WEEK)
}



