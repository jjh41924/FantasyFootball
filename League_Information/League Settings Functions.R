

FF.EXP.PTS = function(proj) {
  proj$pts = apply(proj,1,function(X){
    ret = 0;
    #     browser()
    ret = FF.MULTIPLY(X["passAtt"],passAttMultiplier) +
      FF.MULTIPLY(X["passComp"],passCompMultiplier) +
      #       passIncompMultiplier
      FF.MULTIPLY(X["passYds"],passYdsMultiplier) +
      FF.MULTIPLY(X["passTds"],passTdsMultiplier) +
      FF.MULTIPLY(X["passInt"],passIntMultiplier) +
      #       FF.MULTIPLY(X["passCompPct"],passAttMultiplier) +
      #       FF.MULTIPLY(X["passYdsPerAtt"],passAttMultiplier) +
      FF.MULTIPLY(X["rushAtt"],rushAttMultiplier) +
      FF.MULTIPLY(X["rushYds"],rushYdsMultiplier) +
      #       FF.MULTIPLY(X["rushYdsPerAtt"],passAttMultiplier) +
      FF.MULTIPLY(X["rushTds"],rushTdsMultiplier) +
      FF.MULTIPLY(X["rec"],recMultiplier) +
      FF.MULTIPLY(X["recYds"],recYdsMultiplier) +
      #       FF.MULTIPLY(X["recYdsPerRec"],passAttMultiplier) +
      FF.MULTIPLY(X["recTds"],recTdsMultiplier) +
      FF.MULTIPLY(X["twoPts"],twoPtsMultiplier) +
      FF.MULTIPLY(X["fumbles"],fumlMultiplier)
    return(ret)
  })
  return(proj)
}

FF.MULTIPLY=function(proj.elem,pt) {
  if(is.na(proj.elem) || is.na(pt)) { return(0) }
  return(as.numeric(proj.elem)*pt)
}


FF.GET.LINEUP = function(team) {
  ret = FF.get.best.player(team,"QB",numQBstarters) 
  ret = rbind(ret,FF.get.best.player(team[!(team$name_lookup %in% ret$name_lookup),],"RB",numRBstarters) )
  ret = rbind(ret,FF.get.best.player(team[!(team$name_lookup %in% ret$name_lookup),],"WR",numWRstarters) )
  ret = rbind(ret,FF.get.best.player(team[!(team$name_lookup %in% ret$name_lookup),],"TE",numTEstarters) )
  ret = rbind(ret,FF.get.best.player(team[!(team$name_lookup %in% ret$name_lookup),],c("RB","TE","WR"),numFLEXstarters))
  return(ret)
}

FF.get.best.player = function(team,pos,count) {
#   browser()
  if(count > sum(team$pos %in% pos)) { stop(paste("[get.best.player] ERROR: You dont have enough players at pos[",pos,"]. Required[",count,"] You Have[",sum(team$pos==pos),"]",sep="")) }
  ret = team[team$pos %in% pos,]
  ret = ret[order(ret$pts,decreasing=TRUE),]
  return(ret[1:count,])
}


