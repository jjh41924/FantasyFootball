getStatDF = function(stat,pos){
  all.RB = getAllProjectionsForAGivenStat(stat,pos,TRUE)
  all.act = getPFR_Actuals()
  all.RB.act = all.act[all.act$pos==pos,c(stat,"name","age","date","home.game","opponent","result","week","day","game","year","name_lookup")]
  colnames(all.RB.act)[1] = "Actual"
  m = merge(all.RB,all.RB.act,c("name_lookup","week"),all = FALSE)
  m = m[apply(m,1,function(X) { sum(is.na(X))==0 }),]
  m = m[,c("name_lookup","week","pos","team","age","date","home.game","opponent","result","week","day","game","year"
           ,"rank.fft","rank.cbsd","rank.cbsj","rank.espn","rank.nfl","rank.yahoo","rank.ffn"
           ,"fft","cbsd","cbsj","espn","nfl","yahoo","ffn","Actual")]
  m$rank.ffn = m$rank.yahoo
  return(m)
  #apply(m,2,function(X) { sum(is.na(X)) })
  #dim(m)
}
hist.matrix = function(m){
  d = m[,21:28]
  par(mfrow=c(6,6))
  for(i in 1:ncol(d)){
    for(j in 1:ncol(d)){
      if(i==j){
        #hist of the one
        x = d[i]
        hist(x[,1],col = 'ORANGE',breaks = 100,main= colnames(d)[i],xlab = "",ylab = "")
      } else if (i < j){
        #hist of diffs
        use = d[i] != 0
        x = d[use,i] - d[use,j]
        name = paste(colnames(d)[i],"-",colnames(d)[j])
        hist(x,col = 'ORANGE',breaks = 100,main= name,xlab = paste("mu[",round(mean(x),2),"] sd[",round(sd(x)),"]",sep=""),ylab = "")
      } else {
      }
    }
  }
}
fitted.vs.residuals.plot = function(m, txt, FUN = filter.omit.zero) {
  proj = m[,21:27]
  act = m[,28]
  par(mfrow=c(2,4))
  for(i in 1:ncol(proj)){
    use = FUN(proj[,i],act,m[,i+13])
    resid = proj[use,i] - act[use]
    SSRes = sum(resid^2)
    SSTot = sum((act[use] - mean(act[use]))^2)
    rsquared = 1 - SSRes/SSTot
    plot(cbind(proj[use,i],resid),main = paste(colnames(proj)[i]," ",txt, " Rsq[",round(rsquared,3),"]",sep="") ,ylab = "Residuals", xlab = "Proj")
  }
}
filter.top.rank = function(proj,act,rank) {
  return(rank < 20)
}
filter.top.proj = function(proj,act,rank){
  return(proj > quantile(proj,c(.9)))
}
filter.top.act = function(proj,act,rank){
  return(act > quantile(act,c(.9)))
}
filter.omit.zero = function(proj,act,rank) {
  return(!(proj == 0 & act == 0)) #I omit cases where both proj and actuals are 0.  No credit for injuries
}

rep(TRUE,nrow(proj))
if(top.players==-1) {
} else {
  use =  < top.players
  print(colnames(m)[i+13])
  print(colnames(proj)[i])
}


stat = "rushYds"
pos = "RB"
run.analysis = function(stat,pos){
  m = getStatDF(stat,pos)
  hist.matrix(m)
  par(ask=TRUE)
  fitted.vs.residuals.plot(m,stat)
  fitted.vs.residuals.plot(m,paste(stat,"Top 20"),filter.top.rank)
  fitted.vs.residuals.plot(m,paste(stat,"Top 10%"),filter.top.proj)
  fitted.vs.residuals.plot(m,paste(stat,"Top 10% Actual"),filter.top.act)
  print(cor(m[,21:28]))
  par(ask=FALSE)
}
run.analysis("rushYds","RB")
run.analysis("rushTds","RB")
run.analysis("rec","WR")
run.analysis("recYds","WR")
run.analysis("recTds","WR")

run.analysis("rushTds","RB")


out.sample = (1:nrow(m)) %in% sample(1:nrow(m),round(nrow(m)/20))
in.sample = !out.sample
m.in = m[in.sample,]
m.out = m[out.sample,]

t = m.in
summary(lm(t$Actual~t$fft+t$cbsd+t$cbsj+t$espn+t$nfl+t$ffn+t$yahoo))

#Run regression with the mean and the differences from the means
#Look at the residuals for each projection

all.RB = all.RB[order(paste(all.RB$name_lookup,"_",all.RB$week,"_",all.RB$source,sep="")),]

dim(all.RB)
dim(all.RB.act)
dim(m)
#

head(all.RB.act)
sum(m.in$yahoo != m.in$Actual)
