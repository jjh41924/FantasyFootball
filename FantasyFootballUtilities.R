getMYFFDir = function() { return( "C:/My_GIT_DIR") }
setwd("C:/GIT_FF") 

# source(paste(getMYFFDir(),"/Weekly_Actuals/Actuals From Yahoo.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/ESPN Weekly Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/NFL Weekly Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/Yahoo Weekly Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/FOX Weekly Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/FFtoday Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/CBS Projections.R",sep=""))
source(paste(getMYFFDir(),"/Weekly_Forecast/Scripts/FantasyFootballNerd Projections.R",sep=""))
source(paste(getMYFFDir(),"/League_Information/ESPN.R",sep=""))


getProjections = function(prefix){
  sp = strsplit(prefix, "/")[[1]]
  last = sp[length(sp)]
  directory = substring(prefix,1,nchar(prefix)-nchar(last))
  files = list.files(directory, pattern="*.csv", full.names=TRUE)
  files = files[grepl(prefix,files)]
  dates = rep(Sys.time(),length(files))
  for(i in 1:length(files)) {
    dates[i] = strptime(substring(files[i], nchar(files[i])-13,nchar(files[i])-4),"%Y_%m_%d")
  }
  file = files[dates==max(dates)][1]
  return(read.table(file,sep=",",header = TRUE))
}

setAllProjections.weeks.1.to.17 = function(){
  for(i in 1:17){
    #setPointsForWeek_FFtoday(i)#
    setPointsForWeek_espn(i)
    setPointsForWeek_nfl(i)
    setPointsForWeek_yahoo(i)
    setPointsForWeek_cbs(i)
  }
  #setPointsForCurrentWeek_fox(i)
}

setWeeklyProjections = function(week) {
  setPointsForWeek_espn(week)
  setPointsForWeek_nfl(week)
  setPointsForWeek_yahoo(week)
  setPointsForWeek_cbs(week)
  cat(paste("[SetWeeklyProjections] DONE. week[",week,"]\n"))
  cat("[SetWeeklyProjections] REMEMBER: to download projections from FFNerds.  Do so Here [http://www.fantasyfootballnerd.com/weekly-fantasy-football-projections]\n")
}

getAllActualData = function(){
  ret = genericHeadings(getActualsFromFile(1))
  for(i in 2:16){
    ret = rbind(ret,genericHeadings(getActualsFromFile(i)))
  } 
  return(ret)
}
filterByPositionAndStatistic = function(data,pos,stat){
  ret = data[,c("name_lookup","pos","team","source","week","positionRank",stat)]
  return(ret[ret$pos==pos,])
}
# getAllProjectionData = function() {
#   ret = getAllProjectionsForAGivenWeek(1)
#   for(i in 2:16){
#     ret = rbind(ret,getAllProjectionsForAGivenWeek(i))
#   }
#   return(ret)
# }
getAllProjectionsForAGivenWeek = function(week){
#   fft = genericHeadings(getFFtoday_Projections(week))
  cbs = genericHeadings(getCBS_Projections(week))
  espn = genericHeadings(getESPN_Projections(week))
  nfl = genericHeadings(getNFL_Projections(week))
  yahoo = genericHeadings(getYahoo_Projections(week))
  ffn = genericHeadings(getFFNerd_Projections(week))
  #fox = getFOX_Projections(i)
  a = rbind.fill(cbs,espn,nfl,yahoo,ffn)#fft,
  return(a)
}
getAverageProjectionsForAGivenWeek = function(week,FUN=NA){
  if(missing(FUN)) { FUN=FF.stat.median }
  #   fft = genericHeadings(getFFtoday_Projections(week))
  proj = getAllProjectionsForAGivenWeek(week)
  table(table(proj$name_lookup,proj$source))
  table(proj$source)
  all.proj[grepl(" ",all.proj$team),]
  ret = ddply(.data=proj,.(proj$name_lookup,proj$pos,proj$team),.fun = function(X) {  
      ret.sub = c(as.character(X$name[1])  #as.character(X$name_lookup[1]), as.character(X$pos[1]), as.character(X$team[1]), 
        , FUN(X$positionRank), FUN(X$overallRank), FUN(X$passAtt)
        , FUN(X$passComp),FUN(X$passYds), FUN(X$passTds),FUN(X$passInt), FUN(X$passCompPct), FUN(X$passYdsPerAtt), FUN(X$rushAtt)
        , FUN(X$rushYds),FUN(X$rushYdsPerAtt),FUN(X$rushTds),FUN(X$rec),FUN(X$recYds),FUN(X$recYdsPerRec),FUN(X$recTds)
        , FUN(X$twoPts),FUN(X$fumbles),NA, paste(as.character(substitute(FUN)),"(",nrow(X),")",sep="")
        , X$week[1], FUN(X$returnTds))
      
      return(ret.sub)
    })
  ret = ret[,c(1,2,4,3,5:ncol(ret))]
  colnames(ret) = colnames(proj)
  return(ret)
}

FF.stat.mean = function(X) {
  if(sum(!is.na(X))==0) { return(NA) }
  return(mean(X,na.rm = TRUE))
}

FF.stat.median= function(X) {
  if(sum(!is.na(X))==0) { return(NA) }
  return(median(X,na.rm = TRUE))
}

#rbind(ret[1,],proj[proj$name_lookup=="AARONDOBSON",])

# temp(cbs)
# temp(espn)
# temp(nfl)
# temp(yahoo)
# temp(ffn)
# temp=function(data){
#   print(data$source[1])
# #   print(table(table(data$name_lookup)))
#   print(table(table(data$name_lookup,data$team)))
# #   data[duplicated(  data$name_lookup) | duplicated(data$name_lookup,fromLast = TRUE),]
# }

getAllProjectionsForAGivenStat = function(stat,pos,all = FALSE){
  ret = getAllProjectionsForAGivenWeekAndStat(1,stat,pos,all)
  for(i in 2:16){
    ret = rbind.fill(ret,getAllProjectionsForAGivenWeekAndStat(i,stat,pos,all))
  }
  return(ret)
}
getAllProjectionsForAGivenWeekAndStat = function(week,stat,pos,all = FALSE){
  fft = filterByPositionAndStatistic(genericHeadings(getFFtoday_Projections(week)),pos,stat)
  cbsd = filterByPositionAndStatistic(genericHeadings(getCBS_Projections(week,TRUE)),pos,stat)
  cbsj = filterByPositionAndStatistic(genericHeadings(getCBS_Projections(week,FALSE)),pos,stat)
  espn = filterByPositionAndStatistic(genericHeadings(getESPN_Projections(week)),pos,stat)
  nfl = filterByPositionAndStatistic(genericHeadings(getNFL_Projections(week)),pos,stat)
  yahoo = filterByPositionAndStatistic(genericHeadings(getYahoo_Projections(week)),pos,stat)
  ffn = filterByPositionAndStatistic(genericHeadings(getFFNerd_Projections(week)),pos,stat)
  #fox = getFOX_Projections(i)
  colnames(fft)[7] = "fft"
  colnames(cbsd)[7] = "cbsd"
  colnames(cbsj)[7] = "cbsj"
  colnames(espn)[7] = "espn"
  colnames(nfl)[7] = "nfl"
  colnames(yahoo)[7] = "yahoo"
  colnames(ffn)[7] = "ffn"
  
  colnames(fft)[6] = "rank.fft"
  colnames(cbsd)[6] = "rank.cbsd"
  colnames(cbsj)[6] = "rank.cbsj"
  colnames(espn)[6] = "rank.espn"
  colnames(nfl)[6] = "rank.nfl"
  colnames(yahoo)[6] = "rank.yahoo"
  colnames(ffn)[6] = "rank.ffn"
  ret = merge(fft,cbsd[,c("name_lookup","week","rank.cbsd","cbsd")],   c("name_lookup","week"),all = all)
  ret = merge(ret,cbsj[,c("name_lookup","week","rank.cbsj","cbsj")],   c("name_lookup","week"),all = all)
  ret = merge(ret,espn[,c("name_lookup","week","rank.espn","espn")],   c("name_lookup","week"),all = all)
  ret = merge(ret,nfl[,c("name_lookup","week","rank.nfl","nfl")],     c("name_lookup","week"),all = all)
  ret = merge(ret,yahoo[,c("name_lookup","week","rank.yahoo","yahoo")], c("name_lookup","week"),all = all)
  ret = merge(ret,ffn[,c("name_lookup","week","rank.ffn","ffn")],     c("name_lookup","week"),all = all)
  return(ret)
}
genericHeadings = function(d){
  c = colnames(d)[3:ncol(d)]
  #print(colnames(d))
  for(i in 1:length(c)){
    if(grepl("_",c[i])){
      c[i] = substring(c[i],1,gregexpr("_",c[i])[[1]][1]-1)
    }
  }
  colnames(d) = c("name_lookup","pos",c)
  return(d)
}

my.players = as.data.frame(matrix(ncol=2,nrow=13))
colnames(my.players) = c("NAME","POS")
my.players[1,] = c(toupper("TomBrady"),"QB")
my.players[2,] = c(toupper("JeremyHill"),"RB")
my.players[3,] = c(toupper("JonathanStewart"),"RB")
my.players[4,] = c(toupper("AntonioBrown"),"WR")
my.players[5,] = c(toupper("EmmanuelSanders"),"WR")
my.players[6,] = c(toupper("GregOlsen"),"TE")
my.players[7,] = c(toupper("MattForte"),"RB")
my.players[8,] = c(toupper("JordyNelson"),"WR")
my.players[9,] = c(toupper("RashadJennings"),"RB")
my.players[10,] = c(toupper("FredJackson"),"RB")
my.players[11,] = c(toupper("AlexSmith"),"QB")
my.players[12,] = c(toupper("RuebenRandle"),"WR")
my.players[13,] = c(toupper("ZachErtz"),"TE")

# Tom Brady, NE QBRecent News  
# Jeremy Hill, Cin RBBreaking News and Video  
# Jonathan Stewart, Car RB	
# Antonio Brown, Pit WRRecent News	
# Emmanuel Sanders, Den WRRecent News	
# Greg Olsen, Car TE	
# Ravens D/ST D/ST	
# Cairo Santos, KC KBreaking News	
# Matt Forte, Chi RB	
# Jordy Nelson, GB WR	
# Rashad Jennings, NYG RB  QBreaking News	
# Fred Jackson, Buf RB  QBreaking News	
# Alex Smith, KC QBRecent News	
# Rueben Randle, NYG WRBreaking News	
# Zach Ertz, Phi TERecent News	
# Bills D/ST D/ST	
