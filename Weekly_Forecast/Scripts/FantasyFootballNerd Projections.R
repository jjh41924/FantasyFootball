###########################
# I Had to Pay for this one. $1
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

getFile = function(week,pos){
  filename = paste("C:/My_GIT_DIR/Weekly_Forecast/FFNerd/Raw Files/Week ",week,"/FFN_Weekly_Projections_",pos,".csv",sep="")
  print(filename)
  cleanFile(filename)
  return(read.table(filename,header=TRUE,sep=","))
}

cleanFile = function(filename){
  file = readLines(filename)
  file = gsub("'","",file)
  writeLines(file,filename)
}

setPointsForWeek_ffn = function(week){  
  #Suffix
  suffix <- "ffn"
  
  #Download fantasy football projections from FantasyFootballNerd.com
  qb_ffn = getFile(week,"QB")
  rb_ffn = getFile(week,"RB")
  wr_ffn = getFile(week,"WR")
  te_ffn = getFile(week,"TE")
  kickers_ffn = getFile(week,"K")
  dst_ffn = getFile(week,"DEF")
  
  #Add variable names for each object
  names(qb_ffn) <- c("name_ffn","team_ffn","passComp_ffn","passAtt_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn")
  names(rb_ffn) <- c("name_ffn","team_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","fumbles_ffn")
  names(wr_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn")
  names(te_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn")
  names(kickers_ffn) <- c("name_ffn","team_ffn","xp_ffn","fg_ffn")
  names(dst_ffn) <- c("name_ffn","dstSack_ffn","dstInt_ffn","dstForFuml_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn","dstSafety_ffn","dstPA_ffn","dstYdsA_ffn")
  
  #Add variable for player position
  qb_ffn$pos <- as.factor("QB")
  rb_ffn$pos <- as.factor("RB")
  wr_ffn$pos <- as.factor("WR")
  te_ffn$pos <- as.factor("TE")
  kickers_ffn$pos <- as.factor("K")
  dst_ffn$pos <- as.factor("DST")
  
  #Merge players across positions
  projections_ffn <- rbind.fill(qb_ffn, rb_ffn, wr_ffn, te_ffn, kickers_ffn, dst_ffn)
  
  #Add variables from other projection sources
  projections_ffn$returnTds_ffn <- NA
  projections_ffn$twoPts_ffn <- NA
  

  #Name for merging
  projections_ffn$name <- nameMerge(projections_ffn$name_ffn)
  
  #fakeout these columns
  projections_ffn$positionRank_ffn = 0
  projections_ffn$overallRank_ffn = 0
  projections_ffn$pts_ffn = 0
  
  #Remove duplicate cases
  projections_ffn[projections_ffn$name %in% projections_ffn[duplicated(projections_ffn$name),"name"],]
  
  #Order variables in data set
  projections_ffn <- projections_ffn[,c(prefix, paste(varNames, suffix, sep="_"))]
  
  #Save file
  #Save file  
  save(projections_ffn, file = paste(getMYFFDir(),"/Weekly_Forecast/FFNerd/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".RData", sep=""))
  write.csv(projections_ffn, file=paste(getMYFFDir(),"/Weekly_Forecast/FFNerd/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}

getFFNerd_Projections = function(week){
  ret = getProjections(paste(getMYFFDir(),"/Weekly_Forecast/FFNerd/Projections_Week_",week,"_Date_",sep=""))
  ret$source = "FFNerd"
  ret$week = week
  return(ret)
  #write.csv(file=paste("C:/MY_FF/Weekly_Forecast/ESPN/Projections_Week_",week,"_Date_",strftime(Sys.time(), format = "%Y_%m_%d"),".csv", sep=""), row.names=FALSE)
}
