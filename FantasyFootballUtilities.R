getProjections = function(prefix){
  sp = strsplit(prefix, "/")[[1]]
  last = sp[length(sp)]
  directory = substring(prefix,1,nchar(prefix)-nchar(last))
  files = list.files(directory, pattern="*.csv", full.names=TRUE)
  dates = rep(Sys.time(),length(files))
  for(i in 1:length(files)) {
    dates[i] = strptime(substring(files[i], nchar(files[i])-13,nchar(files[i])-4),"%Y_%m_%d")
  }
  file = files[dates==max(dates)][1]
  return(read.table(file,sep=",",header = TRUE))
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
