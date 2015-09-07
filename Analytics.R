source("C:/My_GIT_DIR/FantasyFootballUtilities.R")


actuals = getActualsFromFile(3)
projections = getESPN_Projections(3)
head(actuals)
head(projections)


sort(actuals$name[actuals$name %in% my.players$NAME])

my.actuals = actuals[paste(actuals$name,"_",actuals$pos,sep="") %in% paste(my.players$NAME,"_",my.players$POS,sep="") ,]
my.actuals = my.actuals[order(my.actuals$name),]
my.projections = projections[paste(projections$name,"_",projections$pos,sep="") %in% paste(my.players$NAME,"_",my.players$POS,sep="") ,]
my.projections = my.projections[order(my.projections$name),]

sum(actuals$name %in% my.players$NAME)
sum(my.players$NAME %in% actuals$name)
sum(projections$name %in% my.players$NAME)
sum(my.players$NAME %in% projections$name)
nrow(my.players)


my.actuals$passYds - my.projections$passYds_espn
my.actuals$passTds - my.projections$passTds_espn
my.actuals$passInt - my.projections$passInt_espn
my.actuals$rushYds - my.projections$rushYds_espn
my.actuals$rushTds - my.projections$rushTds_espn
my.actuals$recYds - my.projections$recYds_espn
my.actuals$recTds - my.projections$recTds_espn
colnames(my.actuals)


