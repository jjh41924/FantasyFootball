
show.stats = function(ranks.min,ranks.max,win) {
  cat("Probability of NOT Making it to the playoffs\n")
  print(t(t(sort(apply(rank.inclusive.greater(ranks.min,7),2,sum)/nrow(win)))))
  cat("\n\nProbability of MAKING it to the playoffs WITHOUT Relying ond a Tie Breaker\n")
  print(t(t(sort(apply(rank.inclusive.less(ranks.max,6),2,sum)/nrow(win)))))
  cat("\n\nProbability of MAKING it to the playoffs WITH WINNING Tie Breaker\n")
  print(t(t(sort(apply(rank.inclusive.less(ranks.min,6),2,sum)/nrow(win)))))
  
  cat("\n\nProbability of getting a BYE WITHOUT Relying on a Tie Breaker\n")
  print(t(t(sort(apply(rank.inclusive.less(ranks.max,2),2,sum)/nrow(win)))))
  cat("\n\nProbability of getting a BYE WITH WINNING Tie Breaker\n")
  print(t(t(sort(apply(rank.inclusive.less(ranks.min,2),2,sum)/nrow(win)))))
}


rank.inclusive.greater = function(ranks.min,level){
  ret = ranks.min >= level
  return(ret)
}
rank.inclusive.less = function(ranks.max,level){
  ret = ranks.max <= level
  return(ret)
}
level = 6






roster = scrape.espn.league.roster() 
teams = unique(roster$Team.Name)
wins = sapply(teams,function(t) { as.numeric(str_sub(t,str_length(t)-3,str_length(t)-3)) } )

t = data.frame(index = 1:length(teams),name = teams, wins = wins, losses = 11-wins)

week12.matchups = rbind(c(9,6),c(2,1),c(12,3),c(11,4),c(10,5),c(7,8))
week13.matchups = rbind(c(8,9),c(1,3),c(4,2),c(5,12),c(6,11),c(7,10))

require(combinat)


win = as.data.frame(matrix(t$wins,ncol=12,nrow=64*64,byrow = TRUE)); colnames(win) = t$name
counter = 1
for(i in 0:63) {
  for(j in 0:63) {
    #index = i + 64*j +1
    inds.12 = ifelse(as.numeric(intToBits(i))[1:6] == 1,week12.matchups[,1],week12.matchups[,2])
    inds.13 = ifelse(as.numeric(intToBits(j))[1:6] == 1,week13.matchups[,1],week13.matchups[,2])
    win[counter,inds.12] = win[counter,inds.12] + 1
    win[counter,inds.13] = win[counter,inds.13] + 1
    counter = counter + 1
  }
}


# are there any opportunities to make the playoffs for every team
rank(c(2,3,4,5,7,1,5),ties.method = "max")
ranks.max = ranks.min = win
for(i in 1:nrow(win)){
  ranks.max[i,] = rank(-win[i,],ties.method = "max")
  ranks.min[i,] = rank(-win[i,],ties.method = "min")
}
show.stats(ranks.min,ranks.max,win)


t = data.frame(index = 1:length(teams),name = teams, wins = wins, losses = 11-wins)
actual.winners.12 = c(1,9,3,4,10,7)
t$wins[actual.winners.12] = t$wins[actual.winners.12] +1
win = as.data.frame(matrix(t$wins,ncol=12,nrow=64,byrow = TRUE)); colnames(win) = t$name
counter = 1
for(j in 0:63) {
  #index = i + 64*j +1
  # inds.12 = ifelse(as.numeric(intToBits(i))[1:6] == 1,week12.matchups[,1],week12.matchups[,2])
  inds.13 = ifelse(as.numeric(intToBits(j))[1:6] == 1,week13.matchups[,1],week13.matchups[,2])
  #win[counter,inds.12] = win[counter,inds.12] + 1
  win[counter,inds.13] = win[counter,inds.13] + 1
  counter = counter + 1
} 
ranks.max = ranks.min = win
for(i in 1:nrow(win)){
  ranks.max[i,] = rank(-win[i,],ties.method = "max")
  ranks.min[i,] = rank(-win[i,],ties.method = "min")
}
show.stats(ranks.min,ranks.max,win)
