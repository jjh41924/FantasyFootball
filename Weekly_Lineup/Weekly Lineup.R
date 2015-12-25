source('C:/My_GIT_DIR/Weekly_Lineup/Weekly_Lineup Functions.R')

WEEK=16
setWeeklyProjections(WEEK)


# Do Lineup analysis
# FF.MATCHUP.set.globals(8)
FF.LINEUP.print.teams(WEEK)
wl.family = FF.LINEUP.Family(WEEK,"A. REID'S ABOVE GROUND... (7-6)")
wl.work   = FF.LINEUP.Work(WEEK,"Concussed Free")

# head(wl.family)
# FF.MATCHUP.print.watch.list(rbind(wl.family,wl.work),starters.only = TRUE,ignore.namelookup = c("JOSEPHRANDLE"))
FF.MATCHUP.print.watch.list(rbind(wl.family,wl.work), starters.only = TRUE, show.actuals = TRUE)
FF.MATCHUP.print.watch.list(wl.work, starters.only = TRUE, show.actuals = TRUE)
