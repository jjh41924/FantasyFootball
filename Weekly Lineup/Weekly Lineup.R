source('C:/My_GIT_DIR/Weekly Lineup/Weekly Lineup Functions.R')


setWeeklyProjections(7)


# Do Lineup analysis
FF.MATCHUP.set.globals(7)
wl.work   = FF.LINEUP.Work()
wl.family = FF.LINEUP.Family()

head(wl.family)
FF.MATCHUP.print.watch.list(rbind(wl.family,wl.work),starters.only = TRUE,ignore.namelookup = c("JOSEPHRANDLE"))
FF.MATCHUP.print.watch.list(rbind(wl.family,wl.work), my.starters.only = TRUE)



