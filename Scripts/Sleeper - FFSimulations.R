# IF ITS THE FIRST TIME UNCOMMENT AND INSTALL THE PACKAGES

install.packages(c("ffsimulator","ggplot2","ffscrapr","dplyr","tidyr","ggridges"))

library(ffsimulator)
library(ggplot2)
library(ggridges)
library(ffscrapr)

S_Username = "XXX";
S_Season = 2022;
S_Simulations = 25;

leaguelist <- sleeper_userleagues(S_Username,2022);
dir.create(file.path(getwd(),"Simulations"), showWarnings = FALSE);

SimLeague <- function(LeagueID,LeagueName,LeagueSeason,LeagueSimulations){
  sleeper_conn<-sleeper_connect(season=LeagueSeason,league_id=LeagueID);
  sleeper_sim<-ff_simulate(conn=sleeper_conn,n_seasons=LeagueSimulations);
  ggplot2::ggsave(plot=autoplot(sleeper_sim,type="wins"),filename=paste0("Simulations/",LeagueName,"_Wins_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(sleeper_sim,type="points"),filename=paste0("Simulations/",LeagueName,"_Points_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(sleeper_sim,type="rank"),filename=paste0("Simulations/",LeagueName,"_Rank_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
}

## ONLY 1 LEAGUE UNCOMMENT AND CHANGE LEAGUEID AND LEAGUENAME FOR THE ONES YOU WANT.
# SimLeague(LEAGUEID,LEAGUENAME,S_Season,S_Simulations)

## PLOT ALL LEAGUES BY DEFAULT
for(x in 1:length(leaguelist$league_id)){
  SimLeague(leaguelist$league_id[x],leaguelist$league_name[x],S_Season,S_Simulations);
}
