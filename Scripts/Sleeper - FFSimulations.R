# IF ITS THE FIRST TIME UNCOMMENT AND INSTALL THE PACKAGES

install.packages(c("ffsimulator","ggplot2","ffscrapr","dplyr","tidyr","ggridges"))

library(ffsimulator)
library(ggplot2)
library(ggridges)
library(ffscrapr)

# <-----EDIT THIS DATA ----->

#Current Season
F_Season = 2022;

# Number of simulations, more is bettter but they can take a long time.
F_Simulations = 25;

# PUT "Sleeper" or "MFL"
F_Platform = "Sleeper"

#Sleeper Username
S_Username = "XXXXX";

# MFL Credentials
MFL_Username = "XXXXXX";
MFL_password = "XXXX";

# <----- END OF EDIT ----->


if(F_Platform== "MFL"){
  mflconn<-mfl_connect(season=F_Season,user_name=MFL_Username,password=MFL_password);
  leaguelist<-ff_userleagues(mflconn,F_Season);
} else {
  leaguelist <- sleeper_userleagues(S_Username,F_Season);
}

dir.create(file.path(getwd(),"Simulations"), showWarnings = FALSE);

SimLeague <- function(LeagueID,LeagueName,LeagueSeason,LeagueSimulations){
  if(F_Platform== "MFL"){
    F_conn<-mfl_connect(season=LeagueSeason,league_id=LeagueID,user_name=MFL_Username,password=MFL_password);
  } else {
    F_conn<-sleeper_connect(season=LeagueSeason,league_id=LeagueID);
  }
  F_sim<-ff_simulate(conn=F_conn,n_seasons=LeagueSimulations);
  ggplot2::ggsave(plot=autoplot(F_sim,type="wins"),filename=paste0("Simulations/",LeagueName,"_Wins_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(F_sim,type="points"),filename=paste0("Simulations/",LeagueName,"_Points_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(F_sim,type="rank"),filename=paste0("Simulations/",LeagueName,"_Rank_",as.numeric(Sys.time()),".jpg"),dpi = "retina");
}

## ONLY 1 LEAGUE UNCOMMENT AND CHANGE LEAGUEID AND LEAGUENAME FOR THE ONES YOU WANT.
# SimLeague(LEAGUEID,LEAGUENAME,F_Season,F_Simulations)

## PLOT ALL LEAGUES BY DEFAULT
for(x in 1:length(leaguelist$league_id)){
  SimLeague(leaguelist$league_id[x],leaguelist$league_name[x],F_Season,F_Simulations);
}