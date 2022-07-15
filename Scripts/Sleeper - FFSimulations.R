reqpack <- c("ffsimulator","ggplot2","ffscrapr","dplyr","tidyr","ggridges");

if (length(setdiff(reqpack, rownames(installed.packages())))!=0) {
  install.packages(setdiff(reqpack, rownames(installed.packages())));
}

library(ffsimulator)
library(ggplot2)
library(ggridges)
library(ffscrapr)

# ╔ ———————— EDIT THIS DATA ———————— ╗

F_Season = 2022; #Current Season
F_Simulations = 25; # Number of simulations, more is bettter but they can take a long time.

F_Platform = "Sleeper"; # PUT "Sleeper" or "MFL"

S_Username = "XXXX"; #Sleeper Username

MFL_Username = "XXXX"; # MFL Username
MFL_password = "XXXX"; # MFL Password

# ╚ ————————  END OF EDITS  ———————— ╝


if(F_Platform== "MFL"){
  mflconn<-mfl_connect(season=F_Season,user_name=MFL_Username,password=MFL_password);
  leaguelist<-ff_userleagues(mflconn,F_Season);
} else {
  leaguelist <- sleeper_userleagues(S_Username,F_Season);
}

dir.create(file.path(getwd(),"Simulations"), showWarnings = FALSE);
paste0("Will generate the simulations on this folder: ",getwd(),"/Simulations/");

SimLeague <- function(LeagueID,LeagueName,LeagueSeason,LeagueSimulations){
  if(F_Platform== "MFL"){
    F_conn<-mfl_connect(season=LeagueSeason,league_id=LeagueID,user_name=MFL_Username,password=MFL_password);
  } else {
    F_conn<-sleeper_connect(season=LeagueSeason,league_id=LeagueID);
  }
  F_sim<-ff_simulate(conn=F_conn,n_seasons=LeagueSimulations);
  ggplot2::ggsave(plot=autoplot(F_sim,type="wins"),filename=paste0("Simulations/",strftime(Sys.time(),format = "%y%m%d"),"_",LeagueName,"_Wins.jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(F_sim,type="points"),filename=paste0("Simulations/",strftime(Sys.time(),format = "%y%m%d"),"_",LeagueName,"_Wins.jpg"),dpi = "retina");
  ggplot2::ggsave(plot=autoplot(F_sim,type="rank"),filename=paste0("Simulations/",strftime(Sys.time(),format = "%y%m%d"),"_",LeagueName,"_Wins.jpg"),dpi = "retina");
}

# PLOT ALL LEAGUES BY DEFAULT
for(x in 1:length(leaguelist$league_id)){
  SimLeague(leaguelist$league_id[x],leaguelist$league_name[x],F_Season,F_Simulations);
}

# TO PLOT ONLY 1 LEAGUE UNCOMMENT AND CHANGE LEAGUEID AND LEAGUENAME FOR THE ONES YOU WANT. AND COMMENT (add a # at the start) THE 3 PAST LINES.
# SimLeague(<LEAGUEID>,<LEAGUENAME>,F_Season,F_Simulations);

paste0("Finished generating simualtions, you can find them here: ",getwd(),"/Simulations/");