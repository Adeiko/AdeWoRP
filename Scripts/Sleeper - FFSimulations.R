reqpack <- c("ffsimulator","ggplot2","ffscrapr","tidyverse","ggridges")

if (length(setdiff(reqpack, rownames(installed.packages())))!=0) {
  install.packages(setdiff(reqpack, rownames(installed.packages())));
}
invisible(lapply(reqpack, require, character.only = TRUE))

# ╔ ———————— EDIT THIS DATA ———————— ╗

F_Season = 2022; #Current Season
F_Simulations = 25; # Number of simulations, more is bettter but they can take a long time.

F_Platform = "Sleeper"; # PUT "Sleeper" or "MFL"

S_Username = "Adeiko"; #Sleeper Username

MFL_Username = "XXXX"; # MFL Username
MFL_password = "XXXX"; # MFL Password

# ╚ ————————  END OF EDITS  ———————— ╝

if(F_Platform== "MFL"){
  mflconn<-mfl_connect(season=F_Season,user_name=MFL_Username,password=MFL_password)
  leaguelist<-ff_userleagues(mflconn,F_Season)
} else {
  leaguelist <- sleeper_userleagues(S_Username,F_Season)
}

dir.create(file.path(getwd(),"Simulations"), showWarnings = FALSE);
message(paste0("Will generate the simulations on this folder: ",getwd(),"/Simulations/"))

SimLeague <- function(LeagueID,LeagueName,LeagueSeason,LeagueSimulations){
  if(F_Platform== "MFL"){
    F_conn<-mfl_connect(season=LeagueSeason,league_id=LeagueID,user_name=MFL_Username,password=MFL_password)
  } else {
    F_conn<-sleeper_connect(season=LeagueSeason,league_id=LeagueID)
  }
  F_sim<-ff_simulate(conn=F_conn,n_seasons=LeagueSimulations)

  purrr::walk(c("wins","points","rank"), function(x){
    ggplot2::ggsave(plot=ggplot2::autoplot(F_sim,type=x)+ggplot2::theme(plot.background=ggplot2::element_rect(fill='#f5f5f5')),filename=paste0("Simulations/",strftime(Sys.time(),format = "%y%m%d"),"_",LeagueName,"_",x,".jpg"),dpi = "retina");
  })
}

# PLOT ALL LEAGUES BY DEFAULT
purrr::pwalk(leaguelist, function(league_id,league_name,...){SimLeague(league_id,league_name,F_Season,F_Simulations)})

# TO PLOT ONLY 1 LEAGUE UNCOMMENT AND CHANGE LEAGUEID AND LEAGUENAME FOR THE ONES YOU WANT. AND COMMENT (add a # at the start) THE 3 PAST LINES.
# SimLeague(<LEAGUEID>,<LEAGUENAME>,F_Season,F_Simulations);

message(paste0("Finished generating simualtions, you can find them here: ",getwd(),"/Simulations/"))
