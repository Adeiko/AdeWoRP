source("Scripts/DevLibraries.r")
load("Data/AdeStats.rda")
load("Data/ADPData.rda")
load("Data/Rosters_headshots.rda")
load("Data/Teams_color.rda")

ADP_MinTopYear = 2014
ADP_MaxTopYear = 2021
ADP_Players = 40
WoRP_TopPlayers = 24
WoRP_TopPlayers_Single = 40
WoRP_MinTopYear = 2014
WoRP_MaxTopYear = 2021
WoRP_SingleYear = 2021

# Define the Scoring of the league
ScoringValues <- c(
  "Pass_Yards"=0.04,
  "Pass_TD"=4,
  "Pass_INT"=-1,
  "Pass_Sack"=0,
  "Pass_Inc"=0,
  "Pass_Comp"=0,
  "Pass_FD"=0,
  "Rush_Yards"=0.1,
  "Rush_TD"=6,
  "Rush_FD"=0,
  "Rush_Att"=0,
  "Rec_QB"=1,
  "Rec_RB"=1,
  "Rec_WR"=1,
  "Rec_TE"=1,
  "Rec_Yards"=0.1,
  "Rec_TD"=6,
  "Rec_FD"=0,
  "Rec_FD_TE"=0,
  "PAT_2p"=2,
  "Fumble"=-1
)

# Define the Roster Spots
RP <- c(
  "TM"=12,
  "QB"=1,
  "RB"=2,
  "WR"=2,
  "TE"=1,
  "FLEX"=1
)

source("Scripts/AdeStats - AdeScoring creator.r")
source("Scripts/AdeStats - AdeScoring Graphs.r")
source("Scripts/AdeStats - AdeScoring ADP.r")
source("Scripts/AdeStats - AdeScoring Tables.r")
