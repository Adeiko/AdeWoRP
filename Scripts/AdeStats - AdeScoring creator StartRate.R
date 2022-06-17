load("Data/AdeStats.rda")

AdeStats = AdeStats %>%
  dplyr::filter(Year >= WoRP_MinTopYear,Year <= WoRP_MaxTopYear)

# Generate the scoring of each player per week based on ScoringValues
fp_scoring <- AdeStats %>%
  dplyr::group_by(ID_PFF,Week,Year) %>%
  dplyr::summarise(
    Player = dplyr::first(Player_Name),
    PaYardScore = sum(Pass_Yards*ScoringValues["Pass_Yards"],na.rm=TRUE),
    PaTDScore = sum(Pass_TD*ScoringValues["Pass_TD"],na.rm=TRUE),
    PaIntScore = sum(Pass_INT*ScoringValues["Pass_INT"],na.rm=TRUE),
    PaSackScore = sum(Pass_Sacks*ScoringValues["Pass_Sack"],na.rm=TRUE),
    PaFDScore = sum(Pass_First_Down*ScoringValues["Pass_FD"],na.rm=TRUE),
    PaIncScore = sum((Pass_Att-Pass_Comp)*ScoringValues["Pass_Inc"],na.rm=TRUE),
    PaCompScore = sum(Pass_Comp*ScoringValues["Pass_Comp"],na.rm=TRUE),
    RuAttScore = sum(Rush_Att*ScoringValues["Rush_Att"],na.rm=TRUE),
    RuYardScore = round(sum(Rush_Yards*ScoringValues["Rush_Yards"],na.rm=TRUE),2),
    RuTDScore = sum(Rush_TD*ScoringValues["Rush_TD"],na.rm=TRUE),
    RuFDScore = sum(Rush_First_Down*ScoringValues["Rush_FD"],na.rm=TRUE),
    ReRecScore = sum((Rec_Receptions[Pos=="QB"]*ScoringValues["Rec_QB"]),(Rec_Receptions[Pos=="RB"]*ScoringValues["Rec_RB"]),(Rec_Receptions[Pos=="WR"]*ScoringValues["Rec_WR"]),(Rec_Receptions[Pos=="TE"]*ScoringValues["Rec_TE"]),na.rm=TRUE),
    ReYardScore = sum(Rec_Yards*ScoringValues["Rec_Yards"],na.rm=TRUE),
    ReTDScore = sum(Rec_TD*ScoringValues["Rec_TD"],na.rm=TRUE),
    ReFDScore = sum((Rec_First_Down*ScoringValues["Rec_FD"]),(Rec_Receptions[Pos=="TE"]*ScoringValues["Rec_FD_TE"]),na.rm=TRUE),
    TwopScore = sum(PAT_Success*ScoringValues["PAT_2p"],na.rm=TRUE),
    FumbleScore = sum(Fumbles*ScoringValues["Fumble"],na.rm=TRUE),
    PaFP = sum(PaYardScore,PaTDScore,PaIntScore,PaSackScore,PaFDScore),
    ReFP = sum(ReRecScore,ReYardScore,ReTDScore,ReFDScore),
    RuFP = sum(RuYardScore,RuTDScore,RuFDScore,RuAttScore),
    MiscFP = sum(TwopScore,FumbleScore),
    CustomScoring = sum(PaFP,ReFP,RuFP)
  )

# Generate a Rank and a PosRank for each week using the custom scoring.
StatCustom <- AdeStats %>%
  dplyr::select(Player_Name,ID_PFF,Week,Year,Pos,Sleeper_Start_Dynasty_per,Snaps_Total) %>%
  dplyr::left_join ((fp_scoring %>%
                       dplyr::select(ID_PFF,Week,Year,CustomScoring)), by = c("ID_PFF","Week","Year"))%>%
  dplyr::arrange(Pos, Sleeper_Start_Dynasty_per) %>%
  dplyr::group_by(Year,Week,Pos) %>%
  dplyr::filter(!is.na(Sleeper_Start_Dynasty_per))%>%
  dplyr::mutate(PosRank=rank(desc(Sleeper_Start_Dynasty_per),ties.method="random"))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(Year,Week)%>%
  dplyr::mutate(Rank=rank(desc(Sleeper_Start_Dynasty_per),ties.method="random"))

FlexRanks = StatCustom %>%
  dplyr::filter(!is.na(Sleeper_Start_Dynasty_per))%>%
  dplyr::group_by(Year,Week)%>%
  dplyr::filter(dplyr::case_when(Pos=="RB" ~ PosRank > (RP["TM"]*RP["RB"]),Pos=="WR" ~ PosRank > (RP["TM"]*RP["WR"]),Pos=="TE" ~ PosRank > (RP["TM"]*RP["TE"])))%>%
  dplyr::mutate(FlexRank = rank(desc(Sleeper_Start_Dynasty_per),ties.method="random"))

StatCustom = StatCustom %>%
  dplyr::left_join(FlexRanks %>% dplyr::select(ID_PFF,Year,Week,FlexRank),by=c("ID_PFF","Year","Week"))


# Generate the Average of Flex by getting the scoring of the (<FlexSpots>*<Teams>) by players that are not used in the other regular positions.
FlexData = StatCustom %>%
  dplyr::filter((!is.na(CustomScoring)) & (!is.na(FlexRank)) & (Pos=="RB"|Pos=="WR"|Pos=="TE")) %>%
  dplyr::filter(FlexRank <= (RP["TM"]*RP["FLEX"])) %>%
  dplyr::group_by(Year,Week) %>%
  dplyr::summarise(
    AvgFLEX = mean(CustomScoring),
    StdFLEX = stats::sd(CustomScoring)
  )
#
#
# QBRPAvg_test = StatCustom %>%
#   dplyr::filter(!is.na(CustomScoring))%>%
#   dplyr::filter(dplyr::case_when(
#     Pos=="QB" ~ PosRank > (RP["TM"]*RP["QB"]),
#     Pos=="RB" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
#     Pos=="WR" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
#     Pos=="TE" ~ FlexRank > RP["TM"]*RP["FLEX"]),
#     Sleeper_Start_Dynasty_per>=0.01
#   )%>%
#   # dplyr::filter(PosRank > (RP["TM"]*RP["QB"]),Sleeper_Start_Dynasty_per>=0.01)%>%
#   dplyr::group_by(Year,Week)%>%
#   arrange(match(Pos, "QB"),desc(Sleeper_Start_Dynasty_per))%>%
#   slice_head(n = extraplayersavg)%>%
#   ungroup()%>%
#   select(Player_Name,Year,Week,Pos,Sleeper_Start_Dynasty_per,FlexRank,CustomScoring)
#
extraplayersavg = 6
QBRPAvg = StatCustom %>%
  dplyr::filter(!is.na(CustomScoring))%>%
  dplyr::filter(dplyr::case_when(
    Pos=="QB" ~ PosRank > (RP["TM"]*RP["QB"]),
    Pos=="RB" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
    Pos=="WR" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
    Pos=="TE" ~ FlexRank > RP["TM"]*RP["FLEX"]),
    Sleeper_Start_Dynasty_per>=0.01,
    Snaps_Total >= 5
  )%>%
  dplyr::group_by(Year,Week)%>%
  arrange(match(Pos, "QB"),desc(Sleeper_Start_Dynasty_per))%>%
  slice_head(n = extraplayersavg)%>%
  ungroup()%>%
  dplyr::group_by(Year,Week)%>%
  dplyr::summarise(
    RPQB = mean(CustomScoring)
  )

ALLRPAvg  = StatCustom %>%
  dplyr::filter(!is.na(CustomScoring))%>%
  dplyr::filter(dplyr::case_when(
    Pos=="QB" ~ is.na(FlexRank),
    Pos=="RB" ~ PosRank > (RP["TM"]*RP["RB"]),
    Pos=="WR" ~ PosRank > (RP["TM"]*RP["WR"]),
    Pos=="TE" ~ PosRank > RP["TM"]*RP["TE"]))%>%
  dplyr::filter(dplyr::case_when(
    Pos=="QB" ~ is.na(FlexRank),
    Pos=="RB" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
    Pos=="WR" ~ FlexRank > (RP["TM"]*RP["FLEX"]),
    Pos=="TE" ~ FlexRank > RP["TM"]*RP["FLEX"]))%>%
  dplyr::group_by(Year,Week,Pos)%>%
  slice_max(Sleeper_Start_Dynasty_per,n=extraplayersavg)%>%
  ungroup()%>%
  dplyr::group_by(Year,Week)%>%
  dplyr::summarise(
    RPRB = mean(CustomScoring[Pos=='RB']),
    RPWR = mean(CustomScoring[Pos=='WR']),
    RPTE = mean(CustomScoring[Pos=='TE'])
  )

RPAvg <- QBRPAvg %>%
  left_join (ALLRPAvg, by = c("Year","Week"))

Base_PlayersAvg = StatCustom %>%
  dplyr::filter(!is.na(CustomScoring))%>%
  dplyr::filter(dplyr::case_when(
    Pos=="QB" ~ PosRank <= (RP["TM"]*RP["QB"]),
    Pos=="RB" ~ (PosRank <= (RP["TM"]*RP["RB"])|FlexRank <= (RP["TM"]*RP["FLEX"])),
    Pos=="WR" ~ (PosRank <= (RP["TM"]*RP["WR"])|FlexRank <= (RP["TM"]*RP["FLEX"])),
    Pos=="TE" ~ (PosRank <= (RP["TM"]*RP["TE"])|FlexRank <= (RP["TM"]*RP["FLEX"]))
  )
  )%>%
  dplyr::group_by(Year,Week) %>%
  dplyr::summarise(
    CouQB = length(CustomScoring[(Pos=='QB'&is.na(FlexRank))]),
    AvgQB = mean(CustomScoring[(Pos=='QB'&is.na(FlexRank))]),
    StdQB = stats::sd(CustomScoring[(Pos=='QB'&is.na(FlexRank))]),
    VarQB = stats::var(CustomScoring[(Pos=='QB'&is.na(FlexRank))]),
    CouRB = length(CustomScoring[(Pos=='RB'&is.na(FlexRank))]),
    AvgRB = mean(CustomScoring[(Pos=='RB'&is.na(FlexRank))]),
    StdRB = stats::sd(CustomScoring[(Pos=='RB'&is.na(FlexRank))]),
    VarRB = stats::var(CustomScoring[(Pos=='RB'&is.na(FlexRank))]),
    CouWR = length(CustomScoring[(Pos=='WR'&is.na(FlexRank))]),
    AvgWR = mean(CustomScoring[(Pos=='WR'&is.na(FlexRank))]),
    StdWR = stats::sd(CustomScoring[(Pos=='WR'&is.na(FlexRank))]),
    VarWR = stats::var(CustomScoring[(Pos=='WR'&is.na(FlexRank))]),
    CouTE = length(CustomScoring[(Pos=='TE'&is.na(FlexRank))]),
    AvgTE = mean(CustomScoring[(Pos=='TE'&is.na(FlexRank))]),
    StdTE = stats::sd(CustomScoring[(Pos=='TE'&is.na(FlexRank))]),
    VarTE = stats::var(CustomScoring[(Pos=='TE'&is.na(FlexRank))]),
    CouFLEX = length(CustomScoring[(!is.na(FlexRank))]),
    AvgFLEX = mean(CustomScoring[(!is.na(FlexRank))]),
    StdFLEX = stats::sd(CustomScoring[(!is.na(FlexRank))]),
    VarFLEX = stats::var(CustomScoring[(!is.na(FlexRank))]),
    AvgTMScore = sum((AvgQB*RP["QB"]),(AvgRB*RP["RB"]),(AvgWR*RP["WR"]),(AvgTE*RP["TE"]),(AvgFLEX*RP["FLEX"])),
    StdTMScore = sqrt(
      sum(
        ((RP["TM"]*RP["QB"])-1)*(VarQB^2),
        ((RP["TM"]*RP["RB"])-1)*(VarRB^2),
        ((RP["TM"]*RP["WR"])-1)*(VarWR^2),
        ((RP["TM"]*RP["TE"])-1)*(VarTE^2),
        ((RP["TM"]*RP["FLEX"])-1)*(VarFLEX^2))
      /
        sum(
          ((RP["TM"]*RP["QB"])-1),
          ((RP["TM"]*RP["RB"])-1),
          ((RP["TM"]*RP["WR"])-1),
          ((RP["TM"]*RP["TE"])-1),
          ((RP["TM"]*RP["FLEX"])-1)
        )
    )
  )%>%
  dplyr::left_join (RPAvg, by = c("Year","Week"))%>%
  dplyr::mutate(
    WinWQBRP = stats::pnorm((RPQB-AvgQB)/StdTMScore),
    WinWRBRP = stats::pnorm((RPRB-AvgRB)/StdTMScore),
    WinWWRRP = stats::pnorm((RPWR-AvgWR)/StdTMScore),
    WinWTERP = stats::pnorm((RPTE-AvgTE)/StdTMScore)
  )

# Generate the Points over replacement player and the win odds
StatCustom = StatCustom %>%
  dplyr::left_join(Base_PlayersAvg, by = c("Year","Week"))%>%
  dplyr::group_by(ID_PFF,Week,Year)%>%
  dplyr::mutate(
    PoAP = round(dplyr::case_when(Pos=="QB" ~ CustomScoring-AvgQB,Pos=="RB" ~ CustomScoring-AvgRB,Pos=="WR" ~ CustomScoring-AvgWR,Pos=="TE" ~ CustomScoring-AvgTE),2),
    PoRP = round(dplyr::case_when(Pos=="QB" ~ CustomScoring-RPQB,Pos=="RB" ~ CustomScoring-RPRB,Pos=="WR" ~ CustomScoring-RPWR,Pos=="TE" ~ CustomScoring-RPTE),2),
    WinW = stats::pnorm(PoAP/StdTMScore)
  )

# Generate the WAR substracting the Replacement player level of the position vs that player
WarPlayerData = StatCustom %>%
  dplyr::group_by(Year,ID_PFF) %>%
  dplyr::summarize(
    FPSoRP = round(sum(PoRP),3),
    FPSoAP = round(sum(PoAP),3),
    WinSoRP = round(dplyr::case_when(Pos=="QB" ~ sum(WinW)-sum(WinWQBRP),Pos=="RB" ~ sum(WinW)-sum(WinWRBRP),Pos=="WR" ~ sum(WinW)-sum(WinWWRRP),Pos=="TE" ~ sum(WinW)-sum(WinWTERP)),3),
    Games_Played = n(),
    WinsoRP_gp = round(WinSoRP/Games_Played,3)
  )

#Generate the dataframe to calculate data.
WORP = unique(AdeStats[c("ID_PFF","Year","Player_Name","Team","Pos","Physical_Age")])%>%
  dplyr::left_join(dplyr::distinct(WarPlayerData,ID_PFF, .keep_all = T))%>%
  dplyr::left_join(nflfastR::teams_colors_logos %>%
    dplyr::mutate(team_abbr = replace(team_abbr, team_abbr=="JAX","JAC"))%>%
    dplyr::select(team_abbr,team_logo_espn,team_color),by=c("Team"="team_abbr"))%>%
  dplyr::left_join(dplyr::distinct(Rosters_headshots,pff_id,headshot_url,season) ,by=c("ID_PFF"="pff_id","Year"="season"))%>%
  dplyr::left_join(ADPData %>% dplyr::select(ADP_RD_1QB,ADP_RD_1QB_Pos_Rank,ID_PFF,Year) ,by=c("ID_PFF","Year"), .keep_all = T)

Start_rate <- TRUE
