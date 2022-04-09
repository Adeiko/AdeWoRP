WORPADP = unique(AdeStats[c("ID_PFF","Year","Player_Name","Team","Pos","Physical_Age")])%>%
  dplyr::left_join(dplyr::distinct(WarPlayerData,ID_PFF, .keep_all = T))%>%
  dplyr::left_join(nflfastR::teams_colors_logos %>% dplyr::select(team_abbr,team_logo_espn,team_color),by=c("Team"="team_abbr"))%>%
  dplyr::left_join(dplyr::distinct(Rosters_headshots,pff_id,headshot_url,season) ,by=c("ID_PFF"="pff_id","Year"="season"))%>%
  dplyr::left_join(ADPData %>%dplyr::filter(Pos!="FB")%>% dplyr::select(ADP_RD_1QB,ADP_RD_1QB_Pos_Rank,ID_PFF,Year) ,by=c("ID_PFF","Year"), .keep_all = T)

WORPADP_pred <- WORPADP %>%
  dplyr::filter(Year>=ADP_MinTopYear,Year<=ADP_MaxTopYear)%>%
  dplyr::group_by(Year,Pos)%>%
  dplyr::mutate(WoRP_PosRank=rank(-WinSoRP,ties.method="first"))%>%
  dplyr::filter(ADP_RD_1QB_Pos_Rank<ADP_Players,!is.na(ADP_RD_1QB_Pos_Rank))%>%
  dplyr::ungroup()

ScatMerge <- ggscatter(WORPADP_pred,
  x="ADP_RD_1QB_Pos_Rank", y="WinSoRP", xlab="ADP_PosRank", ylab="WoRP",
  conf.int = TRUE,  add = "loess",size = 0.75, alpha=.3,
  color = "Pos", fill= "Pos",
  ylim = c(0,2), breaks = c(seq(from = 0, to = 2, by = 0.5)),
  )+
  scale_x_continuous(limits=c(1, ADP_Players),breaks = c(seq(from = 0, to = ADP_Players, by = 6)))+
  grids(linetype = 1)+
  fill_palette(palette = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#ffe599","TE"="#9fc5e8"))+
  color_palette(palette = c("QB"="#83A475", "RB"="#B76666", "WR"="#CCB266","TE"="#6C92B5"))+
  labs(title=paste0("**WoRP** predictions by **ADP Posrank**"),
       subtitle=paste0("Stats from Week 1-16 years ",ADP_MinTopYear,"-",ADP_MaxTopYear," and ADP from Redraft"),
       caption = paste0("**Data:** @PFF | **ADP:** @MyFantasyLeague | **Plot:** @Adeiko_ff<br>
                        **Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
                        **Rec Scoring:** RB:",ScoringValues["Rec_RB"]," WR: ",ScoringValues["Rec_WR"]," TE: ",ScoringValues["Rec_TE"],"<br>
                        **Pass Scoring:** TD:",ScoringValues["Pass_TD"]," Int:",ScoringValues["Pass_INT"]," Sack: ",ScoringValues["Pass_Sack"])) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20),
    plot.subtitle = element_markdown(hjust = 0.5,size = 14),
    plot.caption = element_markdown(size = 12),
    legend.direction = "horizontal",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom")

ggplot2::ggsave(
  plot= ScatMerge, dpi = "retina",  width = 13,  height = 10,
  filename = paste0("Images/WORP_ADP_",ADP_MinTopYear,"-",ADP_MaxTopYear,"_",as.numeric(Sys.time()),".jpg"))

PlotADP <- function(FilPos, TopPlayers, DotColor){
  WoRPADP_df <- WORPADP %>%
    dplyr::filter(Year>=ADP_MinTopYear,Year<=ADP_MaxTopYear)%>%
    dplyr::group_by(Year,Pos)%>%
    dplyr::mutate(WoRP_PosRank=rank(-WinSoRP,ties.method="first"))%>%
    dplyr::filter(Pos==FilPos,ADP_RD_1QB_Pos_Rank<TopPlayers,!is.na(ADP_RD_1QB_Pos_Rank))%>%
    dplyr::ungroup()

  WoRPADP_scat = ggscatter(WoRPADP_df,
                           x="ADP_RD_1QB_Pos_Rank", y="WinSoRP", xlab="ADP_PosRank", ylab="WoRP",
                           conf.int = TRUE, color= "Pos", add = "loess",size = 1.5, title = FilPos,
                           palette = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#ffe599","TE"="#9fc5e8"),
                           ylim = c(-0.5, 2), breaks = c(seq(from = -0.5, to = 2, by = 0.5)))+
                           grids(linetype = "dashed")+
                           theme(plot.title = element_text(hjust = 0.5))
  return (WoRPADP_scat)
}

ScatQB <- PlotADP("QB",32,"#b6d7a8")
ScatRB <- PlotADP("RB",32,"#ea9999")
ScatWR <- PlotADP("WR",48,"#ffe599")
ScatTE <- PlotADP("TE",24,"#9fc5e8")
ScatGrid <- annotate_figure(
            ggarrange(ScatQB,ScatRB,ScatWR,ScatTE,ncol = 2, nrow = 2,common.legend = TRUE, legend="bottom"),
                      top = richtext_grob(paste0("**WoRP** (Week 1-16) predictions by **ADP Posrank** ",ADP_MinTopYear,"-",ADP_MaxTopYear)),
                      bottom = richtext_grob(paste0("**Data:** @PFF | **ADP:** @MyFantasyLeague | **Plot:** @Adeiko_ff"),hjust = 1, x = 1))

ggplot2::ggsave(
  plot = ScatGrid, dpi = "retina",  width = 13,  height = 10,
  filename = paste0("Images/WORP_ADP_Grid_",ADP_MinTopYear,"-",ADP_MaxTopYear,"_",as.numeric(Sys.time()),".jpg"))
