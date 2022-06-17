WORP_df <- WORP %>%
  dplyr::filter(Year>=WoRP_MinTopYear,Year<=WoRP_MaxTopYear)%>%
  dplyr::group_by(Year,Pos)%>%
  dplyr::slice_max(order_by = WinSoRP, n=WoRP_TopPlayers, with_ties = FALSE)%>%
  dplyr::mutate(PosRank=rank(-WinSoRP,ties.method="first"))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(Pos,PosRank) %>%
  dplyr::select(Year,Pos,PosRank,WinSoRP,Physical_Age)%>%
  dplyr::mutate(
    aWORP = round(mean(WinSoRP),2)
)

xrng <- range(WORP_df$PosRank)
yrng <- range(WORP_df$aWORP)

WORP_Pos_Graph <- ggplot2::ggplot(WORP_df,ggplot2::aes(x = PosRank, y = aWORP, color = Pos))+
  ggplot2::scale_color_manual(values = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#E7B800","TE"="#9fc5e8"))+
  ggplot2::geom_line(size = 1.5) +
  ggplot2::geom_point(size = 2)+
  # ggplot2::geom_text(aes(label = aWORP,color="black",size = 8))+
  ggplot2::geom_hline(yintercept = 0, size = 1, color = "black") +
  ggplot2::scale_x_continuous(breaks = c(1,seq(from = 6, to = WoRP_TopPlayers, by = 6),WoRP_TopPlayers))+
  # ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    # panel.grid.major.y = element_blank(),
    legend.direction = "horizontal",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    # legend.justification = "right",
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
  )+
  ggplot2::labs(x = "PosRank",
       y = "WORP",
       title = paste0(dplyr::case_when(Start_rate==FALSE ~ "WORP Pos Data", TRUE ~ "WORP StartRate Pos Data")),
       subtitle = paste0("Average Years ",WoRP_MinTopYear,"-",WoRP_MaxTopYear," Weeks 1-16"),
       caption = paste0("**Data:** @PFF @MyFantasyLeague ",dplyr::case_when(Start_rate==FALSE ~ "", TRUE ~ "@SleeperHQ "),"| **Plot:** @Adeiko_ff"))+
  ggplot2::annotate(geom = "richtext", x = xrng[2], y = yrng[2],fill="grey90", label.colour = NA,label = paste0("**Roster:** QB: ",RP["QB"],", RB: ",RP["RB"],", WR: ",RP["WR"],", TE: ",RP["TE"],", FLEX: ",RP["FLEX"],"<br>
                      **Pass Scoring:** TD: ",ScoringValues["Pass_TD"],", Int: ",ScoringValues["Pass_INT"],", Sack: ",ScoringValues["Pass_Sack"],"<br>
                      **Rec Scoring:** RB: ",ScoringValues["Rec_RB"],", WR: ",ScoringValues["Rec_WR"],", TE: ",ScoringValues["Rec_TE"]), hjust = "inward", vjust ="inward", size = 4,color="black")


ggplot2::ggsave(
  plot = WORP_Pos_Graph,
  filename = paste0("Images/WoRP_Pos_Graph_",WoRP_MinTopYear,"-",WoRP_MaxTopYear,"_",as.numeric(Sys.time()),".jpg"),
  dpi = "retina",
  width = 10,
  height = 8
)

# WORP_Pos_Graph
WORP_Facet_Pos_Graph <- ggplot2::ggplot(WORP_df,ggplot2::aes(x = PosRank, y = WinSoRP, color = Pos))+
  ggplot2::scale_color_manual(values = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#E7B800","TE"="#9fc5e8"))+
  ggplot2::geom_line(size = 1.5) +
  ggplot2::geom_point(size = 2)+
  ggplot2::geom_hline(yintercept = 0, size = 1, color = "black") +
  ggplot2::scale_x_continuous(breaks = c(1,seq(from = 6, to = WoRP_TopPlayers, by = 6),WoRP_TopPlayers))+
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 12, face = "bold")
  )+
  ggplot2::labs(x = "PosRank",
    y = "WORP",
    title = paste0(dplyr::case_when(Start_rate==FALSE ~ "WORP Pos Data", TRUE ~ "WORP StartRate Pos Data")),
    subtitle = paste0("Weeks 1-16 Top",WoRP_TopPlayers),
    caption = paste0("**Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
  **Pass Scoring:** TD:",ScoringValues["Pass_TD"]," Int:",ScoringValues["Pass_INT"]," Sack: ",ScoringValues["Pass_Sack"],"<br>
  **Rec Scoring:** RB:",ScoringValues["Rec_RB"]," WR: ",ScoringValues["Rec_WR"]," TE: ",ScoringValues["Rec_TE"]))+
  ggplot2::facet_wrap(. ~ Year, ncol = ceiling((WoRP_MaxTopYear-WoRP_MinTopYear)/3))+
  ggplot2::theme(strip.text = ggplot2::element_text(face="bold"))

ggplot2::ggsave(
  plot = WORP_Facet_Pos_Graph,
  filename = paste0("Images/WoRP_Facet_Pos_Graph_",WoRP_MinTopYear,"-",WoRP_MaxTopYear,"_",as.numeric(Sys.time()),".jpg"),
  dpi = "retina",
  width = 13,
  height = 15
)
