asp_ratio <- 1.618
link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

WORPGT_df <- WORP %>%
  dplyr::filter(Year==WoRP_SingleYear)%>%
  dplyr::group_by(Year,Pos)%>%
  dplyr::slice_max(order_by = WinSoRP, n=WoRP_TopPlayers_Single, with_ties = FALSE)%>%
  dplyr::mutate(PosRank=rank(-WinSoRP,ties.method="first"))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(
    Rank = rank(-WinSoRP,ties.method="random"),
    PosRankText = paste0(Pos,PosRank)
  )%>%
  mutate(headshot = link_to_img(headshot_url))%>%
  ungroup()

WORPGT_graph <-WORPGT_df %>%
  dplyr::filter(Year==WoRP_SingleYear)%>%
  dplyr::select(Rank,"Player"=headshot_url,"Name"=Player_Name,"Team"=team_logo_espn,"WoRP"=WinSoRP,"FPoRP"=FPSoRP,"Position"=PosRankText)%>%
  dplyr::arrange(Rank)%>%
  dplyr::slice(1:50)%>%
  # dplyr::group_by(Pos)%>%
  gt::gt()%>%
  gt_img_rows(columns = Team, height = 25)%>%
  gt_img_rows(columns = Player, height = 25)%>%
  tab_header(
    title = md(paste0("WoRP Pos Data")),
    subtitle = md(paste0("Weeks 1-16 Top",WoRP_TopPlayers_Single))
  )%>%
  gtExtras::gt_theme_538()%>%
  fmt_number(columns = c("WoRP","FPoRP"), decimals = 2) %>%
  tab_source_note(
    source_note = md(paste0("**Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
  **Pass Scoring:** TD:",ScoringValues["Pass_TD"]," Int:",ScoringValues["Pass_INT"]," Sack: ",ScoringValues["Pass_Sack"],"<br>
  **Rec Scoring:** RB:",ScoringValues["Rec_RB"]," WR: ",ScoringValues["Rec_WR"]," TE: ",ScoringValues["Rec_TE"])))

gt::gtsave(
  data = WORPGT_graph,
  filename = paste0("Images/WoRP_GT_",WoRP_SingleYear,"_",as.numeric(Sys.time()),".png"),
)

WORP_plot <- WORPGT_df %>%
  dplyr::filter(Year==WoRP_SingleYear)%>%
  dplyr::arrange(desc(WinSoRP))%>%
  dplyr::slice(1:50)%>%
  # dplyr::mutate(Rank=rank(-aWORP,ties.method="min"))%>%
  ggplot() +
  geom_col(
    aes(
      x = WinSoRP, y = fct_reorder(Player_Name, WinSoRP),
      fill = Pos,
    ),
    width = 0.7
  ) +
  geom_text(
    aes(x = WinSoRP, y = fct_reorder(Player_Name, WinSoRP), label = paste0(Rank," - ",Pos,PosRank)),
    hjust = -0.1, size = 3,
    position = position_stack(vjust = 0),
    inherit.aes = TRUE
  )+
  geom_text(
    aes(x = WinSoRP, y = fct_reorder(Player_Name, WinSoRP), label = format(WinSoRP, nsmall = 2)),
    hjust = -0.5, size = 3.5,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  )+
  ggimage::geom_image(aes(x = -0.05, y = fct_reorder(Player_Name, WinSoRP), image = headshot_url),
  size = 0.03, by = "width",
  ) +
  scale_fill_manual(values = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#ffe599","TE"="#9fc5e8"))+
  theme_fivethirtyeight()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(x = "\nWoRP",
       y = NULL,
       title = paste0("WoRP - ",WoRP_SingleYear," Season"),
       subtitle = "Weeks 1-16",
       caption = paste0("**Data:** @PFF | **Plot:** @Adeiko_ff<br>
                        **Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
                        **Rec Scoring:** RB:",ScoringValues["Rec_RB"]," WR: ",ScoringValues["Rec_WR"]," TE: ",ScoringValues["Rec_TE"],"<br>
                        **Pass Scoring:** TD:",ScoringValues["Pass_TD"]," Int:",ScoringValues["Pass_INT"]," Sack: ",ScoringValues["Pass_Sack"]
         )) +
    theme(
      text = element_text(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_markdown(size = 12),
      axis.text = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_markdown(margin = margin(r = -20, unit = "pt"),size = 6),
    )

# WORP_plot
ggplot2::ggsave(
  plot = WORP_plot,
  filename = paste0("Images/WoRP_Bars_",WoRP_SingleYear,"_",as.numeric(Sys.time()),".jpg"),
  dpi = "retina",
  width = 10,
  height = 10* asp_ratio
)

