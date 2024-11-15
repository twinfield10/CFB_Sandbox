

build_rolling_epa <- function(yr=2024, team, ma_plays){
  
  pbp <- cfbfastR::load_cfb_pbp(yr)
  
  team_info = cfbfastR::cfbd_team_info(year = yr, only_fbs = TRUE)
  
  tm_col = team_info %>% filter(school == team) %>% pull(color)
  tm_col2 = team_info %>% filter(school == team) %>% pull(alt_color)
  
  team_colors_logos = team_info %>% 
    select(school, abbreviation, color, logo, alt_color) %>%
    group_by(school) %>%
    slice(1) %>% 
    ungroup()
  
  off_epa = pbp %>%
    filter(rush == 1 | pass == 1) %>%
    group_by(offense_play, offense_conference) %>%
    summarize(off_epa = mean(EPA, na.rm = TRUE)) %>%
    arrange(desc(off_epa)) %>%
    rename(Team = offense_play) %>%
    filter(offense_conference %in% c('SEC', 'Big 12', 'Big 10', 'ACC')) %>%
    ungroup() %>%
    mutate(Rank = row_number()) %>%
    mutate(TeamRank = paste0(Team, " #", Rank))
  
  cnt_fbs = nrow(off_epa)
  
  off_epa %>%
    filter(off_epa > 0) %>%
    ggplot(aes(x = reorder(TeamRank, off_epa), y=off_epa)) +
    geom_point(size = 3) +
    coord_flip() +
    theme_bw() +
    ylab("Average EPA Per Play") + xlab("") +
    labs(title = "Offensive EPA Per Play | Positive EPA Teams",
         caption = "Chart by @cfbNate
       Data from @CFB_Data via @cfbfastr")
  
  team_off = pbp %>%
    filter(offense_play == team) %>%
    #filter(wp_before <= 0.85 & wp_before > 0.15) %>%
    filter(rush == 1 | pass == 1) %>%
    filter(!is.na(EPA)) %>%
    mutate(cu_epa=cummean(EPA),  #this field is not used in this vignette but it could be substituted later to graph the cumulative EPA
           ma_epa=rollapply(EPA,ma_plays,mean,align='right',fill=NA),
           play_count = row_number(),
           week_team = paste0("WK", ifelse(week > 9, week, paste0(0,week)), " ", defense_play))
  
  team_off_play_start = team_off %>%
    group_by(week_team) %>%
    slice(1) %>%
    select(defense_play, week_team, play_count) %>%
    rename(play_start = play_count,
           team = defense_play)
  
  team_off_play_stop = team_off %>%
    group_by(week_team) %>%
    filter(row_number() == n()) %>%
    select(week_team, play_count) %>%
    rename(play_stop = play_count)
  
  team_off_start_stop = team_off_play_start %>%
    left_join(team_off_play_stop, by = "week_team") %>%
    mutate(midpoint = (play_start + play_stop)/2)
  
  play_count = max(team_off$play_count)
  
  team_off_start_stop = team_off_start_stop %>% 
    left_join(team_colors_logos, by = c("team" = "school")) %>% 
    mutate(color = replace_na(color,"gray")) %>%
    select(team, week_team, play_start, play_stop, midpoint, color)
  
  team_colors <- as.character(team_off_start_stop$color)
  names(team_colors) <- as.character(team_off_start_stop$team)
  
  all_logos = read_csv("https://raw.githubusercontent.com/natemanzo/cfb_data/master/_team_logos.csv", show_col_types = FALSE)  %>%
    mutate(
      school = case_when(
        school == 'UT San Antonio' ~ 'UTSA',
        school == 'Louisiana Monroe' ~ 'UL Monroe',
        school == 'Sam Houston State' ~ 'Sam Houston',
        school == 'UMass' ~ 'Massachusetts',
        school == 'Connecticut' ~ 'UConn',
        school == 'Appalachian State' ~ 'App State',
        school == 'Southern Mississippi' ~ 'Southern Miss',
        TRUE ~ school
      )
    )
  
  team_off_start_stop = team_off_start_stop %>% 
    left_join(all_logos, by = c("team" = "school"))
  
  signature = "@TMWAnalytics"
  
  graph_team_off = ggplot() +
    geom_rect(data = team_off_start_stop, aes(xmin = play_start, xmax = play_stop, fill = team, ymin = -.5, ymax = .9), color = "gray90") +
    geom_rect(data = team_off_start_stop, aes(xmin = play_start, xmax = play_stop, ymin = .8, ymax = 1), color = "gray90", fill = "white") +
    scale_fill_manual(values = alpha(team_colors, 0.25)) +
    geom_hline(yintercept = quantile(off_epa$off_epa), linetype = 2, color = "gray20", alpha = .8) +
    geom_hline(yintercept = 0, linetype = 1, color = "gray20", alpha = .2) +
    geom_image(data=team_off_start_stop,aes(x=midpoint,y=.9,image=logo), asp = 16/9, size = .05) +
    annotate(x = -2, y = quantile(off_epa$off_epa)[1], geom = "text", size = 4, hjust = "right", vjust = 0, label = off_epa %>% slice(n()) %>% pull(Team)) +
    annotate(x = -2, y = quantile(off_epa$off_epa)[2], geom = "text", size = 4, hjust = "right", vjust = 0, label = "25th %ile") +
    annotate(x = -2, y = quantile(off_epa$off_epa)[3], geom = "text", size = 4, hjust = "right", vjust = 0, label = "Median") +
    annotate(x = -2, y = quantile(off_epa$off_epa)[4], geom = "text", size = 4, hjust = "right", vjust = 0, label = "75th %ile") +
    annotate(x = -2, y = quantile(off_epa$off_epa)[5], geom = "text", size = 4, hjust = "right", vjust = 0, label = off_epa %>% slice(1) %>% pull(Team)) +
    geom_line(data = team_off, aes(x = play_count, y = ma_epa), color = "white", size = 2) +
    geom_line(data = team_off, aes(x = play_count, y = ma_epa), size = 1.25) +
    theme_minimal() +
    theme(
      panel.grid = element_blank()
      ,legend.position = "none"
      ,plot.title = element_text(hjust = 0.5, size = 16)
      ,axis.title = element_text(size = 14)
      ,axis.text = element_text(size = 12)
      ) +
    ylab("EPA") + xlab("Number of Plays") +
    labs(title = paste0(team," Offensive EPA | ",ma_plays,"-Play Moving Average"),
         caption = paste0("Chart by ",signature," using code from @cfbNate
       Data from @CFB_Data via @cfbfastR")) +
    coord_cartesian(xlim = c(-20, play_count),  # This leaves room for the labels over the dashed lines
                    clip = 'off')               # This keeps the labels from disappearing
  
  print(graph_team_off)
  
}
build_rolling_epa(team ='Wyoming', ma_plays = 100)
