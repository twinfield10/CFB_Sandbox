#install.packages("cfbfastR")
#if (!requireNamespace('pacman', quietly = TRUE)){
#  install.packages('pacman')
#}
#pacman::p_load_current_gh("sportsdataverse/recruitR")
#install.packages("tictoc")

library(tidyverse)
library(pacman)
library(cfbfastR)
library(tictoc)
library(rvest)
library(ggrepel)
library(ggimage)
`%notin%` <- Negate(`%in%`)
#library(recruitR)

## CFB FAST R - Data Check (Data only up to 2014) ##
seasons <- 2014:cfbfastR:::most_recent_cfb_season()

## Teams ##
build_teams <- function(start_year = 2000, fbs_teams = TRUE){
  year_load <- seq(start_year, most_recent_cfb_season())
  
  cfbd_team_info <- c()
  
  for(i in length(year_load)){
    df <- cfbd_team_info(year = year_list[i], only_fbs = fbs_teams) %>%
      mutate(season = year_list[i])
    df_list[[i]] <- df
  }
}
tictoc::tic()

teams <- load_cfb_teams()

teams <- cfbd_team_info(year = 2024, only_fbs = FALSE) %>%
  select(-logo) %>%
  left_join(read_csv("https://raw.githubusercontent.com/natemanzo/cfb_data/master/_team_logos.csv", show_col_types = FALSE), by = 'school')
print(paste0("Loaded Teams | Number of Rows: ",nrow(teams), " | Number of Columns: ", ncol(teams)))
tictoc::toc()

## Schedule ##
tictoc::tic()
schedule <- data.frame()
progressr::with_progress({
  schedule <- cfbfastR::load_cfb_schedules(seasons = seasons)
  schedule <- schedule %>% filter(season == 2024)
  schedule %>% head()
})
print("Schedule Load Time:")
tictoc::toc()

# Get Red River ID
#schedule %>% filter(season == 2023 & neutral_site == TRUE & week == 5) %>% print.data.frame()
# Red River ID = 401525861

## Rosters ##
tictoc::tic()
rosters <- data.frame()
progressr::with_progress({
  rosters <- cfbfastR::load_cfb_rosters(seasons = 2024)
  print(paste0("Loaded Rosters | Number of Rows: ",nrow(rosters), " | Number of Columns: ", ncol(rosters)))
  rosters <- rosters %>%
    mutate(
      team = case_when(
        team == 'UT San Antonio' ~ 'UTSA',
        team == 'Louisiana Monroe' ~ 'UL Monroe',
        team == 'Sam Houston State' ~ 'Sam Houston',
        team == 'UMass' ~ 'Massachusetts',
        team == 'Connecticut' ~ 'UConn',
        team == 'Appalachian State' ~ 'App State',
        team == 'Southern Mississippi' ~ 'Southern Miss',
        TRUE ~ team
      )
    )
})
print("Roster Load Time:")
tictoc::toc()


## Play by Play - Back To 2014 ##
tictoc::tic()
pbp <- data.frame()
progressr::with_progress({
  pbp <- cfbfastR::load_cfb_pbp(2024)
  print(paste0("Loaded PBP | Number of Rows: ",nrow(pbp), " | Number of Columns: ", ncol(pbp)))
  gc()
})
print("PBP Load Time:")
tictoc::toc()

clean_pbp <- function(szn){
  pbp <- cfbfastR::load_cfb_pbp(szn, season_type = 'both')
  
  ## Fill Player ID (Not Working At The Moment)
  fix_player_id <- function(data, yr){
    
    # Check Games Without Player_IDs
    
    # Column Prefixes
    player_type_prefixes <- c('passing', 'completion', 'incompletion', 'interception_thrown', 'sack_taken',
                              'rush',
                              'receiving', 'reception', 'target',
                              'touchdown', 'fumble',
                              'interception', 'sack', 'fumble_recovered', 'fumble_forced', 'pass_breakup')
    
    # Get ID Counts
    check_pbp_ids <- data %>%
      mutate(
        passing_player_id = coalesce(completion_player_id, incompletion_player_id, interception_thrown_player_id, sack_taken_player_id),
        passing_player = coalesce(completion_player, incompletion_player, interception_thrown_player, sack_taken_player),
        receiving_player_id = coalesce(reception_player_id, target_player_id),
        receiving_player = coalesce(completion_player, target_player)
      ) %>%
      group_by(game_id, home, away) %>%
      summarise(
        n_plays = n_distinct(id_play),
        across(
          .cols = matches(paste0("^(", paste(player_type_prefixes, collapse = "|"), ").*(_player_id|_player)$")),
          .fns = ~ sum(!is.na(.)),
          .names = "count_{col}"
        ),
        Non_Null_IDs = sum(across(starts_with("count_"), ~ sum(.x, na.rm = TRUE))),
        .groups = 'drop'
      ) %>%
      select(game_id, home, away, Non_Null_IDs) %>%
      filter(Non_Null_IDs == 0)
    
    # Pull Game IDs For Load
    get_ids <- check_pbp_ids %>% pull(game_id) %>% unique()
    
    # Load Player Stats
    clean <- if(length(get_ids) > 0){
      print(paste0("Now Loading Player Stats for Season ", yr, " (",length(get_ids)," Games)"))
      
      all_fix <- cfbd_play_stats_player(year = yr, season_type = "both") %>%
        mutate(
          id_play = as.numeric(play_id),
          across(ends_with('_player_id'), ~ as.numeric(.))
        ) %>%
        select(season, week, game_id, id_play,
               reception_player_id:pass_breakup_stat) %>%
        filter(game_id %in% get_ids) %>%
        distinct()
      
      # Dupes
      all_fix <- all_fix %>%
        group_by(season, week, game_id, id_play) %>%
        mutate(
          cnt = n()
        ) %>%
        ungroup() %>%
        filter(cnt == 1)
      
      fix_cols <- names(all_fix %>% select(reception_player_id:pass_breakup_stat))
      
      print(paste0("Loaded ", length(unique(all_fix$game_id)), " Games From ", yr, " Season | Continue With Join To PBP Data"))
      
      # Join
      clean <- data %>%
        left_join(all_fix, by = c('season', 'week','game_id', 'id_play'))
      
      print(names(clean))
      
      clean <- clean %>%
        mutate(
          across(matches(paste0("^(", paste(fix_cols, collapse = "|"), ").*\\.x$")), ~ coalesce(.x, get(gsub("\\.x$", ".y", cur_column()))), .names = "{.col}")
        )  %>%
        rename_with(~ gsub("\\.x$", "", .), matches("\\.x$")) %>%
        select(-ends_with(".y"))
      
      clean
    } else {
      data
    }
    
    # Create Passing And Receiving ID
    clean <- clean %>%
      mutate(
        passing_player_id = coalesce(completion_player_id, incompletion_player_id, interception_thrown_player_id, sack_taken_player_id),
        passing_player = coalesce(completion_player, incompletion_player, interception_thrown_player, sack_taken_player),
        receiving_player_id = coalesce(reception_player_id, target_player_id),
        receiving_player = coalesce(completion_player, target_player)
      )
    
    return(data)
    
  }
  #result <- fix_player_id(data = pbp, yr = szn)
  
  print(paste0(szn, " Season PBP Loaded with ", nrow(pbp), " Rows"))
  
  return(pbp)
}

pbp %>%
  filter(pos_team == 'Texas', ((passer_player_name == 'Quinn Ewers' & pass == 1) | (rusher_player_name == 'Quinn Ewers' & rush == 1)) ) %>%
  mutate(
    Player = coalesce(passer_player_name, rusher_player_name),
    Game_Status = case_when(
      wp_before <= 0.9 & wp_before >= 0.1 ~ 'Competitive',
      TRUE ~ 'Garbage Time'
    )
  ) %>%
  group_by(pos_team, Player, Game_Status, half) %>%
  summarise(
    n_plays = n_distinct(id_play),
    total_epa = sum(EPA, na.rm = TRUE),
    epa_per_play = total_epa / n_plays,
    comp_pct = sum(completion) / sum(pass_attempt),
    pass_td = sum(pass_td) / n_plays,
    rush_td = sum(rush_td) / n_plays,
    int_pct = sum(int) / n_plays,
    sack_pct = sum(sack) / n_plays,
  ) %>%
  arrange(Game_Status, half)


## EPA Calculations ##
get_player_epa <- function(){
  df <- pbp %>%
    filter(rush == 1 | pass == 1) %>%
    # Build Detail Columns
    mutate(
      IsConf = if_else(offense_conference == defense_conference, 1, 0),
      IsRedZone = if_else(yard_line <= 20, 1, 0),
      IsEarlyDown = if_else(down %in% c(1,2), 1, 0),
      IsHome = if_else(home==pos_team, 1, 0),
      IsGBT = if_else(wp_before > 0.9 | wp_before < 0.1, 1, 0)
    ) %>%
    select(season, game_id, id_play, pos_team, offense_conference,
           passer_player_name, receiver_player_name, rusher_player_name,
           wp_before, wp_after,
           EPA,
           IsConf, IsRedZone, IsEarlyDown, IsHome)
  
  #print(nrow(df %>% filter(!is.na(pass_player_id))))
    
  
  # Combine Offensive Players
  off_df <- bind_rows(
    df %>% filter(!is.na(passer_player_name)) %>% rename(Player = passer_player_name) %>% select(-rusher_player_name, -receiver_player_name),
    df %>% filter(!is.na(rusher_player_name)) %>% rename(Player = rusher_player_name) %>% select(-passer_player_name, -receiver_player_name),
    df %>% filter(!is.na(receiver_player_name)) %>% rename(Player = receiver_player_name) %>% select(-passer_player_name, -rusher_player_name)
  )
  
  #fill_na_df <<- off_df %>% filter(is.na(Player_ID)) %>% select(season, game_id, pos_team, id_play, Player, Player_ID)
  
  # Group BY
  results <- off_df %>%
    group_by(season, pos_team, Player) %>%
    summarise(
      n_games = n_distinct(game_id),
      # All
      n_plays = n_distinct(id_play),
      t_epa = sum(EPA, na.rm = TRUE),
      t_wpa = sum(wp_after - wp_before, na.rm = TRUE),
      epa_per_play = t_epa / n_plays,
      wpa_per_play = t_wpa / n_plays,
      # Non-Garbage Time
      true_plays = n_distinct(if_else(wp_before <= 0.9 & wp_before >= 0.1, id_play, 0)),
      t_true_epa = sum(if_else(wp_before <= 0.9 & wp_before >= 0.1, EPA, 0), na.rm = TRUE),
      t_true_wpa = sum(if_else(wp_before <= 0.9 & wp_before >= 0.1, wp_after - wp_before, 0), na.rm = TRUE),
      true_epa_per_play = t_true_epa / true_plays,
      true_wpa_per_play = t_true_wpa / true_plays,
      # Garbage Time
      gbt_plays = n_distinct(if_else(wp_before > 0.9 | wp_before < 0.1, id_play, 0)),
      t_gbt_epa = sum(if_else(wp_before > 0.9 | wp_before < 0.1, EPA, 0), na.rm = TRUE),
      t_gbt_wpa = sum(if_else(wp_before > 0.9 | wp_before < 0.1, wp_after - wp_before, 0), na.rm = TRUE),
      gbt_epa_per_play = t_gbt_epa / gbt_plays,
      gbt_wpa_per_play = t_gbt_wpa / gbt_plays,
      .groups = "drop"
    )
  
  # Clean Player Names For Join
  bad_ply_vals <- c('TEAM', "incomplete", "pass. Pass incomplete intended",
                    "", "the", "pass. Pass incomplete intended for.",
                    "Brown steps back to pass. Pass", "Team", "gain and",
                    "pass. Sacked at", "Jordon Simmons to", "Keanu Hill Keanu Hill fumbled"
  )
  results <- results %>%
    # Filter Bad Values
    filter(Player %notin% bad_ply_vals) %>%
    filter(!str_detect(Player, "steps back to pass. Pass")) %>%
    # Hardcode Player Names
    mutate(
      Player = case_when(
        Player %in% c("C.J. Ogbonna", "T.J. Harden") ~ str_replace_all(Player, "\\.", ""),
        Player == "Mario Anderson Jr." ~ "Mario Anderson",
        Player == "Joe Fagnano" ~ "Joseph Fagnano",
        Player == "Bill Davis" ~ 'Elijah Davis',
        Player == "W.Howard" ~ 'Will Howard',
        TRUE ~ Player
      )
    )
  
  # Join Player Details - Roster
  slim_roster <- rosters %>% mutate(full_name = paste0(first_name, " ", last_name)) %>% select(season, team, full_name, athlete_id, position, headshot_url)
  slim_team <- teams %>% filter(!is.na(classification)) %>% select(school, mascot, alt_name3, team_id, abbreviation, classification, conference, color, alt_color, logo)
  
  # Add Position For NO Roster Players
  
  results <- results %>%
    left_join(slim_roster, by = c('season', 'pos_team' = 'team', 'Player' = 'full_name')) %>%
    left_join(slim_team, by = c('pos_team' = 'school'))


  return(results %>% arrange(desc(t_epa)))
}
player_epa <- get_player_epa()

  
  
plot_epa <- function(df, play_min = 100){
  
  # Filter DataFrame
  df <- df %>%
    filter(n_plays >= play_min, classification == 'fbs')
  
  x_range <- range(df$true_epa_per_play, na.rm = TRUE)
  y_range <- range(df$gbt_epa_per_play, na.rm = TRUE)
  
  # Plot
  gbt_p <- ggplot(df,
         aes(y = gbt_epa_per_play,
             x = true_epa_per_play,
             label = Player
             )) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_image(aes(image = logo), size = 0.05) +
    geom_text_repel(size = 5) +
    labs(
      y = "Garbage Time EPA per Play (WP > 90% or < 10%)",
      x = "Competitive EPA per Play (WP Between 90% - 10%)",
      title = paste0("EPA Per Play - Competitive vs Non-Garbage Time"),
      subtitle = paste0("Among Players With At Least ", play_min, " Touches")
    ) +
    theme_minimal() +
    annotate("text", x = x_range[2], y = y_range[2], label = "Always Good", hjust = 1, vjust = 1, size = 5, color = "darkgreen") +
    annotate("text", x = x_range[1], y = y_range[2], label = "Better in Garbage Time", hjust = 0, vjust = 1, size = 5, color = "pink") +
    annotate("text", x = x_range[1], y = y_range[1], label = "Always Terrible", hjust = 0.2, vjust = 0, size = 5, color = "#841617") +
    annotate("text", x = x_range[2], y = y_range[1], label = "Better When It Matters", hjust = 1, vjust = 0, size = 5, color = "forestgreen")

  epa_wpa_p <- ggplot(df,
                  aes(y = true_wpa_per_play,
                      x = true_epa_per_play,
                      label = Player)
                  ) +
    geom_point() +
    geom_image(aes(image = logo), size = 0.075) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
    geom_text_repel(size = 5) +
    labs(
      y = "Win Percentage Added Per Play",
      x = "Expected Points Added Per Play",
      title = paste0("Win Percentage Added Per Expected Point"),
      subtitle = paste0("Excluding Garbage Time | Among Players With At Least ", play_min, " Touches")
    ) +
    theme_minimal()
  
  print(gbt_p)
  
}

plot_df <- player_epa %>%
  filter(
    #Player %in% c('Travis Hunter', 'Ashton Jeanty', 'Dillon Gabriel', 'Cam Ward', 'Kurtis Rourke', 'Jalen Milroe', 'Jaxson Dart', 'Quinn Ewers', 'Shedeur Sanders', 'Arch Manning', 'Cade Klubnik', 'Bryson Daily'),
    conference == 'SEC',#,
    #pos_team %notin% c('Tennessee')#,
    position %in% c('QB')
  )

plot_epa(df = plot_df)



teams %>%
  group_by(school) %>%
  mutate(cnt = n()) %>%
  filter(cnt > 1 & classification %in% c('fbs', 'fcs')) %>%
  ungroup() %>%
  arrange(desc(cnt)) %>%
  print.data.frame()

teams %>% filter(school %in% c('Charlotte', 'Troy'))


#pbp %>% head(10000) %>% write_csv(file = 'PBP_Head_Data.csv')

## OTHER DATA SETS ##

#Gambling (Build Function) - UNFINISHED
tictoc::tic()
build_betting <- function(){
  # 2006 to 2016
  df1 <- readRDS(url(
    "https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/master/betting/rds/cfb_lines_odds.rds"
  ))
  
  # 2019 to 2022
  df_list <- c()
  year_list <- seq(2019, 2022)
  for(i in length(year_list)){
    df <- cfbd_betting_lines(year = year_list[i])
    df_list[[i]] <- df
  }
  df2 <- bind_rows(df_list)
  print("Legacy Data Frame Columns:")
  print(colnames(df1))
  df1 %>% head() %>% print.data.frame()
  print("19-22 Data Frame Columns:")
  print(colnames(df2))
  df2 %>% arrange(game_id) %>% head() %>% print.data.frame()
  
  ## Join Columns To Schedule ##
  common_cols <- c("game_id", "season")
}
build_betting()
tictoc::toc()

#Coaches
tictoc::tic()
coaches <- cfbd_coaches()
print(paste0("Loaded Coaches | Number of Rows: ",nrow(coaches), " | Number of Columns: ", ncol(coaches)))
coaches <- coaches %>% filter(year == 2023)
print("Coaches Load Time:")
tictoc::toc()

# Draft
cfbd_draft_picks(year = 2020, college = "Texas")

# Box Scores
cfbd_game_box_advanced(game_id = 401525861)


espn_pbp <- try(espn_cfb_pbp(game_id = 401628319))
cfbdb_pbp <- try(cfbd_pbp_data(year = 2024) %>% filter(game_id == 401628319))
full_pbp <- try(cfbfastR::load_cfb_pbp(2024) %>% filter(!is.na(reception_player_id)))


player_stats_slim <- cfbd_play_stats_player(year = 2024, season_type = "both")
