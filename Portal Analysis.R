## Raw Data:
Portal_Players <- cfbd_recruiting_transfer_portal(2025) %>%
  mutate(
    season = season - 1,
    name = paste0(first_name, " ", last_name),
    in_portal = 1
  ) %>%
  rename(team = origin) %>%
  select(season, team, name, in_portal, transfer_date)


bowl_schedule <- cfbfastR::cfbd_game_info(year = 2024, season_type = "postseason")

get_optouts <- function(url = "https://m.sportsbookreview.com/picks/college-football/ncaaf-bowl-game-opt-out-tracker/"){
  data <- read_html(url)
  
  # Keep Key Players Data
  key_players <- data %>%
    html_table()
  key_players <- key_players[[1]] %>%
    separate(Player, into = c('name', 'position', 'team'), sep = ", ") %>%
    select(-`Bowl matchup`)
  
  # Go Through Other Injuries
  h3_elements <- data %>%
    html_elements("h3")
  h3_texts <- h3_elements %>%
    html_text(trim = TRUE)
  
  get_ui_between_h3_h4 <- function(h3_node) {
    # Locate the next <h4> element
    next_h4 <- h3_node %>%
      html_node(xpath = "./following-sibling::h4[1]")
    
    # Extract <ul> elements between <h3> and the next <h4>
    h3_node %>%
      html_nodes(xpath = "./following-sibling::*[self::ul or self::h4][preceding-sibling::h3[1]]") %>%
      .[html_name(.) == "ul"] %>%
      html_text(trim = TRUE)
  }
  
  # Apply function to each <h3>
  ui_lists_by_h3 <- lapply(h3_elements, get_ui_between_h3_h4)
  
  # Combine the results with their corresponding <h3> headers
  result <- tibble(h3_header = h3_texts, ui_elements = ui_lists_by_h3)  %>%
    filter(h3_header != "Key college football bowl opt-outs & transfers") %>%
    filter(!str_detect(h3_header, " Bowl odds")) %>%
    filter(str_detect(h3_header, " opt-outs"))
  
  players <- str_split(result$ui_elements[[1]], pattern = "(?<=\\))", simplify = FALSE)
  
  # 2. Flatten the list of players into one vector
  players_flat <- unlist(players)
  
  # 3. Extract details using regex
  player_df <- tibble(
    raw_text = players_flat
  ) %>%
    mutate(
      name = str_extract(raw_text, "^[^,]+"),
      name = trimws(name),
      position = str_extract(raw_text, "(?<=, )[^ ]+"),
      team = NA,
      Reason = str_extract(raw_text, "(?<=\\().+?(?=\\))")
    ) %>%
    select(name, position, team, Reason) %>%
    filter(!is.na(name))
  
  # View the dataframe
  
  less_raw <- rbind(key_players, player_df %>% filter(name%notin% key_players$name))
    
  
  bowl_rosters <- rosters %>%
    filter(team %in% bowl_schedule$home_team | team %in% bowl_schedule$away_team) %>%
    filter(athlete_id %notin% c(4686607, 550577)) %>%
    mutate(
      name = case_when(
        name == 'Jordan Brown' & team == 'Georgia Tech' ~ 'Jordan Brown (GT)',
        name == 'Jordan Brown' & team == 'Texas Tech' ~ 'Jordan Brown (TT)',
        TRUE ~ name)
    )
  
  
  final_df <- less_raw %>%
    filter(!str_detect(tolower(Reason), "(injury - questionable)")) %>%
    mutate(
      position = case_when(
        name == 'Alijah Williams' ~ 'LB',
        name == 'Jaitlin Hampton' ~ 'DB',
        TRUE ~ position
      ),
      name = case_when(
        name == 'Jordan Brown' & position == 'OL' ~ 'Jordan Brown (GT)',
        name == 'Jordan Brown' & position == 'WR' ~ 'Jordan Brown (TT)',
        name == 'Luther Burden' ~ 'Luther Burden III',
        name == 'KC Concepcion' ~ 'Kevin Concepcion',
        name == 'Tyrell Simmons' ~ 'Tyrell Simmons Jr.',
        name == 'LT Sanders' ~ 'L.T. Sanders',
        name == "K'wan Powell" ~ "K'Wan Powell",
        name == 'Alonza Barnett' ~ "Alonza Barnett III",
        name == "Saveon Brown" ~ "SaVeon Brown",
        name == "Kendrick Dujor" ~ "Kendrick DuJour",
        name == "Richard Outland" ~ "Richard Outland Jr.",
        name == 'Julian Humphrey' ~ 'Julian Humphrey',
        name == 'Allen Jones' ~ 'Allen Jones Jr.',
        name == 'Aamaris Brown-Bunkley' ~ 'Aamaris Brown',
        name == 'Adonis Forrest Jr.' ~ 'Adonis Forrest',
        name == 'Alonzo Ford' ~ 'Alonzo Ford Jr.',
        name == 'Alvin Gulley Jr.' ~ 'Timothy Gulley',
        name == 'Alzillion Hamilton' ~ "Al'zillion Hamilton",
        name == 'Amari McNeil' ~ 'Amari McNeill',
        name == 'Antonio Tripp' ~ 'Antonio Tripp Jr.',
        name == 'Antony Lucas' ~ 'Anthony Lucas',
        name == 'Aubrey Bruks' ~ 'Aubrey Burks',
        name == 'BJ Blake' ~ 'B.J. Blake',
        name == 'BJ Diakite' ~ 'Boubacar Diakite',
        name == 'Brade Spence' ~ 'Brad Spence',
        name == 'Brady Keim' ~ 'Brayden Keim',
        name == 'CJ Carr' ~ 'C.J. Carr',
        name == 'CJ Wilson' ~ 'CJ Wilson Jr.',
        name == 'Cam Davis' ~ 'Cameron Davis',
        name == 'Carl Swanson' ~ 'Cal Swanson',
        name == 'Carl Williams' & position == 'S' ~ 'Carl Williams IV',
        name == 'Case Uluave' ~ 'Cade Uluave',
        name == 'Corey Coley Jr.' ~ 'Corey Coley',
        name == 'Curley Reed' ~ 'Curley Reed III',
        name == "Dal'mont Gourdine"~ "Dal'Mont Gourdine",
        name == 'Damieon George' ~ 'Damieon George Jr.',
        name == 'Danny Lewis' ~ 'Danny Lewis Jr.',
        name == 'David Oijegbe' ~ 'David Ojiegbe',
        name == "De'Andre Coleman" ~ 'Zachary Coleman',
        name == "De'Corian Clark" ~ 'JT Clark',
        name == "Dea'Vonn Hall" ~ "Dae'vonn Hall",
        name == 'Derek Williams' ~ 'Derek Williams Jr.',
        name == 'Deuce Spurlock' ~ 'Deuce Spurlock II',
        name == 'Don Chaney' ~ 'Donald Chaney',
        name == 'Drak Knobloch' ~ 'Drake Knobloch',
        name == 'Dylan Lonegran' ~ 'Dylan Lonergan',
        name == 'Dyoni Hill' ~ "D'Yoni Hill",
        name == 'E.J. Williams' ~ 'E.J. Williams Jr.',
        name == 'Eugene Wilson' ~ 'Eugene Wilson III',
        name == 'Fernando Garza' ~ 'Fernando Garza III',
        name == 'Greg Turner' ~ 'Greg Turner III',
        name == "Gunnar Givens" ~ "Gunner Givens",
        name == "Harold Perkins" ~ "Harold Perkins Jr.",
        name == "Harvey Dyson" ~ "Harvey Dyson III",
        name == "Henry Parrish" ~ "Henry Parrish Jr.",
        name == "Howard Cross" ~ "Howard Cross III",
        name == "Jack McCalloster" ~ "Jack McCallister",
        name == "Jacob Katauska" ~ "Jacob Katauskas",
        name == "Jaheim Oatis" ~ "Jehiem Oatis",
        name == "Jalen Dupree" ~ "Darrion Dupree",
        name == "Jason Marshall" ~ "Jason Marshall Jr.",
        name == "Jay'via Suggs" ~ "Jay'Viar Suggs",
        name == "Jayden Wright" ~ "Jaydon Wright",
        name == "John Emery" ~ "John Emery Jr.",
        name == "Josh Eaton" ~ "Joshua Eaton",
        name == "Kade Mcintyre" ~ "Kade McIntyre",
        name == "Kendal Dolby" ~ "Kendel Dolby",
        name == "Kendrick Dujour" ~ "Kendrick DuJour",
        name == "Kevin Longstreet" ~ "Kevin Longstreet Jr.",
        name == "Kevin Winston" ~ "Kevin Winston Jr.",
        name == "Larry Moore" ~ "Larry Moore III",
        name == "Mark Nabou" ~ "Mark Nabou Jr.",
        name == "Martrell Harris" ~ "Martrell Harris Jr.",
        name == "Max Moss" ~ "Maxwell Moss",
        name == "Mose Phillips" ~ "Mose Phillips III",
        name == "Naejuan Barber" ~ "Naejaun Barber",
        name == "Nick Booker-Brown" ~ "Brandon Brown",
        name == "Quandarrius Robinson" ~ "Que Robinson",
        name == "RJ Garcia" ~ "RJ Garcia II",
        name == "Russell Davis" ~ "Russell Davis II",
        name == "Rusty Vanwetzinga" ~ "Rusty VanWetzinga",
        name == "Shadrach Banks" ~ "Shad Banks Jr.",
        name == "Sherrod Covil" ~ "Sherrod Covil Jr.",
        name == "Terrence Carter" ~ "Terrance Carter",
        name == "Terrence Cooks" ~ "Terrence Cooks Jr.",
        name == "Terry Kirksey" ~ "Terry Kirksey Jr.",
        name == "Tim Grear" ~ "Tim Grear Jr.",
        name == "Tre Wilson" ~ "Eugene Wilson III",
        name == "Trent Jones" ~ "Trent Jones II",
        name == "Tyrus Washington" ~ "Ty Washington",
        name == "Vince Fiacable" ~ "Vincent Fiacable",
        name == "Warren Smith" ~ "Warren Smith Jr.",
        TRUE ~ name
      )
    ) %>%
    distinct() %>%
    left_join(bowl_rosters, by = 'name') %>%
    mutate(
      team = team.y
    ) %>%
    select(-team.x, -team.y) %>%
    filter(!is.na(position.x)) %>%
    mutate(
      name = case_when(
        name == 'Jordan Brown (GT)' ~ 'Jordan Brown',
        name == 'Jordan Brown (TT)' ~ 'Jordan Brown',
        TRUE ~ name
      )
    )
  
  return(final_df)
}
raw_optouts <- get_optouts() %>% distinct()

missing_coaches <- raw_optouts %>% filter(position.x %in% c('HC', 'DC', 'OC')) %>% select(name, position.x, Reason)

raw_optouts <- raw_optouts %>%
  filter(position.x %notin% c('HC', 'DC', 'OC')) %>%
  filter(!is.na(athlete_id))

# Portal
build_full_portal <- function(){
  
  portal <- raw_optouts %>%
    select(athlete_id, Reason)
  
  portal %>% glimpse()
  
  # Raw Usage
  usage <- cfbd_player_usage(year = 2024)  %>%
    select(-c(usg_1st_down, usg_2nd_down, usg_3rd_down, usg_standard_downs, usg_passing_downs))
  usage %>% glimpse()
  
  # Raw Stats
  ## Player
  ply_stats <- cfbd_metrics_ppa_players_season(year = 2024) %>%
    select(season, athlete_id, name, position, team, conference,
           countable_plays,
           ends_with("_PPA_all"),
           ends_with("_PPA_pass"),
           ends_with("_PPA_rush")
    )
  ## Team (For Percentage)
  tm_stats <- cfbd_metrics_ppa_teams(year = 2024) %>%
    mutate(
      across(off_overall:def_cumulative_rushing, ~ as.numeric(.))
    ) %>%
    select(-ends_with("first_down"), -ends_with("second_down"), -ends_with("third_down"))
  
  ## Join Stats
  stats <- ply_stats %>%
    left_join(tm_stats, by = c('season', 'team', 'conference')) %>%
    mutate(
      ply_pct_off_all_PPA = total_PPA_all / off_cumulative_total,
      ply_pct_off_pass_PPA = total_PPA_pass / off_cumulative_passing,
      ply_pct_off_rush_PPA = total_PPA_rush / off_cumulative_rushing
    ) 
  stats %>% glimpse()
  
  # Get FBS Teams
  fbs_teams <- teams %>% filter(classification == 'fbs') %>% pull(school)
  
  # Join Stats To Portal
  portal_df <- portal %>%
    left_join(usage, by = c('athlete_id')) %>%
    left_join(stats, by = c('athlete_id')) %>%
    rename(
      season = season.x,
      name = name.x,
      team = team.x,
      conference = conference.x,
      position = position.y
    ) %>%
    select(-ends_with(".y"), -position.x)
  
  return(portal_df)
  
}
portal_stats <- build_full_portal()

portal_stats %>% glimpse()

build_depth_adjustments <- function(tm){
  
  # Get Portal Players
  raw_portal <- portal_stats %>%
    group_by(season, team, position) %>%
    mutate(Depth_Rank = dense_rank(desc(usg_overall))) %>%
    ungroup()
  
  # Get Depth Chart
  DC_DF <- raw_portal %>%
    filter(team == tm) %>%
    arrange(position, Depth_Rank) %>%
    select(team, name, position, Depth_Rank, Reason, countable_plays, total_PPA_all, avg_PPA_all, starts_with("usg_"))
  
  print(DC_DF)
  
}
build_depth_adjustments(tm = "Fresno State")




Team_Portal <- portal_stats %>%
  filter(!is.na(athlete_id)) %>%
  filter(team %in% bowl_schedule$home_team | team %in% bowl_schedule$away_team) %>%
  group_by(team) %>%
  #group_by(team, name, position) %>%
  summarise(
    Total_Players = n_distinct(athlete_id),
    Total_Plays = sum(countable_plays, na_rm = TRUE),
    Total_Portal_Passing_PPA = sum(total_PPA_pass, na_rm = TRUE) / 2,
    Total_Portal_Rushing_PPA = sum(total_PPA_rush, na_rm = TRUE),
    Total_Team_PPA = max(off_cumulative_total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    across(Total_Portal_Passing_PPA:Total_Team_PPA, ~ if_else(is.na(.), 0, .)),
    Total_Team_Portal_PPA = Total_Portal_Passing_PPA + Total_Portal_Rushing_PPA,
    Percent_Portal_PPA = Total_Team_Portal_PPA / Total_Team_PPA
  ) %>%
  arrange(-Percent_Portal_PPA)

names(Team_Portal) <- c('Team', "Players_In_Portal", 'n_Plays', 'Portal_EPA_Pass', 'Portal_Rush_EPA', 'Portal_Total_EPA', "Total_EPA", "Pct_EPA_In_Portal")
