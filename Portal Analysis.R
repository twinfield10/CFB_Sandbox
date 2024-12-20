# Bowl Schedule
bowl_schedule <- cfbfastR::cfbd_game_info(year = 2024, season_type = "postseason") %>%
  filter(completed == FALSE) %>%
  mutate(
    start_date = with_tz(ymd_hms(start_date, tz = "UTC"), tz = "America/New_York")
  )
# Bowl Rosters
bowl_rosters <- rosters %>%
  filter(team %in% bowl_schedule$home_team | team %in% bowl_schedule$away_team)

bowl_rosters %>% select(position) %>% distinct() %>% arrange(position) %>% pull()

# Stats
get_stats <- function(){
  
  # Player
  player_stats <- suppressMessages(cfbd_stats_season_player(year = 2024)) %>%
    filter(
      player != " Team"
      ,category %in% c('passing', 'rushing', 'receiving', 'defensive', 'interceptions')
    ) %>%
    select(-starts_with('fumbles_'), -starts_with('kick'), -starts_with("punt"), -category) %>%
    mutate(
      across(passing_completions:defensive_td, ~ if_else(is.na(.), 0, .))
    ) %>%
    group_by(team, conference, athlete_id, player) %>%
    summarise(
      across(passing_completions:defensive_td,
             ~ sum(., na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  # Team
  team_stats <- player_stats %>%
    group_by(team, conference) %>%
    summarise(
      # Sum Stats
      across(
        c(
          passing_completions, passing_att, passing_yds, passing_td, passing_int,
          rushing_car, rushing_yds, rushing_td, rushing_long,
          receiving_rec, receiving_yds, receiving_td,
          defensive_solo, defensive_tot, defensive_tfl, defensive_sacks, defensive_qb_hur, defensive_pd, defensive_td,
          interceptions_int, interceptions_yds, interceptions_td
        ),
        ~ sum(., na.rm = TRUE),
        .names = "{.col}_Team"
      ),
      .groups = "drop"
    )
  
  # usage 
  
  # Join
  Large_DF <- player_stats %>%
    left_join(team_stats, by = c('team', 'conference')) %>%
    mutate(
      across(
        c(passing_completions, passing_att, passing_yds, passing_td, passing_int,
          rushing_car, rushing_yds, rushing_td, rushing_long,
          receiving_rec, receiving_yds, receiving_td,
          defensive_solo, defensive_tot, defensive_tfl, defensive_sacks, defensive_qb_hur, defensive_pd, defensive_td,
          interceptions_int, interceptions_yds, interceptions_td
        ),
        ~ if_else(is.nan(. / get(paste0(cur_column(), "_Team"))), 0, . / get(paste0(cur_column(), "_Team"))),
        .names = "{.col}_IndPct"
      )
    )
  
  # Advanced Stats
  
  # Raw Usage
  usage <- cfbd_player_usage(year = 2024, excl_garbage_time = TRUE)  %>%
    select(-c(usg_1st_down, usg_2nd_down, usg_3rd_down, usg_standard_downs, usg_passing_downs))
  
  # Raw Stats
  ## Player
  ply_stats <- cfbd_metrics_ppa_players_season(year = 2024) %>%
    select(team, conference, athlete_id, position,
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
  adv_stats <- ply_stats %>%
    left_join(tm_stats, by = c('team', 'conference')) %>%
    mutate(
      ply_pct_off_all_PPA = total_PPA_all / off_cumulative_total,
      ply_pct_off_pass_PPA = total_PPA_pass / off_cumulative_passing,
      ply_pct_off_rush_PPA = total_PPA_rush / off_cumulative_rushing
    ) %>%
    select(-position)
  
  # Slim Roster For Position
  Slim_Roster <- rosters %>% select(athlete_id, position)
  
  Large_DF <- Large_DF %>%
    left_join(adv_stats, by = c('team', 'conference', 'athlete_id')) %>%
    left_join(Slim_Roster, by = 'athlete_id') %>%
    select(
      team, conference, athlete_id, player, position,
      passing_completions:ply_pct_off_rush_PPA
    )
  
  print(Large_DF %>% arrange(-passing_completions))
  Large_DF %>% glimpse()
}
Stats_Full <- get_stats()

# Player Optouts
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
  
  less_raw <- player_df %>%
    mutate(
      position = case_when(
        name == 'Briceon Hayes' ~ 'DL',
        TRUE ~ position
      )
    ) %>%
    distinct()
    
  
  bowl_rosters <- bowl_rosters %>%
    filter(athlete_id %notin% c(4686607, 550577, 5160642, 4707184, 5156906, 5144962,
                                4923029, 5220921, 5160232, 4432647, 5220921, 4702800,
                                5121764, 5197034, 5081784, 5082918, 4869324, 5080140,
                                4597250)
           )%>%
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
        name == "Cameron Cook" ~ "Cam Cook",
        name == "Damon Payne" ~ "Damon Payne Jr.",
        name == "Jibreel Al-amin" ~ "Jibreel Al-Amin",
        name == "Rodney Newsom" ~ "Rodney Newsom Jr.",
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
  
  
  misjoined <- final_df %>%
    filter(name != "Jordan Brown") %>%
    group_by(name) %>%
    summarise(
      n = n(),
      position_x_list = list(position.x),
      position_y_list = list(position.y)
    ) %>%
    filter(n > 1)
  
  # Filter Names
  bad_names <- c("Aiden Gobaira", "Arik Gilbert", "Boyton Cheney", "Brady Boehm", 
                 "Brady McKelheer", "Cameron Cook", "Christian Burkhalter", "Corey Lucius", 
                 "Damon Payne", "Dominique Johnson", "Ethan Boyd", "Ezra McAllister", 
                 "Isaiah Cash", "Jibreel Al-amin", "Joshua Miller", "Kadin Bailey", 
                 "Kortlin Rausaw", "Koy Moore", "Lorenzo Vitti", "Marcell Baylor", 
                 "Marcus Williams", "Noah Bolticoff", "Rod Daniels", "Rodney Groce Jr.", 
                 "Rodney Newsom", "Ronald Daragjati", "S'Maje Burrell", "Tai Faavae", 
                 "Tyson Ford", "Velton Gardner")
  
  ## Print NA Players
  new_nas <- final_df %>%
    filter(position.x %notin% c('HC', 'DC', 'OC')) %>%
    filter(is.na(athlete_id)) %>%
    filter(name %notin% bad_names) %>%
    select(name, Reason, position.x) %>%
    arrange(name)
  
  if(nrow(misjoined) > 0){
    print("Misjoined Players - Remove From Roster Before Join")
    misjoined %>% print.data.frame()
  }
  
  if(nrow(new_nas) > 0){
    print("New NA Players - Figure out if Join Available")
    new_nas %>% print.data.frame()
  }
  
  # Save Coaches
  # Coach Opt Outs
  missing_coaches <<- final_df %>% filter(position.x %in% c('HC', 'DC', 'OC')) %>% select(name, position.x, Reason)
  
  return(final_df  %>%
           distinct() %>%
           filter(position.x %notin% c('HC', 'DC', 'OC')) %>%
           filter(!is.na(athlete_id)))
}
raw_optouts <- get_optouts()

# OL Stats
get_OL_stats <- function(){
  # OL Optouts
  OL_Optout <- raw_optouts %>% filter(position.y %in% c("OL", 'C', 'G')) %>% select(athlete_id, Reason)
  
  # Rosters
  OL_Roster <- bowl_rosters %>%
    filter(position %in% c('C', 'G', "OL", "OT")) %>%
    select(athlete_id, name, team, year)
  
  # Recruiting
  Croot <- recruiting %>%
    filter(position %in% c("OG", "OC", "OT")) %>%
    select(athlete_id, stars, rating)
  
  # Join
  OL <- OL_Roster %>%
    left_join(OL_Optout, by = 'athlete_id') %>%
    left_join(Croot, by = 'athlete_id') %>%
    mutate(
      OptOut = if_else(is.na(Reason), 0, 1),
      rating = if_else(is.na(rating), 0.6, rating),
      Value = ((year/4) + rating)/2
    )
  
  Team_OL <- OL %>%
    group_by(team) %>%
    summarise(
      n_OL = n(),
      n_OL_Portal = sum(OptOut, na.rm=TRUE),
      mean_Exp = sum(year, na.rm=TRUE) / n_OL,
      mean_Exp_Portal = sum(year * OptOut, na.rm=TRUE) / n_OL_Portal,
      mean_Value = sum(Value, na.rm = TRUE) /  n_OL,
      mean_Value_Portal = sum(Value * OptOut, na.rm = TRUE) /  n_OL_Portal,
      Percent_Portal = n_OL_Portal / n_OL,
      OL_Prod_Portal = sum(Value * OptOut, na.rm = TRUE) / sum(Value, na.rm = TRUE),
      OL_Prod_Portal = OL_Prod_Portal,
      OLine_Prod_Strength = 1 - (OL_Prod_Portal),
      .groups = "drop"
    ) %>%
    select(team, n_OL, n_OL_Portal, Percent_Portal, OLine_Prod_Strength) %>%
    arrange(OLine_Prod_Strength)
  
  
  return(Team_OL)
  
}
OL_Portal_Strength <- get_OL_stats()




get_portal_adjustments <- function(){
  
  # Join OptOuts To Stats
  Portal <- Stats_Full %>%
    left_join(raw_optouts %>% select(athlete_id, Reason), by = c('athlete_id')) %>%
  # Build Production Value for Position
    mutate(
      Passing_Prod = (passing_yds*0.04) + (passing_td*4) + (passing_int*-2),
      Rushing_Prod = (rushing_car*0.05) + (rushing_yds*0.1) + (rushing_td*6),
      Receiving_Prod = (receiving_rec*0.85) + (receiving_yds*0.1) + (receiving_td*6),
      Secondary_Prod = (interceptions_int*10) + (defensive_pd*2.5),
      Def_Line_Prod = defensive_tot + (defensive_tfl*2.5) + (defensive_sacks*5) + defensive_qb_hur,
      # Totals
      Defensive_Prod = Def_Line_Prod + Secondary_Prod,
      Offensive_Prod = Passing_Prod+Rushing_Prod+Receiving_Prod,
      Total_Prod = Defensive_Prod + Offensive_Prod
    )
  
  print(Portal %>% filter(!is.na(Reason)) %>% select(team, player, position, Total_Prod) %>% arrange(-Total_Prod))
  print(Portal %>% filter(!is.na(Reason)) %>% select(team, player, position, Defensive_Prod) %>% arrange(-Defensive_Prod))
  #print(Portal %>% filter(team == "Sam Houston") %>% select(team, player, position, ends_with("_Prod"))  %>% arrange(-Total_Prod))
  
  Team_Portal <- Portal %>%
    filter(team %in% bowl_schedule$home_team | team %in% bowl_schedule$away_team) %>%
    mutate(
      across(ends_with("_Prod"), ~ if_else(is.na(Reason), 0, .), .names="{.col}_Portal")
    ) %>%
    group_by(team) %>%
    summarise(
      Total_Portal = sum(if_else(is.na(Reason), 0, 1)),
      across(ends_with("_Prod"), ~ sum(., na.rm = TRUE)),
      across(ends_with("_Prod_Portal"), ~ sum(., na.rm = TRUE)),
      across(ends_with("_Prod"), ~ 1 - (sum(get(paste0(cur_column(), "_Portal")), na.rm = TRUE) / sum(., na.rm=TRUE)), .names = "{.col}_Strength"),
      .groups = "drop"
    ) %>%
    arrange(Total_Portal) %>%
    left_join(OL_Portal_Strength %>% select(team, OLine_Prod_Strength), by = c('team')) %>%
    left_join(bowl_schedule %>% select(home_team, notes, start_date, game_id) %>% rename(team = home_team, bowl = notes), by = 'team') %>%
    left_join(bowl_schedule %>% select(away_team, notes, start_date, game_id) %>% rename(team = away_team, bowl = notes), by = 'team') %>%
    mutate(
      bowl =  coalesce(bowl.y, bowl.x),
      date =  coalesce(start_date.x, start_date.y),
      game_id = coalesce(game_id.y, game_id.x)
    ) %>%
    select(-c('bowl.x', 'bowl.y', 'start_date.x', 'start_date.y', 'game_id.y', 'game_id.x')) %>%
    mutate(
      across(ends_with("Prod_Strength"), ~ if_else(. < 0, 0, .))
    )
  
  return(Team_Portal)
}
Bowl_Adjustments<- get_portal_adjustments()


pretty_portal_adj <- function(){
  df <- Bowl_Adjustments %>%
    mutate(
      bowl = case_when(
        str_detect(bowl, "College Football Playoff") ~ "Playoff - First Round Game",
        TRUE ~ bowl
      ),
      
      time = format(as.POSIXct(date), "%H:%M"),
      date = format(date, "%Y-%m-%d")
    ) %>%
    arrange(date, bowl, time) %>%
    select(date, time, bowl, team,
           Passing_Prod_Strength:Receiving_Prod_Strength, OLine_Prod_Strength, Offensive_Prod_Strength,
           Secondary_Prod_Strength:Defensive_Prod_Strength, Total_Prod_Strength)
    
  
  gt_obj <- df %>%
    gt() %>%
    data_color(
      columns = c(Defensive_Prod_Strength, Offensive_Prod_Strength, Total_Prod_Strength),
      colors = scales::col_numeric(palette = c("#FF0000", "#FFFFFF"),
                                   domain = c(0.3, 1))
    ) %>%
    data_color(
      columns = c(Passing_Prod_Strength),
      colors = scales::col_numeric(palette = c("#FF0000", "#FFFFFF"),
                                   domain = c(0, 1.01))
    ) %>%
    fmt_percent(columns=Passing_Prod_Strength:Total_Prod_Strength, decimals = 1) %>%
    gt_theme_538() %>%
    cols_label(
      Passing_Prod_Strength	 = "PASS",
      Rushing_Prod_Strength = "RUSH",
      Receiving_Prod_Strength = "REC",
      OLine_Prod_Strength = "OLINE",
      Secondary_Prod_Strength = "SCDRY",
      Def_Line_Prod_Strength = "DLINE",
      Defensive_Prod_Strength = "TOTAL DEF",
      Offensive_Prod_Strength = "TOTAL OFF",
      Total_Prod_Strength = "TOTAL"
    )
  
  return(gt_obj)
}
pretty_portal_adj()



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
build_depth_adjustments(tm = "Memphis")

test <- cfbd_stats_season_player(year = 2024)

unique(test$category)


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
