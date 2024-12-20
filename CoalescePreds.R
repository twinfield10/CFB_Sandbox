# Get All Prediction Sites
## NORMALIZE
clean_teams <- function(df){
  df <- df %>%
    mutate(
      across(c(Home, Away, Proj_Winner, ATS_Pick),
             ~case_when(
              . == 'UL-Monroe' ~ 'Louisiana-Monroe',
              . == 'USF' ~ 'South Florida',
              . == 'Florida International' ~ 'FIU',
              . == 'Sam Houston' ~ 'Sam Houston State',
              . == 'Miami-OH' ~ 'Miami OH',
              . == 'UL-Lafayette' ~ 'Louisiana',
              . == 'Massachusetts' ~ 'UMASS',
              . == 'Southern Miss' ~ 'Southern Mississippi',
              . == 'UCONN' ~ 'Connecticut',
              . %in% c('Miami FL', 'Miami-FL') ~ 'Miami Florida',
              TRUE ~ .
             )
            )
    )
  
  return(df)
}
flip_H_A <- function(df, true_home, true_away){
  
  
  base <- df %>%
    filter(Home != true_away & Away != true_home)
  
  fix_df <- df %>%
    filter(Home == true_away & Away == true_home)
  
  true_away <- fix_df %>%
    select(starts_with("Home")) %>%
    rename_with(~ gsub("Home", "Away", .), everything())
  
  true_home <- fix_df %>%
    select(starts_with("Away")) %>%
    rename_with(~ gsub("Away", "Home", .), everything())
  
  true_rest <- fix_df %>%
    select(-starts_with("Home"), -starts_with("Away"))
  
  final_df <- rbind(
    cbind(true_away, true_home, true_rest) %>% select(names(base)),
    base
    )
  
  return(final_df)
}
## GET
# CFBInsidersScrape
get_cfbi <- function(url='https://www.collegefootballinsiders.com/'){
  games <- read_html(url) %>%
    html_node('body') %>%
    html_node('div.content') %>%
    html_nodes(".game-box")
  
  games_data <- data.frame()
  
  for (game in games) {
    # Extract location
    location <- game %>% html_node("div[style*='text-align: center']") %>% html_text(trim = TRUE)
    
    # Extract game date
    game_date <- game %>% html_node("h2") %>% html_text(trim = TRUE)
    
    # Extract away team information
    away_team <- game %>% html_nodes(".odds-row .team-column .team-name") %>% .[1] %>% html_text(trim = TRUE)
    away_stats <- game %>% html_nodes(".odds-row .pitcher-name a") %>% .[1] %>% html_text(trim = TRUE)
    away_odds <- game %>% html_nodes(".odds-row span") %>% .[3] %>% html_text(trim = TRUE)
    
    # Extract home team information
    home_team <- game %>% html_nodes(".odds-row .team-column .team-name") %>% .[2] %>% html_text(trim = TRUE)
    home_stats <- game %>% html_nodes(".odds-row .pitcher-name a") %>% .[2] %>% html_text(trim = TRUE)
    home_odds <- game %>% html_nodes(".odds-row span") %>% .[6] %>% html_text(trim = TRUE)
    
    # Extract projected information
    proj_winner <- game %>% html_node(".proj-info-container .proj-info-column .team-logo") %>% html_attr("alt")
    proj_margin <- game %>% html_nodes(".proj-info-column .team-name") %>% .[1] %>% html_text(trim = TRUE)
    proj_total <- game %>% html_nodes(".proj-info-column .team-name") %>% .[2] %>% html_text(trim = TRUE)
    
    # Create a dataframe for the current game and bind it to the main dataframe
    game_data <- data.frame(
      Location = location,
      Date = game_date,
      Away = away_team,
      Away_Stats = away_stats,
      Away_Odds = away_odds,
      Home = home_team,
      Home_Stats = home_stats,
      Home_Odds = home_odds,
      Spread = ifelse(away_odds < 0, away_odds, home_odds),
      Total = ifelse(away_odds >= 0, away_odds, home_odds),
      Proj_Winner = proj_winner,
      Proj_Margin = proj_margin,
      Proj_Total = proj_total,
      stringsAsFactors = FALSE
    )
    
    games_data <- bind_rows(games_data, game_data)
  }
  
  games_data <- games_data %>%
    mutate(
      # Date/Time
      Date = mdy_hm(Date),
      Date = format(Date, "%Y-%m-%d"),
      Time = sub(".*, ", "", games_data$Date),
      # Records
      across(c(Away, Home),~ str_replace_all(str_extract(.x, "\\(\\d+-\\d+\\)"), "[()]", ""), .names = "{.col}_Record"),
      across(c(Away, Home),~ str_remove(.x, "\\s*\\(\\d+-\\d+\\)")),
      # Stats
      Away_Conf = str_split(Away_Stats, " \\| ", simplify = TRUE)[, 1],
      Away_SOS = str_extract(str_split(Away_Stats, " \\| ", simplify = TRUE)[, 2], "\\d+"),
      Away_IPR = str_extract(str_split(Away_Stats, " \\| ", simplify = TRUE)[, 3], "\\d+(\\.\\d+)?"),
      Home_Conf = str_split(Home_Stats, " \\| ", simplify = TRUE)[, 1],
      Home_SOS = str_extract(str_split(Home_Stats, " \\| ", simplify = TRUE)[, 2], "\\d+"),
      Home_IPR = str_extract(str_split(Home_Stats, " \\| ", simplify = TRUE)[, 3], "\\d+(\\.\\d+)?"),
      across(c(Away_SOS, Away_IPR, Home_SOS, Home_IPR, Home_Odds, Away_Odds, Proj_Total, Proj_Margin, Total, Spread), ~ as.numeric(.)),
      # Basic Clean
      Proj_Winner = str_remove(Proj_Winner, " Logo"),
      # Label
      Game_Lab = paste0(Away, " at ", Home),
      # Differences
      Proj_Margin = abs(Proj_Margin),
      Home_Spread = if_else(Home_Odds <= 0, Home_Odds, abs(Away_Odds)),
      Away_Spread = if_else(Home_Odds > 0, Away_Odds, abs(Home_Odds)),
      Diff_Total = Proj_Total - Total,
      Diff_Spread = if_else(Home == Proj_Winner, abs(Proj_Margin) + Home_Spread, abs(Proj_Margin) + Away_Spread),
      # Picks
      ATS_Pick = if_else(Diff_Spread <= 0 & Proj_Winner == Home, Away,
                         if_else(Diff_Spread <= 0 & Proj_Winner == Away, Home,
                                 if_else(Diff_Spread > 0 & Proj_Winner == Home, Home,
                                         if_else(Diff_Spread >0 & Proj_Winner == Away, Away, NA)))),
      Total_Pick = if_else(Diff_Total < 0, 'Under', 'Over')
    ) %>%
    select(
      Date, Time,
      Away, Away_Record, Away_Conf, Away_SOS, Away_IPR,
      Home, Home_Record, Home_Conf, Home_SOS, Home_IPR,
      Home_Spread, Away_Spread, Total,
      Proj_Winner, Proj_Margin, Proj_Total,
      Diff_Total, Diff_Spread,
      ATS_Pick, Total_Pick
    )
  
  # Format Teams
  games_data <- clean_teams(games_data)
  
  return(games_data)
    
  
}
# Connelly
get_bc <- function(){
  path <- paste0("BC_All.csv")
  df <- read.csv(path)
  names(df) <- c('Week', 'Date', 'Time', 'Game_Lab', 'Proj_Winner', 'Proj_Margin', 'Win_Prob', "Proj_Score", "Raw_Spread",
                 "ATS_Pick", "Diff_Spread", "Total", "Total_Pick", "Diff_Total")
  
  df <- df %>%
    filter(!is.na(Total) & !is.na(Raw_Spread)) %>%
    separate(Game_Lab, into = c("Away", "Home"), sep = " at | vs\\. ")
  
  df <- clean_teams(df)
  
  df <- df %>%
    separate(Raw_Spread, into = c("Favorite", "Fav_Spread"), sep = " -", remove = FALSE) %>%
    mutate(
      # DateTime
      Date = paste0(Date, "-", year(Sys.Date())),
      Date = dmy(Date),
      # Totals
      Proj_Total = Total + Diff_Total,
      # Spreads
      Fav_Spread = as.numeric(Fav_Spread),
      Home_Spread = if_else(Favorite == Home, Fav_Spread*-1, Fav_Spread),
      Away_Spread = if_else(Favorite == Away, Fav_Spread*-1, Fav_Spread),
      Win_Prob = as.numeric(gsub("%", "", Win_Prob)) / 100
    ) %>%
    select(
      Week, Date, Time,
      Away, Away_Spread,
      Home, Home_Spread,
      Total,
      Proj_Winner, Win_Prob, Proj_Margin, Proj_Total,
      Diff_Spread, Diff_Total,
      ATS_Pick, Total_Pick
    )
  
  return(df)
  
}
# Pinny
## Get Current Odds
combine_pinny <- function(){
  pinny_df <- read.csv("./Data/Pinnacle/Pinnacle_Base_New.csv") %>%
    filter(IsPrimary == 1 & Period == 'Game') %>%
    select(officialDate, week, Home, Away, Period, BetType, BetSide, BetValue, Price, BetImpProb) %>%
    group_by(officialDate, week, Home, Away, Period, BetType) %>%
    filter(n() < 3 | (Price == max(Price, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(
      across(c(Home, Away, BetSide),
             ~ case_when(
              . == 'Miami Ohio' ~ 'Miami OH', 
              . == 'Utsa' ~ 'UTSA',
              . == 'Ucla' ~ 'UCLA',
              . == 'Massachusetts' ~ 'UMASS',
              . == 'Ul Monroe' ~ 'Louisiana-Monroe',
              . == 'Florida International' ~ 'FIU',
              . == 'Smu' ~ 'SMU',
              . == 'Lsu' ~ 'LSU',
              . == 'Usc' ~ 'USC',
              . == 'Ul Lafayette' ~ 'Louisiana',
              . == 'Southern Miss' ~ 'Southern Mississippi',
              . == 'Texas Am' ~ 'Texas A&M',
              . == 'Uab' ~ 'UAB',
              . == 'Byu' ~ 'BYU',
              . == 'Unlv' ~ 'UNLV',
              TRUE ~ .
             )
            )
    )
  
  spreads_df <- pinny_df  %>%
    filter(BetType == "Handicap") %>%
    mutate(
      SideLoc = if_else(BetSide == Home, "Home", "Away"),
      Spread = BetValue
    ) %>%
    select(Home, Away, SideLoc, Spread) %>%
    unique() %>%
    pivot_wider(
      names_from = SideLoc,
      values_from = Spread,
      names_glue = "{SideLoc}_Spread"
    ) %>%
    mutate(
      Away_Spread = coalesce(Away_Spread, abs(Home_Spread)),
      Home_Spread = coalesce(Home_Spread, abs(Away_Spread))
    )
  
  totals_df <- pinny_df  %>%
    filter(BetType == "Total") %>%
    select(Home, Away, BetValue) %>%
    unique() %>%
    rename(Total = BetValue)
  
  final <- spreads_df %>%
    full_join(totals_df, by=c('Home', 'Away'))
  
  return(final)
  
}
pivot_slim_pinny <- function(w){
  
  pinny_df <- read.csv("./Data/Pinnacle/Pinnacle_Base.csv") %>%
    select(officialDate, Home, Away, ends_with("Price"), ends_with("Value")) %>%
    pivot_longer(
      cols = -c(officialDate, Home, Away),  # Keep ID columns
      names_to = "Combined",
      values_to = "Value"
    ) %>%
    separate(Combined, into = c("BetSide", "BetType", "Metric"), sep = "_") %>%
    pivot_wider(
      names_from = Metric,
      values_from = Value
    ) %>%
    mutate(
      amerOdds = dec_to_amer(Price),
      impProb = ml_to_impprob(amerOdds),
      week = w,
      BetSide = if_else(BetSide == 'Home', Home, if_else(BetSide == 'Away', Away, BetSide))
    ) %>%
    mutate(
      across(c(Home, Away, BetSide),
             ~ case_when(
               . == 'Miami Ohio' ~ 'Miami OH', 
               . == 'Utsa' ~ 'UTSA',
               . == 'Ucla' ~ 'UCLA',
               . == 'Massachusetts' ~ 'UMASS',
               . == 'Ul Monroe' ~ 'Louisiana-Monroe',
               . == 'Florida International' ~ 'FIU',
               . == 'Smu' ~ 'SMU',
               . == 'Lsu' ~ 'LSU',
               . == 'Usc' ~ 'USC',
               . == 'Ul Lafayette' ~ 'Louisiana',
               . == 'Southern Miss' ~ 'Southern Mississippi',
               . == 'Texas Am' ~ 'Texas A&M',
               . == 'Uab' ~ 'UAB',
               . == 'Byu' ~ 'BYU',
               . == 'Unlv' ~ 'UNLV',
               TRUE ~ .
             )
      )
    )
}
combine_slim_pinny <- function(){
  pinny_df <- read.csv("./Data/Pinnacle/Pinnacle_Base.csv") %>%
    select(officialDate, Home, Away, ends_with("Price"), ends_with("Value")) %>%
    mutate(
      across(ends_with('Price'), ~ dec_to_amer(.),.names = "{sub('_Price$', '_Odds', .col)}"),
      across(ends_with('Odds'), ~ ml_to_impprob(.),.names = "{sub('_Odds$', '_Imp', .col)}"),
      across(ends_with('Odds'), ~ round(., 0)),
      across(c(Home, Away),
             ~ case_when(
               . == 'Miami Ohio' ~ 'Miami OH', 
               . == 'Utsa' ~ 'UTSA',
               . == 'Ucla' ~ 'UCLA',
               . == 'Massachusetts' ~ 'UMASS',
               . == 'Ul Monroe' ~ 'Louisiana-Monroe',
               . == 'Florida International' ~ 'FIU',
               . == 'Smu' ~ 'SMU',
               . == 'Lsu' ~ 'LSU',
               . == 'Usc' ~ 'USC',
               . == 'Ul Lafayette' ~ 'Louisiana',
               . == 'Southern Miss' ~ 'Southern Mississippi',
               . == 'Texas Am' ~ 'Texas A&M',
               . == 'Uab' ~ 'UAB',
               . == 'Byu' ~ 'BYU',
               . == 'Unlv' ~ 'UNLV',
               TRUE ~ .
             )
      ),
      Total = (Over_Total_Value + Under_Total_Value) / 2
    ) %>%
    rename(
      Home_Spread = Home_Spread_Value,
      Away_Spread = Away_Spread_Value
    )
  
  return(pinny_df)
  
}

## COmbine Predictions
combine_preds <- function(bc_data = bc_df,
                          cfbi_data = cfbi_df,
                          odds_data = odds,
                          show=20){
  
  # Join Everything
  df <- bc_df %>%
    full_join(cfbi_df, by=c('Home', 'Away')) %>%
    left_join(odds, by=c('Home', 'Away'))
  
  
  # Error Check
  check_tms <- df %>% filter(is.na(Total.y) | is.na(Total.x)) %>% arrange(Home) %>% select(Home, Away)
  if(nrow(check_tms) > 0){
    print("Null Join Values in DF: ")
    print(check_tms)
  }
  # End Error Check
  
  # Combine Prediction Sources
  df <- df %>%
    mutate(
      # Single Source of Truth Columns
      Date = Date.x,
      Time = Time.x,
      # Open From Connelly
      Open_Home_Spread = Home_Spread.y,
      Open_Away_Spread = Away_Spread.y,
      # Coalesce Spreads if Not Live
      Home_Spread = coalesce(Home_Spread, Open_Home_Spread),
      Away_Spread = coalesce(Away_Spread, Open_Away_Spread),
      Total = coalesce(Total, Total.y)
    ) %>%
    select(Week, Date, Time,
           Away, Open_Away_Spread, Away_Spread, Away_Moneyline_Odds, Away_Moneyline_Imp,
           Home, Open_Home_Spread, Home_Spread, Home_Moneyline_Odds, Home_Moneyline_Imp,
           Proj_Winner.x, Proj_Margin.x, Win_Prob, Proj_Winner.y, Proj_Margin.y,
           ATS_Pick.x, Diff_Spread.x, ATS_Pick.y, Diff_Spread.y,
           Total, Proj_Total.x, Diff_Total.x, Total_Pick.x, Proj_Total.y, Diff_Total.y, Total_Pick.y
    ) %>%
    mutate(
      ## Money Lines
      Proj_Winner = if_else(Proj_Winner.x == Proj_Winner.y, Proj_Winner.y,
                            if_else(abs(Proj_Margin.x) > abs(Proj_Margin.y), Proj_Winner.x,
                                    if_else(abs(Proj_Margin.y) > abs(Proj_Margin.x), Proj_Winner.y, NA))),
      Proj_Margin = if_else(Proj_Winner.x == Proj_Winner.y, (Proj_Margin.x + Proj_Margin.y)/2,
                            if_else(Proj_Winner.x == Proj_Winner, (Proj_Margin.x + (Proj_Margin.y*-1))/2,
                                    if_else(Proj_Winner.y == Proj_Winner, (Proj_Margin.y + (Proj_Margin.x*-1))/2, NA))),
      ## Spreads
      Home_Spread_Diff = if_else(Home_Spread <= 0, Proj_Margin + Home_Spread, -1*Proj_Margin + Home_Spread),
      Away_Spread_Diff = if_else(Away_Spread <= 0, Proj_Margin + Away_Spread, -1*Proj_Margin + Away_Spread),
      ATS_Pick = if_else(Home_Spread_Diff > Away_Spread_Diff, Home, Away),
      Moneyline_Bet_BC = if_else(Proj_Winner.x == Home,
                                 paste0(Home, " (", Home_Moneyline_Odds,") Edge = ", round((Win_Prob - Home_Moneyline_Imp)*100, 1)),
                                 paste0(Away, " (", Away_Moneyline_Odds,") Edge = ", round((Win_Prob - Away_Moneyline_Imp)*100, 1))),
      Spread_Diff = if_else(Home_Spread_Diff > Away_Spread_Diff, Home_Spread_Diff, Away_Spread_Diff),
      Spread_Diff_BC = abs(Diff_Spread.x),
      Spread_Diff_PF = abs(Diff_Spread.y),
      
      ## Totals
      Proj_Total = (Proj_Total.x + Proj_Total.y)/2,
      Total_Pick = if_else(Proj_Total > Total, "Over", "Under"),
      Total_Diff = abs(Proj_Total-Total),
      Total_Diff_BC = abs(Proj_Total.x - Total),
      Total_Diff_PF = abs(Proj_Total.y - Total)
    )
  
  
  print(paste0("Top ",show," Best Spread Bets"))
  print(df  %>% select(Week, Date, Time, Home, Home_Spread, Away, Away_Spread, Proj_Winner, Proj_Margin, ATS_Pick, Spread_Diff, Spread_Diff_BC, Spread_Diff_PF)%>% arrange(desc(Spread_Diff)) %>% head(show))
  
  print(paste0("Top ", show, " Best Total Bets"))
  print(df  %>% select(Week, Date, Time, Home, Away, Total, Proj_Total, Total_Pick, Total_Diff:Total_Diff_PF)%>% arrange(desc(Total_Diff)) %>% head(show))
  
  final <- df %>%
    select(
      Week, Date, Time, Home, Home_Spread, Home_Moneyline_Odds, Away, Away_Spread, Away_Moneyline_Odds,
      Proj_Winner, Proj_Margin, ATS_Pick, Moneyline_Bet_BC, Spread_Diff, Spread_Diff_BC, Spread_Diff_PF,
      Total, Proj_Total, Total_Pick, Total_Diff:Total_Diff_PF
    )
  
  return(final)
}



## EXE
cfbi_df <- get_cfbi()
bc_df_all <- get_bc()
bc_df <- bc_df_all %>% filter(Week == 17)
#odds <- bc_df %>% select(Home, Away, Away_Spread, Home_Spread, Total)
#odds <- combine_pinny()
odds <- combine_slim_pinny()

# Flip Home And Away if Necessary (Match Pinny)

cfbi_df <- reduce(
  seq_along(odds$Home),
  ~ flip_H_A(.x, odds$Home[.y], odds$Away[.y]),
  .init = cfbi_df
)

bc_df <- reduce(
  seq_along(odds$Home),
  ~ flip_H_A(.x, odds$Home[.y], odds$Away[.y]),
  .init = bc_df
)

# Join Current Week
Week17Preds <- combine_preds()
write_csv(Week17Preds, "Week17_AllPredictions.csv")
write_csv(cfbi_df, "CFBI_Week17.csv")

# Display Odds
## Spreads
Week17Preds %>%
  select(Date, Time, Home, Home_Spread, Away, Away_Spread,
         Proj_Winner, Proj_Margin, ATS_Pick, Spread_Diff,
         Spread_Diff_BC, Spread_Diff_PF) %>%
  arrange(desc(Spread_Diff)) %>%
  head(20) %>%
  gt() %>%
  gt_theme_538()

## Totals
Week17Preds %>%
  select(Date, Time, Home, Away,
         Total, Proj_Total, Total_Pick,
         Total_Diff:Total_Diff_PF) %>%
  arrange(desc(Total_Diff)) %>%
  head(20)
  


### Get Categories ###

# Home Dogs w/ Spread_Diff > 3
Week16Preds %>%
  filter(ATS_Pick == Home & Home_Spread > 0 & Spread_Diff > 2)

# Over Picks With a BC Difference > 5 and CFBI Difference < 4
Week14Preds %>%
  filter(Total_Pick == 'Over' & Total_Diff_BC > 4 & Total_Diff_PF <= 4)


