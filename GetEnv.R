# GetEnv

# Packages
library(tidyverse)
library(pacman)
library(cfbfastR)
library(tictoc)
library(rvest)
library(aws.s3)
library(zoo)
# Display
library(gt)
library(gtExtras)
library(ggrepel)
library(ggimage)

# Helper Functions
`%notin%` <- Negate(`%in%`)
dec_to_amer <- function(odds){
  if_else(odds >= 2, (odds - 1)*100, (-100)/(odds - 1))
}
ml_to_impprob <- function(odds) {
  if_else(odds > 0, 100 / (odds + 100), abs(odds) / (abs(odds) + 100))
}
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
      name = paste0(first_name, " ", last_name),
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


## Recruiting
tictoc::tic()
recruiting <- data.frame()
progressr::with_progress({
  recruiting <- pmap_dfr(list(year = c(2017:2023)), cfbd_recruiting_player)
  print(paste0("Loaded Recruiting | Number of Rows: ",nrow(recruiting), " | Number of Columns: ", ncol(recruiting)))
  gc()
})
print("Recruiting Load Time:")
tictoc::toc()