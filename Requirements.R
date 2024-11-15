# Packages
library(tidyverse)
library(pacman)
library(cfbfastR)
library(tictoc)
library(rvest)
library(ggrepel)
library(ggimage)
library(aws.s3)
library(zoo)

# Helper Functions
`%notin%` <- Negate(`%in%`)

# Load Functions
build_teams <- function(start_year = 2000, fbs_teams = TRUE){
  year_load <- seq(start_year, cfbfastR:::most_recent_cfb_season())
  
  cfbd_team_info <- c()
  df_list = list()
  
  for(i in length(year_load)){
    df <- cfbd_team_info(year = year_load[i], only_fbs = fbs_teams) %>%
      mutate(season = year_load[i])
    df_list[[i]] <- df
  }
  
  return(bind_rows(df_list))
}

# AWS Connection
bkt <- "cfb-sandbox"
write_rds_comp <- function(x, file){
  write_rds(x, file, compress = "gz")
}
