library(nflreadr)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinyalert)


############################
# Read in 2009 - 2018 data 
############################

# first, read NFL play by play csv (have to manage file size before actually reading in)
# then, use nflreadr::load_players() and load_teams() to gather all player and info (position, date of birth, etc) to use for left join
# https://nflreadr.nflverse.com/reference/load_players.html

# original file is listed in gitignore so it won't overwhelm github pushes
nfl_pbp <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 

# initial subset and clean
nfl_pbp <- nfl_pbp |> 
  filter(penalty_yards > 0 & play_type %in% c('run','pass') & length(penalty_player_id) > 1) |>
  select(play_id, game_id, game_date, home_team, play_type, game_half, qtr,
         penalty_team, penalty_player_id, penalty_yards, penalty_type) |>
  rename('gsis_id' = penalty_player_id,
         'team_abbr' = penalty_team) |>
  mutate(penalty_type = str_replace_na(penalty_type,'Other'))

#vectorized/anonymous function w/ switch() to rename team_abbr of teams that have relocated
 nfl_pbp$team_abbr <- sapply(nfl_pbp$team_abbr, 
                             FUN = function(x){
                               switch(x,
                                       'JAC' = 'JAX',
                                       'STL' = 'LA',
                                       'SD' = 'LAC',
                                       'OAK' = 'LV',
                                        x)}
                             )
 ##Check rename
 # unique(nfl_pbp$team_abbr)
 # unique(nfl_pbp$team_division)
 
 #load player and team specific information
players <- load_players()
teams <- load_teams()

#join data on gsis_id - an established key specific to each NFL player 

nfl_pbp <- left_join(nfl_pbp, players, by = "gsis_id")
nfl_pbp <- left_join(nfl_pbp, teams, by = "team_abbr")

#Add player age at at gametime and year(game_date)
nfl_pbp$age <- as.integer((as.Date(nfl_pbp$game_date) - as.Date(nfl_pbp$birth_date))/365)
nfl_pbp$game_year <- year(as.Date(nfl_pbp$game_date))
