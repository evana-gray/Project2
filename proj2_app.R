library(nflreadr)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinyalert)


############################################
# Read in 2009 - 2018 NFL play by play data 
############################################

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
  rename("gsis_id" = penalty_player_id,
         "team_abbr" = penalty_team) |>
  mutate(penalty_type = str_replace_na(penalty_type,"Other"))

#vectorized/anonymous function w/ switch() to rename team_abbr of teams that have relocated
 nfl_pbp$team_abbr <- sapply(nfl_pbp$team_abbr, 
                             FUN = function(x){
                               switch(x,
                                       "JAC" = "JAX",
                                       "STL" = "LA",
                                       "SD" = "LAC",
                                       "OAK" = "LV",
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

#Add: 
# player age at at gametime
# year(game_date)
# player years of experience
nfl_pbp <- nfl_pbp |> mutate(
  age = as.integer((as.Date(game_date) - as.Date(birth_date))/365),
  game_year = year(as.Date(game_date)),
  years_experience = game_year - rookie_season
)

############################################
# Static Tables and Plots
############################################


#contingency table 1 - what positional groups commit the most penalties, what about penalty yards?
nfl_pbp |>
  drop_na(position_group) |>
  group_by(position_group) |>
  summarize(count = n()) |>
  arrange(desc(count))


#contingency table 2 - penalties committed by years of experience
nfl_pbp |>
  drop_na(years_experience) |>
  group_by(years_experience) |>
  summarize(count = n()) |>
  arrange(years_experience)

#contingency table 3 
nfl_pbp |>
  drop_na(game_year, team_conf) |>
  group_by(game_year, team_conf) |>
  summarize(count = n()) |>
  pivot_wider(names_from = team_conf, values_from = count) 


#Measures of Spread for age by penalty type
fxlist <- list("mean" = mean, "median" = median, "min" = min, "max" = max, "sd" = sd)

five_summary_type <- nfl_pbp |> 
                          group_by(penalty_type)|> 
                          summarize(across(years_experience,.fns = fxlist,.names = "{.col}_{.fn}", na.rm = TRUE)) |>
                          arrange(years_experience_mean)

print(five_summary_type, n = 40)

