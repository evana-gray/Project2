library(nflreadr)
library(tidyverse)


# original file is listed in gitignore so it won't overwhelm github pushes
nfl_pbp <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 
#nfl_pbp_full <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 

# initial subset and clean
nfl_pbp_less <- nfl_pbp |> 
  filter(penalty_yards > 0 & play_type %in% c('run','pass','no_play') & length(penalty_player_id) >= 2) |>
  select(play_id, game_id, defteam, game_date, home_team, play_type, game_half, qtr,
         penalty_team, penalty_player_id, penalty_yards, penalty_type) 




write_csv(nfl_pbp_less,"NFL_PlayByPlay_Split.csv")

