library(nflreadr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
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
nfl_pbp_full <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 

# initial subset and clean
nfl_pbp <- nfl_pbp |> 
  filter(penalty_yards > 0 & play_type %in% c('run','pass') & length(penalty_player_id) >= 2) |>
  select(play_id, game_id, defteam, game_date, home_team, play_type, game_half, qtr,
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
playerstats <- load_player_stats

#join data on gsis_id - an established key specific to each NFL player 
nfl_pbp <- left_join(nfl_pbp, players, by = "gsis_id")
nfl_pbp <- left_join(nfl_pbp, teams, by = "team_abbr")

#Add: 
# player age at at gametime
# year(game_date)
# player years of experience
nfl_pbp <- nfl_pbp |> 
  mutate(
    age = as.integer((as.Date(game_date) - as.Date(birth_date))/365),
    game_year = year(as.Date(game_date)),
    years_experience = game_year - rookie_season,
    side_of_ball = if_else(defteam == team_abbr,"Offense","Defense"),
    penaltyid = paste0(nfl_pbp$play_id,'-',nfl_pbp$game_id)
  ) |>
  filter(!(position_group %in% c('SPEC', 'NA')))

nfl_pbp_full <- nfl_pbp_full |> mutate(
  game_year = year(as.Date(game_date))
)


#Create Penalties Per Game Measure
#full dataset needed
# 
# getPenaltiesPerGame <- function(){
#   
#   count_unique_games_all <- length(unique(nfl_pbp_full$game_id))
#   penalty_count <- length(unique(nfl_pbp$penaltyid))
#   
#   penalties_per_game <- penalty_count/count_unique_games_all
#   
#   return(penalties_per_game)
#   
# }
# 
# 
# penalties_per_game_tbl <- nfl_pbp_full |>
#   group_by(game_year, gsis_id) |>
#   summarize(games = length(unique(nfl_pbp_full$game_id)))
# 










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

#contingency table 3 - penalties by year and conference 
nfl_pbp |>
  drop_na(game_year, team_conf) |>
  group_by(game_year, team_conf) |>
  summarize(count = n()) |>
  pivot_wider(names_from = team_conf, values_from = count) 

#reorganized with summarize for plotting penalties by year and conference
pen_year_conf <- nfl_pbp |>
  group_by(game_year, team_conf) |>
  summarize(Penalties_Taken = sum(penalty_yards, na.rm = TRUE))

pen_year_conf


#Measures of Spread for age by penalty type
fxlist <- list("mean" = mean, "median" = median, "min" = min, "max" = max, "sd" = sd)

five_summary_type <- nfl_pbp |> 
 group_by(penalty_type)|> 
 summarize(across(years_experience,.fns = fxlist,.names = "{.col}_{.fn}", na.rm = TRUE)) |>
 arrange(years_experience_mean)

print(five_summary_type, n = 50)


#Line graph 1 - Penalties taken by conference and year
# NFL Conference Colors https://www.trucolor.net/portfolio/national-football-league-official-colors-additional-records-1920-through-present/

gline1 <- ggplot(pen_year_conf, aes(x = game_year, y = Penalties_Taken, color= team_conf))
gline1 + geom_line(size = 2.5) +
  labs(title = "Penalties Taken by Conference", x = "Year", y = "Penalties Taken") +
  scale_color_manual(values = c("#C8102E","#003A70")) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal")

#bar graph one positional group penalties by conference
gbar1 <- ggplot(nfl_pbp, aes(x = position_group, fill = team_conf))
gbar1 + geom_bar(stat = "count", position = "dodge") +
  labs(title = "Penalties Taken by Positional Group and Conference", x = "Positional Group", y = "Penalties Taken") +
  scale_fill_manual(values = c("#C8102E","#003A70")) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal")



#positional box and whisker - experience distribution by 
#Fill colors taken from teams dataset - manually ordered to be distinct 
gbox1 <- ggplot(nfl_pbp, aes(x = position_group,fill = position_group, y = years_experience))
gbox1 + geom_violin() + 
  scale_fill_manual(values = c(teams$team_color3[1:3], teams$team_color[2:32])) +
  labs(title = "Distribution of Experience (Years) Among Penalty Takers by Position Group", x = "Positional Group", y = "Years Experience") +
  theme_minimal()+
  theme(legend.position = "top", legend.direction = "horizontal")
