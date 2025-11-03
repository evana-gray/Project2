library(nflreadr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(bslib)
library(shinyalert)
library(scales)


############################################
# Read in 2009 - 2018 NFL play by play data 
############################################

# first, read NFL play by play csv (have to manage file size before actually reading in)
# then, use nflreadr::load_players() and load_teams() to gather all player and info (position, date of birth, etc) to use for left join
# https://nflreadr.nflverse.com/reference/load_players.html

# original file is listed in gitignore so it won't overwhelm github pushes
nfl_pbp <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 
#nfl_pbp_full <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 

# initial subset and clean
nfl_pbp <- nfl_pbp |> 
  filter(penalty_yards > 0 & play_type %in% c('run','pass','no_play') & length(penalty_player_id) >= 2) |>
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
  filter(position_group %in% c("DB","DL","LB","OL","QB","RB","TE","WR"))

# nfl_pbp_full <- nfl_pbp_full |> mutate(
#   game_year = year(as.Date(game_date))
# )

gline_data <- nfl_pbp |>
  drop_na(position_group, game_year) |>
  group_by(position_group, game_year) |>
  summarize(count = n()) |>
  arrange(position_group, game_year)

gbar2_data <- nfl_pbp |>
  drop_na(position_group, penalty_type) |>
  group_by(position_group, penalty_type) |>
  summarize(count = n()) |>
  arrange(position_group,desc(count)) |>
  slice(1:3)



##############################################
# SHINY APP
##############################################
ui <- fluidPage(
  titlePanel("Evan Gray - Project 2"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons(
        "conf",
        "Conference",
        choiceNames = list("All","AFC","NFC"),
        choiceValues = list(c("AFC","NFC"),"AFC","NFC"),
        selected = "All"
        ),
      sliderInput(
        "year",
        "Select Year Range",
        min = 2009,
        max = 2018,
        value = c(2009,2018),
        step = 1,
        sep = ""
      ),
      actionButton("subset","Subset and Run")
      ),
      mainPanel()
    )
)

server <- function(input,output,session){
  
}

# Run the application 
shinyApp(ui = ui, server = server)