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




##############################################
# SHINY APP
##############################################

##               UI
ui <- fluidPage(
  titlePanel("Evan Gray - Project 2"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons(
        "conf",
        "Conference",
        choiceNames = list("Both","AFC","NFC"),
        choiceValues = list("both", "AFC", "NFC"),
        selected = "both"
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
    
      mainPanel(
        tableOutput("table1"),
        plotOutput("plot1"),
        plotOutput("plot2")
        )
    )
)

##             SERVER
server <- function(input,output,session){
  
#recalculate when subset button pressed
  observeEvent(
    input$subset, {
    
      
      if(input$conf == "both"){
        conf_sub <- c('NFC','AFC')
      } else {
        conf_sub <- input$conf
      }      
      
      
      
######################################
#Subset original data
######################################  
    
  nfl_pbp <- nfl_pbp |> 
    mutate(
      age = as.integer((as.Date(game_date) - as.Date(birth_date))/365),
      game_year = year(as.Date(game_date)),
      years_experience = game_year - rookie_season,
      side_of_ball = if_else(defteam == team_abbr,"Offense","Defense"),
      penaltyid = paste0(nfl_pbp$play_id,'-',nfl_pbp$game_id)
    ) |>
    filter(
      position_group %in% c("DB","DL","LB","OL","QB","RB","TE","WR") &
      between(game_year, input$year[1], input$year[2]) & team_conf %in% conf_sub # compare game year to first and second elements of input$year range
      )
  
  pen_year_conf <- nfl_pbp |>
    group_by(game_year, team_conf) |>
    summarize(Penalties_Taken = sum(penalty_yards, na.rm = TRUE))
  
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
  
  
  
  
  
  
  
  
  
######################################
#Plots
######################################
  
  output$table1 <- renderTable({
    
    fxlist <- list("mean" = mean, "median" = median, "min" = min, "max" = max, "sd" = sd)
    
    five_summary_type <- nfl_pbp |> 
      group_by(penalty_type)|> 
      summarize(across(years_experience,.fns = fxlist,.names = "{.col}_{.fn}", na.rm = TRUE),
                count = n() #include count
      ) |>
      arrange(years_experience_mean)
    
    print(five_summary_type, n = 50)
    
  })
  
  output$plot1 <- renderPlot({
    
    #Line graph 1 - Penalties taken by conference and year
    # NFL Conference Colors https://www.trucolor.net/portfolio/national-football-league-official-colors-additional-records-1920-through-present/
    gline1 <- ggplot(pen_year_conf, aes(x = game_year, y = Penalties_Taken, color= team_conf))
    gline1 + geom_line(size = 2.5) +
      labs(title = "Penalties Taken by Conference", x = "Year", y = "Penalties Taken") +
      scale_color_manual(values = c("#C8102E","#003A70")) +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")
  })
  
  output$plot2 <- renderPlot({
    
    #bar graph one positional group penalties by conference
    gbar1 <- ggplot(nfl_pbp, aes(x = position_group, fill = team_conf))
    gbar1 + geom_bar(stat = "count", position = "dodge") +
      labs(title = "Penalties Taken by Positional Group and Conference", x = "Position Group", y = "Penalties Taken") +
      scale_fill_manual(values = c("#C8102E","#003A70")) +
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "top", legend.direction = "horizontal")
  })
  
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)