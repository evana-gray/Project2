library(nflreadr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(bslib)
library(shinyalert)
library(scales)
library(DT)


############################################
# Read in 2009 - 2018 NFL play by play data 
############################################

# first, read NFL play by play csv (have to manage file size before actually reading in)
# then, use nflreadr::load_players() and load_teams() to gather all player and info (position, date of birth, etc) to use for left join
# https://nflreadr.nflverse.com/reference/load_players.html

# original file is listed in gitignore so it won't overwhelm github pushes
nfl_pbp <- read_csv("NFL_PlayByPlay_Split.csv") 
#nfl_pbp_full <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 

# initial subset and clean
nfl_pbp <- nfl_pbp |> 
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
##############################################
# SHINY APP
##############################################
##############################################

##               UI
ui <- fluidPage(
  titlePanel("Evan Gray - Project 2 - NFL Analysis of Penalties"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Subsetting Variables:"),  
      radioButtons(
        "conf",
        "Conference",
        choiceNames = list("Both","AFC","NFC"),
        choiceValues = list("both", "AFC", "NFC"),
        selected = "both"
        ),
      
      radioButtons(
        "side",
        "Side of Ball",
        choiceNames = list("All","Offense","Defense","Special Teams"),
        choiceValues = list("all","Offense","Defense","Special Teams"),
        selected = "all"
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
      
      sliderInput(
        "exp",
        "Player Years of Experience",
        min = 0,
        max = 25,
        value = c(0,25),
        step = 1,
        sep = ""
      ),
      actionButton("subset","Subset and Run"),
      
      h2("Create Your Own - Pick Axes:"),  
       selectizeInput("y_var",
                      "Select Numeric Variable (Calculation = Mean)",
                      choices = list("Height", "weight", "years_experience"), 
                      selected = "Height"
                      ),
      selectizeInput("x_var",
                     "Select Categorical Variable",
                     choices= list("position_group","team_abbr", "DraftRd","RookieSeason", "college_conference"), 
                     selected = "position_group"
                     ),
      ),
    
      mainPanel(
        tabsetPanel(
        tabPanel("Subsettable Plots",
          plotOutput("plot1"),
          plotOutput("plot2"),
          plotOutput("plot3"),
          plotOutput("plot4"),
          plotOutput("plot5"),
          plotOutput("plot6"),
          plotOutput("plot7")
        ),
        #layout_columns full screen for top table as it's widest
        #layout_column_wrap for bottom tables
        #shiny.posit.co for formatting help
        tabPanel("Subsettable Tables",
          layout_columns(
          card(
            full_screen = TRUE,
            card_header("Descriptive Statistics by Penalty Type"),
            DT::dataTableOutput("table1")
            )),
          layout_column_wrap(
          card(
            card_header("Penalties by Position Group"),
            DT::dataTableOutput("table2")
            ),
          card(
            card_header("Penalties by Years of Experience"),
            DT::dataTableOutput("table3")
            ),
          )
        ),
        tabPanel("Create Your Own Comparison",
                 layout_columns(
                   card(
                     full_screen = TRUE,
                     card_header("Custom Comparison Among NFL Players Taking Penalties"),
                     plotOutput("custom")
                   ))
                 )
        )
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
      
      if(input$side == "all"){
        side_sub <- c('Offense','Defense','Special Teams')
      } else {
        side_sub <- input$side
      }    
      
      
      
##################################################
# Create data subsets to populate plots and tables
##################################################
    
  nfl_pbp <- nfl_pbp |> 
    mutate(
      age = as.integer((as.Date(game_date) - as.Date(birth_date))/365),
      game_year = year(as.Date(game_date)),
      years_experience = game_year - rookie_season,
      side_of_ball = if_else(position_group %in% c("OL","QB","RB","TE","WR"),"Offense",
                             if_else(position_group %in% c("DB","DL","LB"),"Defense","Special Teams")),
      penaltyid = paste0(nfl_pbp$play_id,'-',nfl_pbp$game_id),
      DraftRd = as.character(draft_round),
      RookieSeason = as.character(rookie_season),
      Height = round(height/12,digits = 2)
      
    ) |>
    filter(
      position_group %in% c("DB","DL","LB","OL","QB","RB","TE","WR","SPEC") &
        # compare game year to first and second elements of input$year range
      between(game_year, input$year[1], input$year[2]) & 
      team_conf %in% conf_sub &
      side_of_ball %in% side_sub &
      between(years_experience, input$exp[1], input$exp[2])
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
  
  
  fxlist <- list("mean" = mean, "median" = median, "min" = min, "max" = max, "sd" = sd)
  
  five_summary_type <- nfl_pbp |> 
    group_by(penalty_type)|> 
    summarize(across(years_experience,.fns = fxlist,.names = "{.col}_{.fn}", na.rm = TRUE),
              count = n() #include count
    ) |>
    arrange(years_experience_mean)
  
  
  
  
  
  
######################################
#Plots
######################################
  
  
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
  
  output$plot3 <- renderPlot({
    
    #positional box and whisker - experience distribution by positional group
    #Fill colors taken from teams dataset - manually ordered to be distinct 
    gbox1 <- ggplot(nfl_pbp, aes(x = position_group,fill = position_group, y = years_experience))
    gbox1 + geom_violin() + 
      scale_fill_manual(values = c(teams$team_color3[1:3], teams$team_color[2:32])) +
      labs(title = "Distribution of Experience (Years) Among Penalized Players by Position Group", x = "Position Group", y = "Years Experience") +
      theme_minimal()+
      theme(legend.position = "none")

  })
  
  output$plot4 <- renderPlot({
    #Cleveland dot plot - total penalty count by penalty type
    gdot1 <- ggplot(five_summary_type, aes(x = count, y = reorder(penalty_type, count))) + 
      geom_point() 
    gdot1 + labs(title = "Total Penalties by Penalty Type", x = "Penalty Count", y = "Penalty") +
      theme_minimal()
  })
  
  output$plot5 <- renderPlot({
    #Cleveland dot plot - descending avg years of experience by penalty type
    gdot2 <- ggplot(five_summary_type, aes(x = years_experience_mean, y = reorder(penalty_type, years_experience_mean))) + 
      geom_point() 
    gdot2 + labs(title = "Avg Years of Experience by Penalty", x = "Avg Experience (Years)", y = "Penalty") +
      theme_minimal()
  })
  
  output$plot6 <- renderPlot({
    gl1 <- ggplot(gline_data, aes(x = game_year, y = count, color = position_group))
    gl1 + geom_line(size = 2.5) +
      scale_color_manual(values = c(teams$team_color3[1:3], teams$team_color[2:32])) +
      labs(title = "Penalties Taken by Year and Position", x = "Year", y = "Penalties") +
      facet_wrap(~ position_group, nrow = 2) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot7 <- renderPlot({
    gbar2 <- ggplot(gbar2_data, aes(x = penalty_type, y = count, fill = position_group))
    gbar2 + geom_bar(stat = "identity") +
      scale_fill_manual(values = c(teams$team_color3[1:3], teams$team_color[2:32])) +
      scale_x_discrete(labels = label_wrap(10)) +
      labs(title = "Top 3 Most Common Penalties by Position", x = "Penalty", y = "Penalties") +
      geom_text(aes(label=count), vjust=-0.2) +
      facet_wrap(~ position_group, nrow = 3, scales = "free_x") +
      ylim(0,5000) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_blank())
  })
  

  
######################################
#Numeric Analysis Tables
######################################
  
#https://rstudio.github.io/DT.html for datatable & (options) details
  
  output$table1 <- renderDT({

    fxlist <- list("mean" = mean, "median" = median, "min" = min, "max" = max, "sd" = sd)
  datatable(
    five_summary_type <- nfl_pbp |>
      group_by(penalty_type)|>
      summarize(across(years_experience,.fns = fxlist,.names = "{.col}_{.fn}", na.rm = TRUE),
                Penalties = n() #include count
      ) |>
      arrange(years_experience_mean),
    rownames = FALSE
  ) %>%
    formatRound(columns = 2:7, digits = 2)
  })
  
  output$table2 <- renderDT({
    
    datatable(nfl_pbp |>
      drop_na(position_group) |>
      group_by(position_group) |>
      summarize(Penalties = n()) |>
      arrange(desc(Penalties))
      ,
      options = list(dom = "t"),
      rownames = FALSE
    ) 
  })
  
  output$table3 <- renderDT({
    
    datatable(
      nfl_pbp |>
      drop_na(years_experience) |>
      group_by(years_experience) |>
      summarize(Penalties = n()) |>
      arrange(years_experience)
      ,
    options = list(dom = "t"),
    rownames = FALSE
    )
  })

######################################
#Custom Table
######################################

  
  output$custom <- renderPlot({

    ggplot(nfl_pbp, aes_string(x = input$x_var, y = input$y_var, fill = input$x_var)) +
      geom_bar(stat = "summary", fun = "mean") +
      scale_fill_manual(values = c(teams$team_color3[1:3], teams$team_color[2:32], teams$team_color4)) + #large enough list of colors to accommodate long x axis list
      scale_x_discrete(labels = label_wrap(10)) +
      theme_minimal() +
      theme(legend.position = "none")
      
  })
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)