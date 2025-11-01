library(nflreadr)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinyalert)


# Read in 2009 - 2018 data 
# first, read NFL play by play csv (have to manage file size before actually reading in)
# then, use nflreadr::load_players() to gather all player info (position, date of birth, etc) to use for left join
# https://nflreadr.nflverse.com/reference/load_players.html

#nfl_pbp <- read_csv("NFL Play by Play 2009-2018 (v5).csv") 
players <- load_players()

#join data on gsis_id - an established key specific to each NFL player 
