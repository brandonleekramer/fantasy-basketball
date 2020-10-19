
rm(list = ls())
for (pkg in c( "tidyverse", "janitor", "nbastatR", "naniar")) {library(pkg, character.only = TRUE)}

# pull in the 1950-2017 data 
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/")
nba_player_data <- read_rds("bbref_19702019.Rds") 

setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
nba_salary_data <- read_rds("nba_player_salary_history.Rds")




nba_player_data %>% 
  left_join(nba_salary_data, by = c("player", "tm"))

str(nba_player_data)
