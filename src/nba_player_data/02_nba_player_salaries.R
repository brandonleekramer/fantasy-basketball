rm(list = ls())

for (pkg in c( "tidyverse", "janitor")) {library(pkg, character.only = TRUE)}
source("~/Documents/fantasy-basketball/funs/nba_team_abbs.R")

### clean raw data and output that to an Rds 
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
players <- read_csv("nba_players.csv") %>% rename(player_id = `_id`) %>% clean_names() 
salaries <- read_csv("nba_player_salaries_19852018.csv") %>% clean_names()
salaries201819 <- read_csv("nba_salaries_13_19.csv")
salaries201920 <- read_csv("nba_salaries_201920.csv")

cleaned_salaries <- players %>% 
  full_join(salaries, by = "player_id") %>% 
  drop_na(salary) %>% 
  select(player_id, team, season, name, position, salary) %>% 
  arrange(season, team, -salary)

cleaned_salaries <- cleaned_salaries %>% 
  drop_na(team) %>% 
  nba_team_to_abbs(team) 






write_rds(cleaned_salaries, 
  "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/nba_player_salary_history.Rds")

