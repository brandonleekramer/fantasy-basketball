rm(list = ls())

for (pkg in c( "tidyverse", "janitor")) {library(pkg, character.only = TRUE)}

setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
players <- read_csv("nba_players.csv") %>% rename(player_id = `_id`) %>% clean_names() 
salaries <- read_csv("nba_player_salaries_19852018.csv") %>% clean_names()

cleaned_salaries <- players %>% 
  full_join(salaries, by = "player_id") %>% 
  drop_na(salary) %>% 
  select(player_id, team, season, name, position, salary) %>% 
  arrange(season, team, -salary)

cleaned_salaries

write_rds(cleaned_salaries, "nba_player_salaries_19852018_cleaned.Rds")
