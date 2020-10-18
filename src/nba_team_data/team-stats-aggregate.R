rm(list = ls())

for (pkg in c( "tidyverse", "janitor", "data.table", "readxl")) {library(pkg, character.only = TRUE)}

source("../funs/nba_team_abbs.R")
source("../funs/win_pct.R")

setwd("~/Documents/fantasy-basketball/data/nba_team_records/")
nbat_200708 <- read_csv("nba_team200708_stats.csv") %>% clean_names() %>% mutate(year = 2008)
nbat_200809 <- read_csv("nba_team200809_stats.csv") %>% clean_names() %>% mutate(year = 2009)
nbat_200910 <- read_csv("nba_team200910_stats.csv") %>% clean_names() %>% mutate(year = 2010)
nbat_201011 <- read_csv("nba_team201011_stats.csv") %>% clean_names() %>% mutate(year = 2011)
nbat_201112 <- read_csv("nba_team201112_stats.csv") %>% clean_names() %>% mutate(year = 2012)
nbat_201213 <- read_csv("nba_team201213_stats.csv") %>% clean_names() %>% mutate(year = 2013)
nbat_201314 <- read_csv("nba_team201314_stats.csv") %>% clean_names() %>% mutate(year = 2014)
nbat_201415 <- read_csv("nba_team201415_stats.csv") %>% clean_names() %>% mutate(year = 2015)
nbat_201516 <- read_csv("nba_team201516_stats.csv") %>% clean_names() %>% mutate(year = 2016)
nbat_201617 <- read_csv("nba_team201617_stats.csv") %>% clean_names() %>% mutate(year = 2017)
nbat_201718 <- read_csv("nba_team201718_stats.csv") %>% clean_names() %>% mutate(year = 2018)
nbat_201819 <- read_csv("nba_team201819_stats.csv") %>% clean_names() %>% mutate(year = 2019)
nbat_201920 <- read_csv("nba_team201920_stats.csv") %>% clean_names() %>% mutate(year = 2020)

nba_team_data <- bind_rows(nbat_200708, nbat_200809, nbat_200910, nbat_201011, 
          nbat_201112, nbat_201213, nbat_201314, nbat_201415, 
          nbat_201516, nbat_201617, nbat_201718, nbat_201819, nbat_201920) %>% 
  nba_team_abbs(team) %>% 
  win_pct(overall) %>% 
  select(team_abb, winpct, year)

rm(nbat_200708, nbat_200809, nbat_200910, nbat_201011, 
   nbat_201112, nbat_201213, nbat_201314, nbat_201415, 
   nbat_201516, nbat_201617, nbat_201718, nbat_201819, nbat_201920)


nbat_201920 %>% win_pct(overall)


players_to_team <- function(player_data, team_data){
  team_totals <- player_data %>% 
    filter(team != "(N/A)") %>% 
    group_by(team) %>% 
    summarize(f_pts = sum(f_pts), fp_g = sum(fp_g),
              fgm = sum(fgm), fga = sum(fga), 
              ftm = sum(ftm), fta = sum(fta),
              pts = sum(pts), x3ptm = sum(x3ptm),
              oreb = sum(oreb), dreb = sum(dreb),
              ast = sum(ast), stl = sum(st),
              blk = sum(blk), to = sum(to)) %>% 
    arrange(-f_pts) %>% 
    left_join(team_data %>% 
                separate(overall, c("wins", "losses"), sep = "-") %>% 
                mutate(wins = as.numeric(wins),
                       losses = as.numeric(losses),
                       winpct = round(wins / (wins + losses), 3)) %>% 
                select(team, winpct), by = "team") %>% 
    mutate(year = 2020)
  team_totals
}



players_to_team(nbap_201920, nbat_201920)




nbat_201920 %>% nba_team_abbs(team) %>% 
  select(team_abb)


nbat_201920 %>% 
  separate(overall, c("wins", "losses"), sep = "-") %>% 
  mutate(wins = as.numeric(wins),
         losses = as.numeric(losses),
         winpct = round(wins / (wins + losses), 3)) %>% 
  select(team, winpct)


nbat_201819 %>% 
  mutate(new_abb = ifelse(test = str_detect(string = team,
                          pattern = paste0("\\b(?i)(Houston Rockets)\\b")),
                          yes = "HOU", no = team)) %>%
  mutate(new_abb = ifelse(test = str_detect(string = team,
                                            pattern = paste0("\\b(?i)(Milwaukee Bucks)\\b")),
                          yes = "MIL", no = new_abb)) %>%
  mutate(new_abb = ifelse(test = str_detect(string = team,
                                            pattern = paste0("\\b(?i)(Denver Nuggets)\\b")),
                          yes = "DEN", no = new_abb)) %>%
  mutate(new_abb = ifelse(test = str_detect(string = team,
                                            pattern = paste0("\\b(?i)(Portland Trail Blazers)\\b")),
                          yes = "POR", no = new_abb)) %>%
  select(new_abb)








