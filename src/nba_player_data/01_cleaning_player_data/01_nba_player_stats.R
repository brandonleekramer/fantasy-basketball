rm(list = ls())
rm(check)
# in this file...
# i join all the basic and adv stats from kaggle datasets (1950-2016)
# then i scrape all the 2017-2019 data from bball reference 
# clean and add in all of the contract data from 2017-2019 
# bind the 1950-2016 datasets together 
# check missingness and do some summary stats 


library(readxl)
library(janitor)
library(tidyverse)
library(nbastatR)
library(naniar)
source("~/Documents/fantasy-basketball/funs/conv_to_points.R")

setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/")
basic_stats <- read_excel("NBA Players - Basic Season Stats (1950-2017).xlsx") %>% clean_names()

# join basic_stats and adv_stats 
stats_19502016 <- basic_stats %>% 
  filter(season_start != 2017) %>% 
  select(-number, -blanl, -blank2) %>% 
  rename(year = season_start, player = player_name, ft_rate = f_tr, 
         team = tm, position = pos, mins = mp, salary = player_salary_in) %>% 
  select(-salary, salary, -x3p_ar)

stats_19502016 %>% count()
# complete: year, player, position, team, g,  
# lots of missing: gs (6391), mins (486), per (523), ts_percent (84), ft_rate (97), 
# age can be imputed
stats_19502016 %>% count(is.na(orb_percent)) 


# MIGHT NEED TO TRY MATCHING USING FUZZY JOIN





#adv_stats <- read_csv("NBA Players - Advanced Season Stats (1978-2016).csv")
#adv_stats <- adv_stats %>% 
#  rename(salary = truesalary) %>% 
#  select(year, player, salary)

#stats_19502016 <- basic_stats %>% 
#  full_join(adv_stats, by = c("player", "year")) %>% 
#  arrange(-year, team, player, age)


# scrape the rest of the data   
bref_players_stats(seasons = c(2017, 2018, 2019, 2020), 
                   tables = c("advanced", "totals"), 
                   widen = TRUE, assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
bref_totals <- dataBREFPlayerTotals

# join the two dataframes together 
bref_combined <- bref_advanced %>% 
  full_join(bref_totals,  by = c("namePlayer", "slugSeason", "yearSeason", "slugPlayerSeason",
                                 "slugPosition", "agePlayer", "slugTeamBREF", "slugPlayerBREF",
  "countGames", "isSeasonCurrent", "isHOFPlayer", "slugTeamsBREF", "idPlayerNBA")) %>% 
  select(-starts_with("url")) 

# reorder and format scraped data to bind 
bref_20172019 <- bref_combined %>% 
  rename(year = yearSeason, player = namePlayer, position = slugPosition, team = slugTeamBREF,
         mins = minutes, age = agePlayer, g = countGames, gs = countGamesStarted,
         per = ratioPER, ts_percent = pctTrueShooting, ft_rate = pctFTRate, 
         orb_percent = pctORB, drb_percent = pctDRB, trb_percent = pctTRB,
         ast_percent = pctAST, stl_percent = pctSTL, blk_percent = pctBLK, 
         tov_percent = pctTOV, usg_percent = pctUSG, 
         ows = ratioOWS, dws = ratioDWS, ws = ratioWS, ws_48 = ratioWSPer48,
         obpm = ratioOBPM, dbpm = ratioDBPM, bpm = ratioBPM, vorp = ratioVORP,
         fg = fgmTotals, fga = fgaTotals, fg_percent = pctFG, 
         x3p = fg3mTotals, x3pa = fg3aTotals, x3p_percent = pctFG3, 
         x2p = fg2mTotals, x2pa = fg2aTotals, x2p_percent = pctFG2,
         e_fg_percent = pctEFG, ft = ftmTotals, fta = ftaTotals, ft_percent = pctFT,
         orb = orbTotals, drb = drbTotals, trb = trbTotals, ast = astTotals, 
         stl = stlTotals, blk = blkTotals, tov = tovTotals, pf = pfTotals, pts = ptsTotals) %>% 
  select(year, player, position, age, team, g, gs, mins, per, ts_percent, ft_rate, 
         orb_percent, drb_percent, trb_percent, ast_percent, stl_percent, 
         blk_percent, tov_percent, usg_percent, ows, dws, ws, ws_48, 
         obpm, dbpm, bpm, vorp, fg, fga, fg_percent, x3p, x3pa, x3p_percent, 
         x2p, x2pa, x2p_percent, e_fg_percent, ft, fta, ft_percent, 
         orb, drb, trb, ast, stl, blk, tov, pf, pts, -slugSeason, 
         -isSeasonCurrent, -isHOFPlayer, -idPlayerNBA, 
         -slugPlayerBREF, -slugPlayerSeason, -slugTeamsBREF) %>% 
  filter(year != 2016) %>% 
  clean_nba_names(player)

# adding in salary for 2017-18
source("~/Documents/fantasy-basketball/funs/conv_to_points.R")
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
salary_201718 <- read_csv("NBA_season1718_salary.csv")
salary_201718 <- salary_201718 %>% 
  rename(player = Player, salary = season17_18) %>% 
  select(player, salary)%>% 
  mutate(year = 2017) %>% 
  clean_nba_names(player)
tomerge_201718 <- bref_20172019 %>% 
  select(player, year) %>% 
  full_join(salary_201718, by = c("player", "year")) %>% 
  filter(year == 2017) 

# adding in salary for 2018-19
source("~/Documents/fantasy-basketball/funs/conv_to_points.R")
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
salary_201819 <- read_csv("nba_salaries_201819.csv")
salary_201819 <- salary_201819 %>% 
  mutate(year = 2018) %>% 
  clean_nba_names(player)
tomerge_201819 <- bref_20172019 %>% 
  select(player, year) %>% 
  full_join(salary_201819, by = c("player", "year")) %>% 
  filter(year == 2018) %>% 
  select(-team)

# adding in salary for 2018-19
source("~/Documents/fantasy-basketball/funs/conv_to_points.R")
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_salaries/")
salary_201920 <- read_csv("nba_salaries_201920.csv")
salary_201920 <- salary_201920 %>% 
  rename(player = Player, salary = `y2019-20`) %>% 
  mutate(year = 2019) %>% 
  clean_nba_names(player) %>%
  select(player, year, salary) 
tomerge_201920 <- bref_20172019 %>% 
  full_join(salary_201920, by = c("player", "year")) %>% 
  filter(year == 2019) %>% 
  select(player, year, salary)

# add in 2017-19 contracts 
contracts_201719 <- tomerge_201718 %>% 
  bind_rows(tomerge_201819) %>% 
  bind_rows(tomerge_201920)
bref_20172019 <- bref_20172019 %>% 
  full_join(contracts_201719, by = c("player", "year"))

# bind 1950-2016 and 2017-2019 datasets together 
stats_19502019 <- stats_19502016 %>% 
  bind_rows(bref_20172019) %>% 
  rename(games = g) %>% 
  arrange(year, player, age)

# check missingness (full dataset)
pct_complete_case(stats_19502019) # 40.25608
pct_complete_var(stats_19502019) # 2
pct_miss_var(stats_19502019) # 98 
# lots of missingness bc pre-1970 has no advanced 
# and pre-1985 has no contract data

stats_19782019 <- stats_19502019 %>% filter(year > 1977)
# check missingness 
pct_complete_case(stats_19782019) # 49.82339
pct_complete_var(stats_19782019) # 2
pct_miss_var(stats_19782019) # 98 

stats_19852019 <- stats_19502019 %>% filter(year > 1984)
# check missingness 
pct_complete_case(stats_19852019) # 56.71285
pct_complete_var(stats_19852019) # 2
pct_miss_var(stats_19852019) # 98

stats_19902019 <- stats_19502019 %>% filter(year > 1989)
# check missingness 
pct_complete_case(stats_19902019) # 63.56108
pct_complete_var(stats_19902019) # 2
pct_miss_var(stats_19902019) # 98

stats_20002019 <- stats_19502019 %>% filter(year > 1999)
# check missingness 
pct_complete_case(stats_20002019) # 65.9397
pct_complete_var(stats_20002019) # 2
pct_miss_var(stats_20002019) # 98

stats_20102019 <- stats_19502019 %>% filter(year > 2009)
# check missingness 
pct_complete_case(stats_20102019) # 70.67744
pct_complete_var(stats_20102019) # 2
pct_miss_var(stats_20102019) # 98

# remove dupllicates
stats_19902019_dedup <- stats_19902019 %>% 
  distinct(player, year, .keep_all = TRUE) %>% 
  arrange(year, player, team)

# impute some missingness 
stats_19902019_dedup <- stats_19902019_dedup %>% 
  mutate(x3p_percent = ifelse(x3p == 0 & x3pa == 0, 0, x3p_percent)) %>% 
  mutate(x3p_percent = ifelse(x3p == 0 & x3pa == 0, 0, x3p_percent)) %>% 
  # fix stars in the names 
  mutate(player = str_replace_all(player, "\\*", ""))

pct_complete_case(stats_19902019_dedup) # 74.1268
pct_complete_case(stats_19902019_dedup %>% select(-salary)) # 97.37566 (lots of missing salaries)
pct_complete_var(stats_19902019_dedup %>% select(-salary)) # 64
pct_miss_var(stats_19902019_dedup %>% select(-salary)) # 36

check <- stats_19902019_dedup %>%  
  filter(is.na(salary))
missing_salaries <- check %>% group_by(year) %>% count(is.na(salary))
# LEFT OFF HERE REALIZING THAT THERE IS STILL A LOT OF MISSING SALARY DATA 

check %>% group_by(year) %>% count(is.na(player))




gsd2_draft_data <- stats_19902019_dedup %>% 
  filter(year == "2018" | year == "2019") 





# write the 1950-2017 and 1990-2017 datasets 
write_rds(stats_19502019, "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/nba_stats_19502019_wdupes.Rds")

write_rds(stats_19902019, "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/nba_stats_19902019_wdupes.Rds")

write_rds(stats_19902019_dedup, "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/nba_stats_19902019_dedup.Rds")

write_rds(gsd2_draft_data, "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/gsd2_draft_data.Rds")

read







### 
#select(-column_s, -column_x) %>% 
rename(orb_pg = orb, drb_pg = drb, trb_pg = trb, 
       ast_pg = ast, stl_pg = stl, blk_pg = blk, 
       tov_pg = tov) %>% 
  select(year, player, pos, age, tm, g, gs, mp, per, ts_percent, ft_rate, 
         orb_percent, drb_percent, trb_percent, ast_percent, stl_percent, 
         blk_percent, tov_percent, usg_percent, ows, dws, ws, ws_48, 
         obpm, dbpm, bpm, vorp, fg, fga, fg_percent, x3p, x3pa, x3p_percent, 
         x2p, x2pa, x2p_percent, e_fg_percent, ft, fta, ft_percent, 
         orb, drb, trb, ast, stl, blk, tov, pf, pts, player_salary_in)

stats_19502016 <- adv_stats %>% 
  full_join(basic_stats, by = c("player", "year", "tm", "g", "mp", "per", "age", 
                                "ows", "dws", "ws", "ws_48", "obpm", "dbpm", "bpm", "vorp")) %>% 
  #arrange(year) %>% 
  mutate(player = str_replace_all(player, "\\*", "")) %>% 
  select(year, player, pos, age, tm, g, gs, mp, per, ts_percent, f_tr, 
         orb_percent, drb_percent, trb_percent, ast_percent, stl_percent, 
         blk_percent, tov_percent, usg_percent, ows, dws, ws, ws_48, 
         obpm, dbpm, bpm, vorp, fg, fga, fg_percent, x3p, x3pa, x3p_percent, 
         x2p, x2pa, x2p_percent, e_fg_percent, ft, fta, ft_percent, 
         orb, drb, trb, ast, stl, blk, tov, pf, pts, player_salary_in) %>% 
  rename(salary = player_salary_in) %>% 
  arrange(year)












