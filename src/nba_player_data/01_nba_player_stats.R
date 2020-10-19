rm(list = ls())

library(readxl)
library(janitor)
library(tidyverse)
library(nbastatR)
source("~/Documents/fantasy-basketball/funs/conv_to_points.R")

setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/")
basic_stats <- read_excel("NBA Players - Basic Season Stats (1950-2017).xlsx") %>% clean_names()
adv_stats <- read_excel("NBA Players - Advanced Season Stats (1978-2016).xlsx")

# join basic_stats and adv_stats 
basic_stats <- basic_stats %>% 
  filter(season_start != 2017) %>% 
  select(-number, -blanl, -blank2) %>% 
  rename(year = season_start, player = player_name)
adv_stats <- adv_stats %>% 
  select(-column_s, -column_x) %>% 
  rename(orb_pg = orb, drb_pg = drb, trb_pg = trb, 
         ast_pg = ast, stl_pg = stl, blk_pg = blk, 
         tov_pg = tov) 

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
       orb, drb, trb, ast, stl, blk, tov, pf, pts) %>% 
  arrange(year)



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
  rename(year = yearSeason, player = namePlayer, pos = slugPosition, tm = slugTeamBREF,
         mp = minutes, age = agePlayer, g = countGames, gs = countGamesStarted,
         per = ratioPER, ts_percent = pctTrueShooting, f_tr = pctFTRate, 
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
  select(year, player, pos, age, tm, g, gs, mp, per, ts_percent, f_tr, 
         orb_percent, drb_percent, trb_percent, ast_percent, stl_percent, 
         blk_percent, tov_percent, usg_percent, ows, dws, ws, ws_48, 
         obpm, dbpm, bpm, vorp, fg, fga, fg_percent, x3p, x3pa, x3p_percent, 
         x2p, x2pa, x2p_percent, e_fg_percent, ft, fta, ft_percent, 
         orb, drb, trb, ast, stl, blk, tov, pf, pts, -slugSeason, 
         -isSeasonCurrent, -isHOFPlayer, -idPlayerNBA, -slugPlayerBREF, -slugPlayerSeason, -slugTeamsBREF) 

stats_19502019 <- stats_19502016 %>% 
  bind_rows(bref_20172019) %>% 
  rename(games = g)

write_rds(stats_19502019, "~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/nba_stats_19502019.Rds")




stats_19502019 %>% 
  rename(games = g) %>% 
  mutate(ld_fppg_bbr = pts/games + (drb/games *1.5) + (orb/games*2) + (ast/games *2) + (stl/games*2.5) + 
  (blk/games*2.5) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-1)) %>% 
  mutate(fr_fppg_bbr = pts/games + (drb/games*1.5) + (orb/games*2) + (ast/games*2) + (stl/games*2.5) + 
           (blk/games*2.5) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-1)) %>%
  mutate(botb_fppg_bbr = pts/games + drb/games + (orb/games*1.25) + (ast/games*1.5) + (stl/games*1.5) + 
           (blk/games) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-0.75)) %>%
  mutate(gsd2_fppg_bbr = pts/games + drb/games + (orb/games*1.25) + (ast/games*1.5) + (stl/games*1.5) + 
           (blk/games*2) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-0.75)) %>% 
  arrange(-ld_fppg_bbr) %>% 
  select(year, player, pos, age, tm, contains("fppg"))












