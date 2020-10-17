
rm(list = ls())
for (pkg in c( "tidyverse", "janitor", "nbastatR")) {library(pkg, character.only = TRUE)}

# pull in the 1950-2017 data 
setwd("~/Documents/fantasy-basketball/data/nba_player_data/nba_player_season/nba_player_season_byteam")
bref_19502017 <- read_csv("nba_player_season_by_team.csv") %>% clean_names()
bref_19502017 <- bref_19502017 %>% select(-x1, -blanl, -blank2, -x3p_ar) 

# scrape the rest of the data   
bref_players_stats(seasons = c(2018, 2019, 2020), tables = c("advanced", "totals"), widen = TRUE, assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
bref_totals <- dataBREFPlayerTotals

# join the two dataframes together 
bref_combined <- bref_advanced %>% 
  full_join(bref_totals,  by = c("namePlayer", "slugSeason", "yearSeason", "slugPlayerSeason",
                                 "slugPosition", "agePlayer", "slugTeamBREF", "slugPlayerBREF",
              "countGames", "isSeasonCurrent", "isHOFPlayer", "slugTeamsBREF", "idPlayerNBA")) %>% 
  select(-starts_with("url"))


bref_combined
bref_totals

bref_19502020 <- bref_combined %>% 
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
         -isSeasonCurrent, -isHOFPlayer, -idPlayerNBA, -slugPlayerBREF, -slugPlayerSeason, -slugTeamsBREF) %>% 
  bind_rows(bref_19502017) %>% 
  rename(position = pos, games = g, mins = mp, efg_percent = e_fg_percent)








