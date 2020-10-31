

# cleans messy strings in basketball refernece data for matching later 
clean_nba_names <- function(df, player){
  player <- enquo(player)
  df <- df %>%  
    mutate(player = iconv(player, to="ASCII//TRANSLIT")) %>%   
    mutate(player = str_replace_all(player, "\\'", "")) %>%  
    mutate(player = str_replace_all(player, c('Á' = 'A', 'ā' = 'a', 'á' = 'a', 
           'č' = 'c', 'ć' = 'c', 'è' = 'e', 'é' = 'e', 'ê' = 'e', 'ić' = 'ic', 
           'ö' = 'o', 'ó' = 'o', 'Š' = 'S', 'š' = 's', 'ū' = 'u', 'ý' = 'y', 
           'Ž' = 'Z', 'ž' = 'z', 'Élie' = 'Elie', 'P.J.'='PJ','C.J.'='CJ', 'R.J.'='RJ', 
           'T.J.'='TJ', 'D.J.'='DJ', 'B.J.'='BJ', 'J.J.'='JJ', "O.G."="OG", "K.J."="KJ", 
           'A.J.'='AJ', 'J.R.'='JR', "Nene" = "Nene Hilario", "Demarre" = "DeMarre", 
           'Dončić' = 'Doncic', 'Jaren Jackson' = 'Jaren Jackson Jr.',
           'Kelly Oubre' = 'Kelly Oubre Jr.', 'Marvin Bagley' = 'Marvin Bagley III', 
           'James Ennis III' = 'James Ennis', 'Otto Porter' = 'Otto Porter Jr.', 
           'Wendell Carter' = 'Wendell Carter Jr.', 'Larry Nance' = 'Larry Nance Jr.',
           'Walt Lemon' = 'Walt Lemon Jr.', 'Dennis Schr"oder' = 'Dennis Schroder', 
           'Tim Hardaway' = 'Tim Hardaway Jr.', 'Dennis Smith' = 'Dennis Smith Jr.',
           'Wes Iwundu' = 'Wesley Iwundu', 'Marcus Morris Sr.' = 'Marcus Morris',
           'Frank Mason III' = 'Frank Mason', 'Juan Hernangomez' = 'Juancho Hernangomez',
           'Taurean Waller-Prince' = 'Taurean Prince', 
           "Hilario Hilario" = "Hilario", "Jr. Jr." = "Jr.", "III III" = "III")))
  df
}



# converts basketball reference averages to fppg in my fantasy leagues 
bbrhist_avgs_to_fppg <- function(df){
  df <- df %>%
    mutate(ld_fppg_bbr = pts/games + (drb/games *1.5) + (orb/games*2) + (ast/games *2) + (stl/games*2.5) + 
    (blk/games*2.5) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-1)) %>% 
    mutate(fr_fppg_bbr = pts/games + (drb/games*1.5) + (orb/games*2) + (ast/games*2) + (stl/games*2.5) + 
    (blk/games*2.5) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-1)) %>%
    mutate(botb_fppg_bbr = pts/games + drb/games + (orb/games*1.25) + (ast/games*1.5) + (stl/games*1.5) + 
    (blk/games) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-0.75)) %>%
    mutate(gsd2_fppg_bbr = pts/games + drb/games + (orb/games*1.25) + (ast/games*1.5) + (stl/games*1.5) + 
    (blk/games*2) + (tov/games*-1) + x3p/games + (fga/games*-0.5) + (fg/games*0.5) + ft/games + (fta/games*-0.75)) %>% 
    mutate(ld_totals = pts + (drb*1.5) + (orb*2) + (ast*2) + (stl*2.5) + 
             (blk*2.5) + (tov*-1) + x3p + (fga*-0.5) + (fg*0.5) + ft + (fta*-1)) %>% 
    mutate(fr_totals = pts + (drb*1.5) + (orb*2) + (ast*2) + (stl*2.5) + 
             (blk*2.5) + (tov*-1) + x3p + (fga*-0.5) + (fg*0.5) + ft + (fta*-1)) %>% 
    mutate(botb_totals = pts + drb + (orb*1.25) + (ast*1.5) + (stl*1.5) + 
             (blk*2) + (tov*-1) + x3p+ (fga*-0.5) + (fg*0.5) + ft + (fta*-0.75)) %>% 
    mutate(gsd2_totals = pts + drb + (orb*1.25) + (ast*1.5) + (stl*1.5) + 
             (blk*2) + (tov*-1) + x3p+ (fga*-0.5) + (fg*0.5) + ft + (fta*-0.75))
  df
}



# converts basketball reference averages to fppg in my fantasy leagues 
bbr_avgs_to_fppg <- function(df){
  df <- df %>%
    mutate(ld_fppg_bbr = PTS + (DRB*1.5) + (ORB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-1)) %>% 
    mutate(fr_fppg_bbr = PTS + (DRB*1.5) + (ORB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-1)) %>%
    mutate(botb_fppg_bbr = PTS + DRB + (ORB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-0.75)) %>%
    mutate(gsd2_fppg_bbr = PTS + DRB + (ORB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-0.75))  
  df
}

# converts basketball reference totals to fppg in my fantasy leagues 
bbr_ttls_to_fppg <- function(df){
  df <- df %>%
    mutate(ld_fppg_bbr = PTS + (DRB*1.5) + (ORB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-1) / G) %>% 
    mutate(fr_fppg_bbr = PTS + (DRB*1.5) + (ORB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-1) / G) %>%
    mutate(botb_fppg_bbr = PTS + DRB + (ORB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-0.75) / G) %>%
    mutate(gsd2_fppg_bbr = PTS + DRB + (ORB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TOV*-1) + `3P` + (FGA*-0.5) + (FG*0.5) + FT + (FTA*-0.75) / G) 
}

# converts hastag projection averages to fppg in my fantasy leagues
hashtag_avgs_to_fppg <- function(df){
  df <- df %>%
    mutate(ld_fppg_ht = PTS + (DREB*1.5) + (OREB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-1)) %>% 
    mutate(fr_fppg_ht = PTS + (DREB*1.5) + (OREB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-1)) %>%
    mutate(botb_fppg_ht = PTS + DREB + (OREB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-0.75)) %>%
    mutate(gsd2_fppg_ht = PTS + DREB + (OREB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-0.75))  
  df
}


# converts hastag projection averages to fppg in my fantasy leagues
hashalt_avgs_to_fppg <- function(df){
  df <- df %>%
    rename(TO = TOV) %>% 
    mutate(DREB = REB*0.78,
           OREB = REB*0.22) %>%
    mutate(ld_fppg_hta = PTS + (DREB*1.5) + (OREB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-1)) %>% 
    mutate(fr_fppg_hta = PTS + (DREB*1.5) + (OREB*2) + (AST*2) + (STL*2.5) + 
             (BLK*2.5) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-1)) %>%
    mutate(botb_fppg_hta = PTS + DREB + (OREB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-0.75)) %>%
    mutate(gsd2_fppg_hta = PTS + DREB + (OREB*1.25) + (AST*1.5) + (STL*1.5) + 
             (BLK*2) + (TO*-1) + THREES + (FGA*-0.5) + (FGM*0.5) + FTM + (FTA*-0.75))  
  df
}







