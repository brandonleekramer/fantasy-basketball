

# cleans messy strings in basketball refernece data for matching later 
clean_nba_names <- function(df, player){
  player <- enquo(player)
  df <- df %>%  
    mutate(player = iconv(player, to="ASCII//TRANSLIT")) %>%   
    mutate(player = str_replace_all(player, "\\'", "")) %>%  
    mutate(player = str_replace_all(player, c('Á' = 'A', 'ā' = 'a', 'á' = 'a', 'č' = 'c', 'ć' = 'c', 'è' = 'e', 
                                              'é' = 'e', 'ê' = 'e', 'ić' = 'ic', 'ö' = 'o', 'ó' = 'o', 'Š' = 'S', 
                                              'š' = 's', 'ū' = 'u', 'ý' = 'y', 'Ž' = 'Z', 'ž' = 'z', 'Élie' = 'Elie',
                                              'P.J.'='PJ','C.J.'='CJ', 'R.J.'='RJ', 'T.J.'='TJ', 'D.J.'='DJ', 'B.J.'='BJ',
                                              'J.J.'='JJ', "O.G."="OG", "Demarre" = "DeMarre", 'Dončić' = 'Doncic',
                                              'Jaren Jackson' = 'Jaren Jackson Jr.','Kelly Oubre' = 'Kelly Oubre Jr.', 
                                              'Marvin Bagley' = 'Marvin Bagley III', 'Otto Porter' = 'Otto Porter Jr.', 
                                              'Wendell Carter' = 'Wendell Carter Jr.', 'Larry Nance' = 'Larry Nance Jr.',
                                              'Walt Lemon' = 'Walt Lemon Jr.', 'Dennis Schr"oder' = 'Dennis Schroder', 
                                    'Tim Hardaway' = 'Tim Hardaway Jr.', 'Taurean Waller-Prince' = 'Taurean Prince',
                                    "Jr. Jr." = "Jr.", "III III" = "III")))
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







