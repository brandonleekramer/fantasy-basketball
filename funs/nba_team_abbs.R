
# function to recode names to abbreviations 
nba_team_to_abbs <- function(df, team){
  team <- enquo(team)
  df <- df %>%
    mutate(team_abb = tolower(team),
           team_abb = trimws(team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Atlanta Hawks)\\b"),
                             yes = "ATL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Brooklyn Nets|BRK)\\b"),
                             yes = "BKN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Boston Celtics)\\b"),
                             yes = "BOS", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Charlotte Hornets)\\b"),
                             yes = "CHO", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Chicago Bulls)\\b"),
                             yes = "CHI", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Cleveland Cavaliers)\\b"),
                             yes = "CLE", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Dallas Mavericks)\\b"),
                             yes = "DAL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Denver Nuggets)\\b"),
                             yes = "DEN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Detroit Pistons)\\b"),
                             yes = "DET", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Golden State Warriors|GS)\\b"),
                             yes = "GSW", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                              pattern = "\\b(?i)(Houston Rockets)\\b"),
                            yes = "HOU", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Indiana Pacers)\\b"),
                             yes = "IND", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Los Angeles Clippers)\\b"),
                             yes = "LAC", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Los Angeles Lakers)\\b"),
                             yes = "LAL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Memphis Grizzlies)\\b"),
                             yes = "MEM", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Miami Heat)\\b"),
                             yes = "MIA", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                              pattern = "\\b(?i)(Milwaukee Bucks)\\b"),
                             yes = "MIL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Minnesota Timberwolves)\\b"),
                             yes = "MIN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(New Orleans Pelicans|NO)\\b"),
                             yes = "NOP", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(New York Knicks|NY)\\b"),
                             yes = "NYK", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Oklahoma City Thunder)\\b"),
                             yes = "OKC", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Orlando Magic)\\b"),
                             yes = "ORL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Philadelphia 76ers)\\b"),
                             yes = "PHI", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Phoenix Suns|PHX)\\b"),
                             yes = "PHO", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                              pattern = "\\b(?i)(Portland Trail Blazers)\\b"),
                            yes = "POR", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Sacramento Kings)\\b"),
                             yes = "SAC", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(San Antonio Spurs|SA)\\b"),
                             yes = "SAS", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                              pattern = "\\b(?i)(Toronto Raptors)\\b"),
                            yes = "TOR", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = paste0("\\b(?i)(Utah Jazz)\\b")),
                             yes = "UTA", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(Washington Wizards)\\b"),
                             yes = "WAS", no = team_abb)) %>% 
    # historical 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(ST. LOUIS HAWKS|ST LOUIS HAWKS)\\b"),
                             yes = "SLH", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(MILWAUKEE HAWKS)\\b"),
                             yes = "MIL", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(TRI-CITIES BLACKHAWKS)\\b"),
                             yes = "TCB", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(NEW JERSEY NETS)\\b"),
                             yes = "NJN", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(CHARLOTTE BOBCATS)\\b"),
                             yes = "CHA", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(FORT WAYNE PISTONS)\\b"),
                             yes = "FWP", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(SAN FRANCISCO WARRIORS)\\b"),
                             yes = "SFW", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(SAN DIEGO CLIPPERS)\\b"),
                             yes = "SDC", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(BUFFALO BRAVES)\\b"),
                             yes = "BUF", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(MINNEAPOLIS LAKERS)\\b"),
                             yes = "MIN", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(VANCOUVER GRIZZLIES)\\b"),
                             yes = "VAN", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(NEW ORLEANS HORNETS|OKLAHOMA CITY HORNETS)\\b"),
                             yes = "NOK", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(SEATTLE SUPERSONICS)\\b"),
                             yes = "SEA", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(PHILADELPHIA 76ERS)\\b"),
                             yes = "PHI", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(SYRACUSE NATIONALS)\\b"),
                             yes = "SYR", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(KANSAS CITY KINGS|KANSAS CITY-OMAHA KINGS)\\b"),
                             yes = "KCK", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(CINCINNATI ROYALS)\\b"),
                             yes = "CIN", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(ROCHESTER ROYALS)\\b"),
                             yes = "ROR", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(NEW ORLEANS JAZZ)\\b"),
                             yes = "NOJ", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(WASHINGTON BULLETS|WASHINGTON CAPITOLS)\\b"),
                             yes = "WAS", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(CAPITAL BULLETS)\\b"),
                             yes = "CAP", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(BALTIMORE BULLETS)\\b"),
                             yes = "BAL", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(CHICAGO ZEPHYRS|CHICAGO PACKERS|CHICAGO STAGS)\\b"),
                             yes = "CHI", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(ANDERSON PACKERS)\\b"),
                             yes = "AND", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(INDIANAPOLIS OLYMPIANS)\\b"),
                             yes = "IND", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(SHEBOYGAN RED SKINS)\\b"),
                             yes = "SRS", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(ST. LOUIS BOMBERS)\\b"),
                             yes = "SLB", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team_abb,
                                               pattern = "\\b(?i)(WATERLOO HAWKS)\\b"),
                             yes = "WAT", no = team_abb)) 
  df
}



# function to recode abbreviations to names 
nba_current_abbs_to_team <- function(df, team){
  team <- enquo(team)
  df <- df %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(ATL)\\b"),
                             yes = "Atlanta Hawks", no = team)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(BKN|BRK)\\b"),
                             yes = "Brooklyn Nets", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(BOS)\\b"),
                             yes = "Boston Celtics", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(CHO)\\b"),
                             yes = "Charlotte Hornets", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(CHI)\\b"),
                             yes = "Chicago Bulls", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(CLE)\\b"),
                             yes = "Cleveland Cavaliers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(DAL)\\b"),
                             yes = "Dallas Mavericks", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(DEN)\\b"),
                             yes = "Denver Nuggets", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(DET)\\b"),
                             yes = "Detroit Pistons", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(GSW|GS)\\b"),
                             yes = "Golden State Warriors", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(HOU)\\b"),
                             yes = "Houston Rockets", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(IND)\\b"),
                             yes = "Indiana Pacers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(LAC)\\b"),
                             yes = "Los Angeles Clippers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(LAL)\\b"),
                             yes = "Los Angeles Lakers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(MEM)\\b"),
                             yes = "Memphis Grizzlies", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(MIA)\\b"),
                             yes = "Miami Heat", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(MIL)\\b"),
                             yes = "Milwaukee Bucks", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(MIN)\\b"),
                             yes = "Minnesota Timberwolves", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(NO|NOP)\\b"),
                             yes = "New Orleans Pelicans", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(NY|NYK)\\b"),
                             yes = "New York Knicks", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(OKC)\\b"),
                             yes = "Oklahoma City Thunder", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(ORL)\\b"),
                             yes = "Orlando Magic", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(PHI)\\b"),
                             yes = "Philadelphia 76ers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(PHO|PHX)\\b"),
                             yes = "Phoenix Suns", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(POR)\\b"),
                             yes = "Portland Trail Blazers", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(SAC)\\b"),
                             yes = "Sacramento Kings", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(SAS|SA)\\b"),
                             yes = "San Antonio Spurs", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(TOR)\\b"),
                             yes = "Toronto Raptors", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(UTA)\\b")),
                             yes = "Utah Jazz", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = "\\b(?i)(WAS)\\b"),
                             yes = "Washington Wizards", no = team_abb)) 
  df
}


