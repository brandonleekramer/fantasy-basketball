
# function to recode names to abbreviations 
nba_team_abbs <- function(df, old_col){
  old_col <- enquo(old_col)
  df <- df %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Houston Rockets)\\b")),
                            yes = "HOU", no = team)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Milwaukee Bucks)\\b")),
                            yes = "MIL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Denver Nuggets)\\b")),
                            yes = "DEN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Portland Trail Blazers)\\b")),
                            yes = "POR", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Toronto Raptors)\\b")),
                            yes = "TOR", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Indiana Pacers)\\b")),
                            yes = "IND", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Boston Celtics)\\b")),
                            yes = "BOS", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                              pattern = paste0("\\b(?i)(Oklahoma City Thunder)\\b")),
                            yes = "OKC", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Los Angeles Lakers)\\b")),
                             yes = "LAC", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Los Angeles Clippers)\\b")),
                             yes = "LAL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Miami Heat)\\b")),
                             yes = "MIA", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Utah Jazz)\\b")),
                             yes = "UTA", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(San Antonio Spurs)\\b")),
                             yes = "SA", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Washington Wizards)\\b")),
                             yes = "WAS", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Phoenix Suns)\\b")),
                             yes = "PHX", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Memphis Grizzlies)\\b")),
                             yes = "MEM", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Orlando Magic)\\b")),
                             yes = "ORL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Sacramento Kings)\\b")),
                             yes = "SAC", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Chicago Bulls)\\b")),
                             yes = "CHI", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Dallas Mavericks)\\b")),
                             yes = "DAL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(New York Knicks)\\b")),
                             yes = "NY", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Atlanta Hawks)\\b")),
                             yes = "ATL", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Minnesota Timberwolves)\\b")),
                             yes = "MIN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Golden State Warriors)\\b")),
                             yes = "GS", no = team_abb)) %>% 
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Philadelphia 76ers)\\b")),
                             yes = "PHI", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Brooklyn Nets)\\b")),
                             yes = "BKN", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(New Orleans Pelicans)\\b")),
                             yes = "NO", no = team_abb)) %>% 
    
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Cleveland Cavaliers)\\b")),
                             yes = "CLE", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Detroit Pistons)\\b")),
                             yes = "DET", no = team_abb)) %>%
    mutate(team_abb = ifelse(test = str_detect(string = team,
                                               pattern = paste0("\\b(?i)(Charlotte Hornets)\\b")),
                             yes = "CHA", no = team_abb)) 
  df
}
