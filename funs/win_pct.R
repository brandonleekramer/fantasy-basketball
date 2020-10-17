
# convert record to winning percentage 

win_pct <- function(team_data, overall){
  record <- enquo(overall)
  team_data <- team_data %>% 
    separate(overall, c("wins", "losses"), sep = "-") %>% 
    mutate(wins = as.numeric(wins),
           losses = as.numeric(losses),
           winpct = round(wins / (wins + losses), 3)) 
  team_data
}
