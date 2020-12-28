

adv_value_calc <- function(df,  teams, pts, salaries){
  
  library("tidyverse")
  library("tidymodels")
  
  teams <- enquo(teams)
  pts <- enquo(pts)
  salaries <- enquo(salaries)
  
  prediction_data <- df %>% 
    drop_na(!!(pts), !!(salaries)) %>% 
    filter(!!(teams) != "FA") %>% 
    rename(teams = !!(teams),
           fppg = !!(pts),
           salaries = !!(salaries)) %>% 
    select(player, teams, position, salaries, fppg)
  
  fit_lm <- lm(fppg ~ salaries, data = prediction_data) 
  prediction_data$lm_pred <- predict(fit_lm)   
  prediction_data$lm_value <- residuals(fit_lm) 
  
  fit_rf <- rand_forest() %>%
    set_mode("regression") %>% 
    set_engine("randomForest") %>% 
    fit(fppg ~ salaries, data = prediction_data)
  fit_rf_df <- as_tibble(
    fit_rf %>% 
      predict(new_data = prediction_data) %>% 
      rename("rf_predicted" = .pred)) 
  prediction_data$rf_pred <- fit_rf_df$rf_predicted 
  
  prediction_data %>% 
    mutate(# calculate rf residual 
           rf_value = fppg - rf_pred,
           # round values 
           lm_pred = round(lm_pred, 3),
           lm_value = round(lm_value, 3),
           rf_pred = round(rf_pred, 3),
           rf_value = round(rf_value, 3),
           # normalize value columns  
           #lm_norm = (lm_value - min(lm_value)) / (max(lm_value) - min(lm_value)),
           #rf_norm = (rf_value - min(rf_value)) / (max(rf_value) - min(rf_value)),
           lm_norm = (lm_value - mean(lm_value)) / sd(lm_value),
           rf_norm = (rf_value - mean(rf_value)) / sd(rf_value),
           lm_norm = round(lm_norm, 3),
           rf_norm = round(rf_norm, 3)) %>% 
    arrange(-lm_value) %>% 
    select(player, teams, fppg, lm_pred, lm_value, lm_norm,
           rf_pred, rf_value, rf_norm, position, salaries, fppg)
  

  
}



value_by_team <- function(input_data, teams_col, value_col, fppg_col, players_col){
  
  library("tidyverse")
  
  teams_col <- enquo(teams_col)
  value_col <- enquo(value_col)
  fppg_col <- enquo(fppg_col)
  players_col <- enquo(players_col)
  
  input_data <- input_data %>% 
    filter(!!(teams_col) != "FA") %>% 
    group_by(!!(teams_col)) %>% 
    top_n(!!(players_col), wt = !!(fppg_col)) %>% 
    summarize(team_value = sum(!!(value_col))) %>% 
    arrange(-team_value)
  
  input_data
}


team_value_structure <- function(input_data, teams_col, value_col, fppg_col){
  
  teams_col <- enquo(teams_col)
  value_col <- enquo(value_col)
  fppg_col <- enquo(fppg_col)
  
  value1 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 1) %>% 
    rename(top01 = team_value)
  value2 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 2) %>% 
    rename(top02 = team_value)
  value3 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 3) %>% 
    rename(top03 = team_value)
  value4 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 4) %>% 
    rename(top04 = team_value)
  value5 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 5) %>% 
    rename(top05 = team_value)
  value6 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 6) %>% 
    rename(top06 = team_value)
  value7 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 7) %>% 
    rename(top07 = team_value)
  value8 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 8) %>% 
    rename(top08 = team_value)
  value9 <- value_by_team(input_data = input_data,
                          teams_col = !!(teams_col), value_col = !!(value_col), 
                          fppg_col = !!(fppg_col), players_col = 9) %>% 
    rename(top09 = team_value)
  value10 <- value_by_team(input_data = input_data,
                           teams_col = !!(teams_col), value_col = !!(value_col), 
                           fppg_col = !!(fppg_col), players_col = 10) %>% 
    rename(top10 = team_value)
  value11 <- value_by_team(input_data = input_data,
                           teams_col = !!(teams_col), value_col = !!(value_col), 
                           fppg_col = !!(fppg_col), players_col = 11) %>% 
    rename(top11 = team_value)
  value12 <- value_by_team(input_data = input_data,
                           teams_col = !!(teams_col), value_col = !!(value_col), 
                           fppg_col = !!(fppg_col), players_col = 12) %>% 
    rename(top12 = team_value)
  value13 <- value_by_team(input_data = input_data,
                           teams_col = !!(teams_col), value_col = !!(value_col), 
                           fppg_col = !!(fppg_col), players_col = 13) %>% 
    rename(top13 = team_value)
  
  value_by_players <- value1 %>% 
    left_join(value2, by = "teams") %>% left_join(value3, by = "teams") %>% 
    left_join(value4, by = "teams") %>% left_join(value5, by = "teams") %>% 
    left_join(value6, by = "teams") %>% left_join(value7, by = "teams") %>% 
    left_join(value8, by = "teams") %>% left_join(value9, by = "teams") %>% 
    left_join(value10, by = "teams") %>% left_join(value11, by = "teams") %>% 
    left_join(value12, by = "teams") %>% left_join(value13, by = "teams") %>% 
    arrange(-top10)
  
  value_by_players
  
}







