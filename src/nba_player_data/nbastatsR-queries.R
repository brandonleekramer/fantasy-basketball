suppressPackageStartupMessages(library(tidyverse))
library(gt)

library(nbastatR)

# 




teams_tables(teams = c("Brooklyn Nets", "New York Knicks"),
             seasons = 2017:2018, 
             tables = c("splits", "shooting"), 
             measures = "Base", 
             modes = c("PerGame", "Totals"))



# draft combine data 

?teams_tables







nbastatR::teams_players_stats(seasons = 2019, 
                              types = c("player"), 
                              tables = "general", 
                              measures = "Advanced", assign_to_environment = TRUE)


bref_players_stats(seasons = 2020, 
                   tables = c("advanced", "totals"), 
                   widen = TRUE, 
                   assign_to_environment = TRUE)
bref_advanced <- dataBREFPlayerAdvanced
bref_totals <- dataBREFPlayerTotals

?bref_players_stats

coaching <- coaching_staffs(seasons = 2019,
                            assign_to_environment = TRUE,
                            return_message = TRUE)
?coaching_staffs



