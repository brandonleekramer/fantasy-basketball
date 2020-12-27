
# nba team colors 
# for legacy, gsd2
nba_team_colors <- c("#C8102E", "#008348", "#061922", "#00788C", "#ce1141", 
                     "#860038", "#007dc5", "#0E2240", "#C8102E", "#1D428A",
                     "#ce1141", "#ffc633", "#1D428A", "#552582", "#5D76A9", 
                     "#98002e", "#00471b", "#78be20", "#b6995a", "#F58426",
                     "#EF3B24", "#007dc5", "#006bb6", "#e56020", "#000000", 
                     "#724c9f", "#bac3c9", "#ce1141", "#002B5C", "#002b5c")

# for franchise 
franchise_team_colors <- c("#C8102E", "#061922", "#008348","#00788C", "#ce1141", 
                     "#860038", "#007dc5", "#0E2240", "#C8102E", "#1D428A",
                     "#ce1141", "#ffc633", "#1D428A", "#552582", "#5D76A9", 
                     "#98002e", "#00471b", "#78be20", "#b6995a", "#F58426",
                     "#EF3B24", "#007dc5", "#006bb6", "#e56020", "#000000", 
                     "#724c9f", "#bac3c9", "#ce1141", "#002B5C", "#002b5c")

# PRE-TRADE CALCULATOR ############################################################

pre_trade_calc <- function(df, league, current_team, traded_to, fppg_col, cap_col){
  
  library("tidyverse")
  current_team <- enquo(current_team)
  traded_to <- enquo(traded_to)
  fppg_col <- enquo(fppg_col)
  cap_col <- enquo(cap_col)
  
  if(league == "legacy"){
  
  tmp_1 <- df %>% 
    rename(current_team = !!(current_team)) %>% 
    filter(current_team != "FA") %>% 
    drop_na(current_team) %>%
    group_by(current_team) %>% 
    top_n(13, wt = !!(fppg_col)) %>% 
    summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
    mutate(avg_fppg = round(avg_fppg, 3))
  
  tmp_2 <- df %>% 
    rename(current_team = !!(current_team)) %>% 
    filter(current_team != "FA") %>% 
    drop_na(current_team) %>%
    group_by(current_team) %>% 
    top_n(13, wt = !!(fppg_col)) %>% 
    summarize(players = n()) %>% 
    filter(current_team != "NA") %>% 
    full_join(tmp_1, by = "current_team") 
  
  tmp_3 <- df %>% 
    rename(current_team = !!(current_team)) %>% 
    filter(current_team != "FA") %>% 
    drop_na(current_team) %>%
    group_by(current_team) %>% 
    top_n(13, wt = !!(fppg_col)) %>% 
    summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
    mutate(cap_left = 114400000 - cap_used)
  
  pre_trade <- full_join(tmp_2, tmp_3, by = "current_team") %>% 
    mutate(team_total = avg_fppg * players * 2.66667) %>% 
    arrange(-team_total) %>%
    rename(team = current_team) %>% 
    select(team, players, avg_fppg, team_total, cap_used, cap_left)
  
  }
  
  if(league == "franchise"){
    
    tmp_1 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA") %>% 
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>% 
      summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
      mutate(avg_fppg = round(avg_fppg, 3))
    
    tmp_2 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA") %>% 
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>% 
      summarize(players = n()) %>% 
      filter(current_team != "NA") %>% 
      full_join(tmp_1, by = "current_team") 
    
    tmp_3 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA") %>% 
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>% 
      summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
      mutate(cap_left = 114400000 - cap_used)
    
    pre_trade <- full_join(tmp_2, tmp_3, by = "current_team") %>% 
      mutate(team_total = avg_fppg * players * 2.66667) %>% 
      arrange(-team_total) %>%
      rename(team = current_team) %>% 
      select(team, players, avg_fppg, team_total, cap_used, cap_left)
    
  }
  
  if(league == "gsd2"){
    
    tmp_1 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA" & current_team != "W (Sat)" & 
               current_team != "W (Thu)" & current_team != "W (Wed)") %>%  
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>% 
      summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
      mutate(avg_fppg = round(avg_fppg, 3))
    
    tmp_2 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA" & current_team != "W (Sat)" & 
               current_team != "W (Thu)" & current_team != "W (Wed)") %>%  
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>% 
      summarize(players = n()) %>% 
      filter(current_team != "NA") %>% 
      full_join(tmp_1, by = "current_team") 
    
    tmp_3 <- df %>% 
      rename(current_team = !!(current_team)) %>% 
      filter(current_team != "FA" & current_team != "W (Sat)" & 
               current_team != "W (Thu)" & current_team != "W (Wed)") %>%  
      drop_na(current_team) %>%
      group_by(current_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>% 
      summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
      mutate(cap_left = 114400000 - cap_used)
    
    pre_trade <- full_join(tmp_2, tmp_3, by = "current_team") %>% 
      mutate(team_total = avg_fppg * players * 2.66667) %>% 
      arrange(-team_total) %>%
      rename(team = current_team) %>% 
      select(team, players, avg_fppg, team_total, cap_used, cap_left)
    
  }
  
  pre_trade
}

# POST-TRADE CALCULATOR ###########################################################

post_trade_calc <- function(df, league, current_team, traded_to, fppg_col, cap_col){
  
  library("tidyverse")
  current_team <- enquo(current_team)
  traded_to <- enquo(traded_to)
  fppg_col <- enquo(fppg_col)
  cap_col <- enquo(cap_col)
  
  if(league == "legacy"){
    
    tmp_4 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(13, wt = !!(fppg_col)) %>%
      summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
      mutate(avg_fppg = round(avg_fppg, 3))  
    
    tmp_5 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(13, wt = !!(fppg_col)) %>%
      summarize(players = n()) %>% 
      filter(new_team != "NA") %>% 
      full_join(tmp_4, tmp_5, by = "new_team")
    
    tmp_6 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(13, wt = !!(fppg_col)) %>%
      summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
      mutate(cap_left = 114400000 - cap_used)
    
    post_trade <- full_join(tmp_5, tmp_6, by = "new_team") %>% 
      mutate(team_total = avg_fppg * players * 2.66667) %>% 
      arrange(-team_total) %>%
      rename(team = new_team) %>% 
      select(team, players, avg_fppg, team_total, cap_used, cap_left)
  }
  
  if(league == "franchise"){
    
    tmp_4 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>%
      summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
      mutate(avg_fppg = round(avg_fppg, 3))  
    
    tmp_5 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>%
      summarize(players = n()) %>% 
      filter(new_team != "NA") %>% 
      full_join(tmp_4, tmp_5, by = "new_team")
    
    tmp_6 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(15, wt = !!(fppg_col)) %>%
      summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
      mutate(cap_left = 114400000 - cap_used)
    
    post_trade <- full_join(tmp_5, tmp_6, by = "new_team") %>% 
      mutate(team_total = avg_fppg * players * 2.66667) %>% 
      arrange(-team_total) %>%
      rename(team = new_team) %>% 
      select(team, players, avg_fppg, team_total, cap_used, cap_left)
  }
  
  if(league == "gsd2"){
    
    tmp_4 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA" & new_team != "W (Sat)" & 
             new_team != "W (Thu)" & new_team != "W (Wed)") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>%
      summarize(avg_fppg = mean(!!(fppg_col), na.rm = TRUE)) %>% 
      mutate(avg_fppg = round(avg_fppg, 3))  
    
    tmp_5 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA" & new_team != "W (Sat)" & 
               new_team != "W (Thu)" & new_team != "W (Wed)") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>%
      summarize(players = n()) %>% 
      filter(new_team != "NA") %>% 
      full_join(tmp_4, tmp_5, by = "new_team")
    
    tmp_6 <- df %>% 
      rename(new_team = !!(traded_to)) %>%
      filter(new_team != "FA" & new_team != "W (Sat)" & 
               new_team != "W (Thu)" & new_team != "W (Wed)") %>% 
      drop_na(new_team) %>%
      group_by(new_team) %>% 
      top_n(12, wt = !!(fppg_col)) %>%
      summarize(cap_used = sum(!!(cap_col), na.rm = TRUE)) %>% 
      mutate(cap_left = 114400000 - cap_used)
    
    post_trade <- full_join(tmp_5, tmp_6, by = "new_team") %>% 
      mutate(team_total = avg_fppg * players * 2.66667) %>% 
      arrange(-team_total) %>%
      rename(team = new_team) %>% 
      select(team, players, avg_fppg, team_total, cap_used, cap_left)
  }
  
  post_trade
  
}

# PRE-TRADE PLOTTER ############################################################

pre_trade_plot <- function(pre_trade_df, var, league, plotly){
  
  library("ggplot2")
  library("plotly")
  
  if(var == "team_total"){
    
    if(league == "legacy"){
      
      pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -team_total), 
                                                 y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "LD Team Rankings - Total Points - Before Trade", fill = "Team") +
        scale_fill_manual(values=nba_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "franchise"){
      
      pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -team_total), 
                                                 y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "Franchise Team Rankings - Total Points - Before Trade", fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "gsd2"){
      
      pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -team_total), 
                                                 y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "GSD2 Team Rankings - Total Points - Before Trade", fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
  }
  
  if(var == "avg_fppg"){
    
    if(league == "legacy"){
      
      pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                 y=avg_fppg, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "LD Team Rankings - Average FPPG/Player - Before Trade", fill = "Team") +
        scale_fill_manual(values=nba_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "franchise"){
      
      pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                 y=avg_fppg, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "Franchise Team Rankings - Average FPPG/Player - Before Trade", 
             fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
      
      if(league == "gsd2"){
        
        pre_trade_plot <- ggplot(pre_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                   y=avg_fppg, fill=team)) +
          geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
          geom_text(aes(label = players), size = 5, 
                    colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
          labs(title = "GSD2 Team Rankings - Average FPPG/Player - Before Trade", fill = "Team") +
          scale_fill_manual(values=franchise_team_colors) +
          theme(axis.title.x=element_blank(), 
                axis.title.y=element_blank(),
                axis.text.x=element_text(size=16),
                title=element_text(size=20), 
                legend.position = "none") 
      }
      
  }
  
  if(plotly == TRUE){
    ggplotly(pre_trade_plot)
  } else {
    pre_trade_plot
  }
  
}

# POST-TRADE PLOTTER ############################################################


post_trade_plot <- function(post_trade_df, league, var, plotly){
  
  library("ggplot2")
  library("plotly")
  
  if(var == "team_total"){
    
    if(league == "legacy"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -team_total), 
                                                   y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "LD Team Rankings - Total Points - After Trade", fill = "Team") +
        scale_fill_manual(values=nba_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "franchise"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -team_total), 
                                                   y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "Franchise Team Rankings - Total Points - After Trade", fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "gsd2"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -team_total), 
                                                   y=team_total, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "GSD2 Team Rankings - Total Points - After Trade", fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
  }
  
  if(var == "avg_fppg"){
    
    if(league == "legacy"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                   y=avg_fppg, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "LD Team Rankings - Average FPPG/Player - After Trade", fill = "Team") +
        scale_fill_manual(values=nba_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "franchise"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                   y=avg_fppg, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "Franchise Team Rankings - Average FPPG/Player - After Trade", 
             fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    if(league == "gsd2"){
      
      post_trade_plot <- ggplot(post_trade_df, aes(x=reorder(team, -avg_fppg), 
                                                   y=avg_fppg, fill=team)) +
        geom_bar(stat="identity", width=0.6, colour="black") + theme_bw() +  
        geom_text(aes(label = players), size = 5, 
                  colour="white", fontface="bold", hjust = 0.5, vjust = 5) +
        labs(title = "GSD2 Team Rankings - Average FPPG/Player - After Trade", 
             fill = "Team") +
        scale_fill_manual(values=franchise_team_colors) +
        theme(axis.title.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.x=element_text(size=16),
              title=element_text(size=20), 
              legend.position = "none") 
    }
    
    
  }
  
  
  if(plotly == TRUE){
    ggplotly(post_trade_plot)
  } else {
    post_trade_plot
  }
  
}

