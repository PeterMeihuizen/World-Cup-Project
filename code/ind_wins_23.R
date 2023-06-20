ind_wins_23 <- function(data) {
  home_wins <- data %>%
    group_by(home_team, away_team) %>%
    summarize(home_wins = sum(Winner == home_team),
              home_matches = n())
  
  away_wins <- data %>%
    group_by(home_team, away_team) %>%
    summarize(away_wins = sum(Winner == away_team),
              away_matches = n())
  
  combined_wins <- home_wins %>%
    left_join(away_wins, by = c("home_team", "away_team")) %>%
    mutate(total_wins = coalesce(home_wins, 0) + coalesce(away_wins, 0),
           total_matches = coalesce(home_matches, 0) + coalesce(away_matches, 0)) %>%
    mutate(win_percentage = total_wins / total_matches * 100) %>%
    select(home_team, away_team, win_percentage) %>%
    pivot_wider(names_from = home_team, values_from = win_percentage, values_fill = NA)
  
  return(combined_wins)
}






  
  
