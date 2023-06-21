ind_win_percentage <- function(data, Teams) {
  result <- data %>%
    filter(Winner %in% Teams, Loser %in% Teams) %>%
    group_by(WC_cycle, Winner, Loser) %>%
    summarize(
      total_matches = n(),
      total_wins = sum(Winner == WC_winner),
      win_percentage = total_wins / total_matches * 100
    ) %>%
    ungroup()
  
  return(result)
}

ind_wins <- function(data, Teams) {
  result <- data.frame()
  
  for (team in Teams) {
    opposition_teams <- setdiff(Teams, team)
    team_result <- ind_win_percentage(data, Teams = c(team, opposition_teams))
    result <- bind_rows(result, team_result)
  }
  
  return(result)
}



