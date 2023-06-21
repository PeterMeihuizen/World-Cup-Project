ind_total_wins <- function(data, Teams) {
  result <- data.frame()
  
  for (team in Teams) {
    team_result <- ind_win_percentage(data, Teams = team, opposition = setdiff(Teams))
    result <- bind_rows(result, team_result)
  }
  
  result <- result %>%
    filter(!is.na(win_percentage), !is.na(WC_cycle)) %>%
    rename(Team_1 = team, Team_2 = opposition, Total_Matches = total_matches, Total_Wins = total_wins)
  
  return(result)
}

ind_wins <- function(data, Teams) {
  result <- data.frame()
  
  for (team in Teams) {
    team_result <- ind_win_percentage(data, Team = team, opposition = setdiff(Teams, team))
    result <- bind_rows(result, team_result)
  }
  
  result <- result %>%
    filter(!is.na(win_percentage), !is.na(WC_cycle)) %>%
    rename(Winner = team, year = WC_cycle)
  
  return(result)
}







