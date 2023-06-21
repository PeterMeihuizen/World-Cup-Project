ind_wins_23 <- function(data) {
  data %>%
    group_by(team1 = pmin(home_team, away_team), team2 = pmax(home_team, away_team)) %>%
    summarize(
      wins = sum(Winner == team1),
      losses = sum(Loser == team1)
    ) %>%
    ungroup() %>%
    mutate(
      total_matches = wins + losses,
      win_percentage = wins / total_matches * 100
    ) %>%
    select(team1, team2, win_percentage) %>%
    pivot_wider(names_from = team2, values_from = win_percentage) %>%
    replace(is.na(.), 0)
}

# Usage example:
win_matrix <- df %>%
  ind_wins_23() %>% 
  filter(date > "2019-11-02")






  
  
