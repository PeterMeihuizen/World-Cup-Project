win_percentage <- function(data){
  data %>%
    group_by(WC_winner, WC_cycle) %>%
    summarize(
      total_matches = sum(home_team == WC_winner | away_team == WC_winner),
      total_wins = sum(Winner == WC_winner)
    ) %>%
    mutate(
      win_percentage = ifelse(!is.na(WC_winner), total_wins / total_matches * 100, NA)
    )
  
}
  