win_percentage_23 <- function(data){
  data %>%
    group_by(Winner, Loser) %>%
    summarize(
      total_matches = sum(home_team == Winner | away_team == WC_winner),
      total_wins = sum(Winner == Winner)
    ) %>%
    mutate(
      win_percentage = ifelse(!is.na(WC_winner), total_wins / total_matches * 100, NA)
    )
  
}