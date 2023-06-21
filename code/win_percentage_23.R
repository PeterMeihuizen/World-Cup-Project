win_percentage_23 <- function(data){
  data %>%
    summarize(
      total_matches = sum(home_team == Team | away_team == Team),
      total_wins = sum(Winner == Winner)
    ) %>%
    mutate(
      win_percentage = ifelse(!is.na(WC_winner), total_wins / total_matches * 100, NA)
    )
  
}