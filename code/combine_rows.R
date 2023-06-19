combine_rows <- function(data, winner, cycle1, cycle2) {
  combined <- data %>%
    filter(WC_winner == winner & (WC_cycle == cycle1 | WC_cycle == cycle2)) %>%
    summarize(
      total_wins_combined = sum(total_wins),
      total_matches_combined = sum(total_matches),
      win_percentage_combined = total_wins_combined / total_matches_combined * 100
    ) %>%
    mutate(
      WC_winner = winner,
      WC_cycle = as.character(cycle2)
    )
  
  updated_data <- data %>%
    filter(!(WC_winner == winner & (WC_cycle == cycle1 | WC_cycle == cycle2))) %>%
    mutate(WC_cycle = as.character(WC_cycle)) %>%
    bind_rows(combined) %>%
    mutate(
      total_matches = ifelse(WC_winner == winner & WC_cycle == cycle2, total_matches_combined, total_matches),
      total_wins = ifelse(WC_winner == winner & WC_cycle == cycle2, total_wins_combined, total_wins),
      win_percentage = ifelse(WC_winner == winner & WC_cycle == cycle2, win_percentage_combined, win_percentage)
    ) %>%
    select(-total_wins_combined, -total_matches_combined, -win_percentage_combined)
  
  return(updated_data)
}


