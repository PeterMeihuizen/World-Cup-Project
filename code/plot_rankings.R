plot_rankings <- function(data, teams, colors) {
  # Reshape the data to long format
  rankings_long <- tidyr::pivot_longer(data, cols = -date, names_to = "Team", values_to = "Ranking")
  
  # Convert date column to date type
  rankings_long$date <- as.Date(rankings_long$date)
  
  # Convert ranking column to numeric type
  rankings_long$Ranking <- as.numeric(rankings_long$Ranking)
  
  # Filter the data for the selected teams
  rankings_filtered <- rankings_long %>%
    mutate(Team = as.character(Team)) %>%
    filter(Team %in% teams)
  
  # Plot the rankings for the selected teams
  ggplot(rankings_filtered, aes(x = date, y = Ranking, color = Team)) +
    geom_line() +
    labs(x = "Date", y = "Ranking", title = "Ranking of Teams Over Time") +
    scale_color_manual(values = colors) +
    theme_minimal()
}








