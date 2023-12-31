plot_rankings <- function(data, title = "Ranking of Teams Over Time") {
  # Reshape the data to long format
  rankings_long <- tidyr::pivot_longer(data, cols = -date, names_to = "team", values_to = "ranking")
  
  # Convert date column to date type
  rankings_long$date <- as.Date(rankings_long$date)
  
  # Convert ranking column to numeric type
  rankings_long$ranking <- as.numeric(rankings_long$ranking)
  
  # Sort the data by date and team
  rankings_long <- rankings_long[order(rankings_long$date, rankings_long$team), ]
  
  # Plot the time series graph with y-axis formatting
  ggplot(rankings_long, aes(x = date, y = ranking, color = team)) +
    geom_line() +
    labs(x = "Date", y = "Ranking", title = title) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(1, max(rankings_long$ranking), 1))
}













