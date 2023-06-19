win_loss_bar <- function(data) {
  colors <- c("Australia" = "yellow", "New Zealand" = "black", "England" = "white", "South Africa" = "green")
  
  data$WC_cycle <- factor(data$WC_cycle, levels = unique(data$WC_cycle))
  
  ggplot(data, aes(x = WC_cycle, y = win_percentage, fill = WC_winner)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colors) +
    labs(x = "World Cup Cycle", y = "Win Percentage", title = "Win Percentage by World Cup Cycle") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "lightblue"))
}


