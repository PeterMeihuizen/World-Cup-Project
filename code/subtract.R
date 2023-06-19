subtract <- function(data, home_score, away_score) {
  points_diff <- data[[home_score]] - data[[away_score]]
  return(points_diff)
}