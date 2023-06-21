process_RWC_data <- function(data) {
  data <- data %>%
    mutate(date = 2023)
  
  data <- left_join(data, win_percent_df, by = c("date" = "WC_cycle", "home_team" = "team")) %>%
    mutate(HT_win_percent = win_percentage) %>%
    select(-win_percentage) %>%
    left_join(win_percent_df, by = c("date" = "WC_cycle", "away_team" = "team")) %>%
    mutate(AT_win_percent = win_percentage) %>%
    select(-win_percentage)
  
  data <- data %>%
    left_join(June_12_rankings, by = c("home_team" = "Team")) %>%
    select(-starts_with("Team")) %>%
    rename(HT_rank = Ranking) %>%
    left_join(June_12_rankings, by = c("away_team" = "Team")) %>%
    select(-starts_with("Team")) %>%
    rename(AT_rank = Ranking)
  
  data <- data %>%
    left_join(WC_titles, by = c("date" = "Date", "home_team" = "Team")) %>%
    mutate(HT_titles = ifelse(!is.na(value), value, 0)) %>%
    select(-value) %>%
    left_join(WC_titles, by = c("date" = "Date", "away_team" = "Team")) %>%
    mutate(AT_titles = ifelse(!is.na(value), value, 0)) %>%
    select(-value)
  
  data <- data %>% 
    rename(year = date) %>% 
    mutate(points_diff = 0) %>% 
    select("year", "home_team", "away_team", "HT_win_percent", "AT_win_percent", "HT_rank", "AT_rank", "neutral", "points_diff", "HT_titles", "AT_titles")
  
  return(data)
}
