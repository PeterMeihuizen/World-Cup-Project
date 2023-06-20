ind_win_percentage <- function(data, opposition) {
  data %>%
    filter(Winner == opposition | Loser == opposition) %>%
    group_by(WC_winner, WC_cycle) %>%
    summarize(
      team = opposition,
      total_matches = sum(home_team == WC_winner | away_team == WC_winner),
      total_wins = sum(Winner == WC_winner)
    ) %>%
    mutate(
      win_percentage = ifelse(!is.na(WC_winner), total_wins / total_matches * 100, NA)
    )
}

ind_wins <- function(data) {
  NZ <- ind_win_percentage(data, opposition = "New Zealand")
  AUS <- ind_win_percentage(data, opposition = "Australia") 
  ARG <- ind_win_percentage(data, opposition = "Argentina") 
  SA <- ind_win_percentage(data, opposition = "South Africa") 
  IRE <- ind_win_percentage(data, opposition = "Ireland") 
  ITA <- ind_win_percentage(data, opposition = "Italy")
  ENG <- ind_win_percentage(data, opposition = "England")
  SCO <- ind_win_percentage(data, opposition = "Scotland") 
  WAL <- ind_win_percentage(data, opposition = "Wales")
  FRA <- ind_win_percentage(data, opposition = "France") 
 
  result <- bind_rows(NZ, AUS, ARG, SA, IRE, ITA, ENG, SCO, WAL, FRA)
  
  result %>% filter(!is.na(win_percentage), WC_winner != team)

}
