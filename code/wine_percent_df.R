win_percentages_SA <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "South Africa"),
    total_matches = sum(home_team == "South Africa"| away_team == "South Africa"),
    win_percentage = total_wins / total_matches * 100,
  ) 

win_percentages_NZ <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "New Zealand"),
    total_matches = sum(home_team == "New Zealand"| away_team == "New Zealand"),
    win_percentage = total_wins / total_matches * 100,
  ) 

win_percentages_AUS <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Australia"),
    total_matches = sum(home_team == "Australia"| away_team == "Australia"),
    win_percentage = total_wins / total_matches * 100,
  ) 

win_percentages_ARG <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Argentina"),
    total_matches = sum(home_team == "Argentina" | away_team == "Argentina"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_IRE <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Ireland"),
    total_matches = sum(home_team == "Ireland" | away_team == "Ireland"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_ITA <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Italy"),
    total_matches = sum(home_team == "Italy" | away_team == "Italy"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_FRA <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "France"),
    total_matches = sum(home_team == "France" | away_team == "France"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_SCO <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Scotland"),
    total_matches = sum(home_team == "Scotland" | away_team == "Scotland"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_ENG <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "England"),
    total_matches = sum(home_team == "England" | away_team == "England"),
    win_percentage = total_wins / total_matches * 100
  )

win_percentages_WAL <- df %>%
  group_by(WC_cycle) %>% 
  summarize(
    total_wins = sum(Winner == "Wales"),
    total_matches = sum(home_team == "Wales" | away_team == "Wales"),
    win_percentage = total_wins / total_matches * 100
  )

# Combine the win percentages into a single data frame
win_percent_df <- bind_rows(
  win_percentages_SA %>% mutate(team = "South Africa"),
  win_percentages_NZ %>% mutate(team = "New Zealand"),
  win_percentages_AUS %>% mutate(team = "Australia"),
  win_percentages_ARG %>% mutate(team = "Argentina"),
  win_percentages_IRE %>% mutate(team = "Ireland"),
  win_percentages_ITA %>% mutate(team = "Italy"),
  win_percentages_FRA %>% mutate(team = "France"),
  win_percentages_SCO %>% mutate(team = "Scotland"),
  win_percentages_ENG %>% mutate(team = "England"),
  win_percentages_WAL %>% mutate(team = "Wales")
)








