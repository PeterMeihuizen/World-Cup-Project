WC_games <- function(data) {
  NZ_87 <- data %>% 
    filter(year == "1987", world_cup == TRUE, Winner == "New Zealand") %>% 
    select(year, Winner, Loser, points_diff)
  
  Aus_91 <- data %>% 
    filter(year == "1991", world_cup == TRUE, Winner == "Australia") %>% 
    select(year, Winner, Loser, points_diff)
  
  SA_95 <- data %>% 
    filter(year == "1995", world_cup == TRUE, Winner == "South Africa") %>% 
    select(year, Winner, Loser, points_diff)
  
  Aus_99 <- data %>% 
    filter(year == "1999", world_cup == TRUE, Winner == "Australia") %>% 
    select(year, Winner, Loser, points_diff)
  
  Eng_03 <- data %>% 
    filter(year == "2003", world_cup == TRUE, Winner == "England") %>% 
    select(year, Winner, Loser, points_diff)
  
  SA_07 <- data %>% 
    filter(year == "2007", world_cup == TRUE, Winner == "South Africa") %>% 
    select(year, Winner, Loser, points_diff)
  
  NZ_11 <- data %>% 
    filter(year == "2011", world_cup == TRUE, Winner == "New Zealand") %>% 
    select(year, Winner, Loser, points_diff)
  
  NZ_15 <- data %>% 
    filter(year == "2015", world_cup == TRUE, Winner == "New Zealand") %>% 
    select(year, Winner, Loser, points_diff)
  
  SA_19 <- data %>% 
    filter(year == "2019", world_cup == TRUE, Winner == "South Africa") %>% 
    select(year, Winner, Loser, points_diff)
  
  # Combine all the groups into one dataset
  all_games <- bind_rows(NZ_87, Aus_91, SA_95, Aus_99, Eng_03, SA_07, NZ_11, NZ_15, SA_19)
  
  return(all_games)
}