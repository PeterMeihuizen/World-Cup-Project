generate_rankings <- function(data) {
  rankings <- bind_rows(data)
  rankings <- rankings[-c(1:2), ]
  rankings <- select(rankings, matches("^(date|New_Zealand|Australia|South_Africa|France|England|Ireland|Wales|Scotland|Italy|Argentina)$"))
  
  return(rankings)
}
