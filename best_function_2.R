best <- function(state, outcome) {
  outcome_df <- read.csv("C:\\Users\\PC\\Desktop\\rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(state %in% outcome_df$State)) {
    stop("invalid state")
  } 
  
  outcome_df <- outcome_df[outcome_df$State == state, ]
  
  target_outcome <- c(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  if (!(outcome %in% names(target_outcome))) {
    stop("invalid outcome")
  } 
  
  
  
  target_outcome_name <- target_outcome[outcome]
  
  suppressWarnings({
    outcome_df[, target_outcome_name] <- as.numeric(outcome_df[, target_outcome_name])
  })
  
  min_rate <- min(as.numeric(outcome_df[, target_outcome[outcome]]), na.rm = TRUE)
  best_hospital_df <- outcome_df[outcome_df[, target_outcome_name] == min_rate, ]
  best_hospital <- sort(best_hospital_df$Hospital.Name)[1]
  return(best_hospital)
}