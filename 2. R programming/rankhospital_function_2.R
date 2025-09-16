rankhospital <- function(state, outcome, num = "best") {
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
  
  outcome_df$rate <- as.numeric(outcome_df[, target_outcome_name])
  
  outcome_df <- data.frame(outcome_df$Hospital.Name, outcome_df$rate)
  outcome_df <- outcome_df[order(outcome_df$outcome_df.rate, outcome_df$outcome_df.Hospital.Name),]
  
  if (num == "worst") {
    outcome_df <- outcome_df[!is.na(outcome_df$outcome_df.rate),]
    return(tail(outcome_df$outcome_df.Hospital.Name, 1))
  } else if (num == "best") {
    return(outcome_df$outcome_df.Hospital.Name[1])
  }
  return(outcome_df$outcome_df.Hospital.Name[num])
  
  
}