rankall <- function(outcome, num = "best") {
  outcome_df <- read.csv("C:\\Users\\PC\\Desktop\\rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character")
  rankall_hospital <- data.frame(
    hospital = character(0),
    state = character(0),
    stringsAsFactors = FALSE
  )
  
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
    outcome_df$rate <- as.numeric(outcome_df[, target_outcome_name])
  })
  
  unique_state <- unique(outcome_df$State)
  unique_state <- sort(unique_state)
  
  
  for (i in unique_state) {
    
    for_df <- outcome_df[outcome_df$State == i,]
    for_df <- for_df[!is.na(for_df$rate), ]
    for_df <- for_df[order(for_df$rate, for_df$Hospital.Name), ]
    hospital_name_for_state <- NA_character_
    
    
    
    if (nrow(for_df) > 0) {
      if (num == "best") {
        hospital_name_for_state <- for_df$Hospital.Name[1]
      } else if (num == "worst") {
        hospital_name_for_state <- tail(for_df$Hospital.Name, 1) 
      } else if (is.numeric(num)) {
        if (num <= nrow(for_df)) {
          hospital_name_for_state <- for_df$Hospital.Name[num]
        }
      } else {
        stop("invalid num value")
      }
    }
    
    new_row <- data.frame(hospital = hospital_name_for_state, state = i, stringsAsFactors = FALSE)
    rankall_hospital <- rbind(rankall_hospital, new_row)
  }
  
  return(rankall_hospital)
}