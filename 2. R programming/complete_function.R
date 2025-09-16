directory <- setwd("C:\\Users\\PC\\Desktop\\specdata")

complete <- function(directory, id = 1:332) {
  filenames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs_complete <- numeric()
  
  for (i in id) {
    data_frame_from_csv <- read.csv(filenames[i])
    num_complete_rows <- nrow(na.omit(data_frame_from_csv))
    nobs_complete <- c(nobs_complete, num_complete_rows)
  }
  result_df <- data.frame(id = id, nobs = nobs_complete)
  return(result_df)
}