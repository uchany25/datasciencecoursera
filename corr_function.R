directory <- setwd("C:\\Users\\PC\\Desktop\\specdata")

corr <- function(directory, threshold = 0) {
  filenames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  correlations <- numeric()
  
  for(all_file in filenames) {
    df <- read.csv(all_file)
    df <- na.omit(df)        
    col_num <- nrow(df)
    
    if(col_num > threshold) {
      correlation <-  cor(df$sulfate, df$nitrate)
      correlations <- c(correlations, correlation)
    }
    
  }
  
  return(correlations)
}
