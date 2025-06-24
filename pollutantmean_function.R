directory <- setwd("C:\\Users\\PC\\Desktop\\specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  all_data <- data.frame()
  for (i in id) {
    df <- read.csv(filenames[i])
    all_data <- rbind(all_data, df)
  }
  
  if(pollutant %in% names(all_data)) {
    need_data <- all_data[[pollutant]]
    mean_value <- mean(need_data, na.rm = TRUE)
    return(mean_value)
  } else {
    warning("No data")
    return(NA)
  }
}

cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))