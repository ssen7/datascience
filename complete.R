complete <- function(directory, id = 1:332){
  
  files <- list.files(directory, pattern = "*.csv",full.names=TRUE)
  
  completevector <- numeric()
  
  for (i in id) {
    df <- read.csv(files[i])
    df.complete <- subset(df, !is.na(sulfate) & !is.na(nitrate))
    completevector <- c (completevector, nrow(df.complete))
  }
  
  result <- data.frame("id" = id, "nobs" = completevector)
  result
  
}