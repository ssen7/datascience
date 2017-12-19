pollutantmean <- function(directory, pollutant, id = 1:332){
  files <- list.files(directory, pattern = "*.csv",full.names=TRUE)
  
  polmeanvector <- numeric()
  
  for (i in id) {
    df <- read.csv(files[i])
    df.pollutant <- df[,pollutant]
    polmeanvector <- c(polmeanvector, df.pollutant)
  }
  
  mean(polmeanvector, na.rm = T)
}