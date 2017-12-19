source("complete.R")
corr <- function(directory, threshold = 0){
  
  files <- list.files(directory, pattern = "*.csv",full.names=TRUE)
  
  id = 1:332
  
  corrvector = numeric()
  
  for (i in id) {
    df <- read.csv(files[i])
    df.complete <- complete(directory, i)
    ##df.complete.numeric <- df.complete[,2 ]
    
    if(df.complete[,2] > threshold) {
      
      corrvectortemp <- cor(df[,2], df[, 3], use = "pairwise.complete.obs")
      
      corrvector <- c(corrvector, corrvectortemp)
    }
  }
  
  corrvector
  
}