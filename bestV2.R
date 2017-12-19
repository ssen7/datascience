bestV2 <- function(state, outcome, num = "best") {
        
        data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        state.data <- subset(data, State == state)
        
        outcome.colnum <- numeric()
        
        if(outcome == "heart attack") outcome.colnum <- c(11)
        else if(outcome == "heart failure") outcome.colnum <- c(17)
        else if(outcome == "pneumonia") outcome.colnum <- c(23)
        else {
                stop("invalid outcome")
        }
        
        
        state.data[(order(as.numeric(state.data[, outcome.colnum])))[1],2]
        
        
}