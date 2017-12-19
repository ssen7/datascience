best <- function(state, outcome) {
        
        data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        split.data <- split(data, data$State)
        
        state.data <- split.data[state]
        
        sorted <- numeric()
        
        if(outcome == "heart attack") {
                outcome.data <- state.data[[1]][11]
                sorted <- order(as.numeric(unlist(outcome.data, use.names = F)))
        } else if (outcome == "heart failure") {
                outcome.data <- state.data[[1]][17]
                sorted <- order(as.numeric(unlist(outcome.data, use.names = F)))
        } else if (outcome == "pneumonia") {
                outcome.data <- state.data[[1]][23]
                sorted <- order(as.numeric(unlist(outcome.data, use.names = F)))
        }
        
        state.data.frame <- as.data.frame(state.data[1])
        state.data.frame[sorted[1], 2]
        
        
}