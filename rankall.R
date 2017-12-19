rankall <- function(outcome, num = "best") {
        
        data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        state.names <- names(split(data, data$State))
        
        outcome.colnum <- numeric()
        
        if(outcome == "heart attack") outcome.colnum <- c(11)
        else if(outcome == "heart failure") outcome.colnum <- c(17)
        else if(outcome == "pneumonia") outcome.colnum <- c(23)
        else {
                stop("invalid outcome")
        }
        
        ranked.hospital.name <- numeric()
        
        for( state in state.names){
                state.data <- subset(data, State == state)
                
                state.outcome.num <- as.numeric(state.data[, outcome.colnum])
                
                state.hospital.name <- state.data[, 2]
                
                order.vector <- order(state.outcome.num, state.hospital.name, na.last = NA)
                
                sorted.data.fram <- state.data[order.vector,c(2,outcome.colnum)]
                
                if(class(num) == "numeric") {
                        ranked.hospital.name <- c(ranked.hospital.name,sorted.data.fram[num, 1])
                } else if (num == "best") {
                        ranked.hospital.name <- c(ranked.hospital.name,sorted.data.fram[1,1])
                } else if (num == "worst") {
                        ranked.hospital.name <- c(ranked.hospital.name,sorted.data.fram[length(order.vector), 1])
                }
        }
        
        final.ans <- data.frame("hospital" = ranked.hospital.name, "state" = state.names)
        
        final.ans
        
}