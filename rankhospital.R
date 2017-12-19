rankhospital <- function(state, outcome, num = "best") {
        
        data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        state.data <- subset(data, State == state)
        
        state.names <- names(split(data, data$State))
        
        if (!(state %in% state.names)){
                stop("invalid state")
        }
        
        outcome.colnum <- numeric()
        
        if(outcome == "heart attack") outcome.colnum <- c(11)
        else if(outcome == "heart failure") outcome.colnum <- c(17)
        else if(outcome == "pneumonia") outcome.colnum <- c(23)
        else {
                stop("invalid outcome")
        }
        
        state.outcome.num <- as.numeric(state.data[, outcome.colnum])
        
        state.hospital.name <- state.data[, 2]
        
        order.vector <- order(state.outcome.num, state.hospital.name, na.last = NA)
        
        sorted.data.fram <- state.data[order.vector,c(2,outcome.colnum)]
        
        if(class(num) == "numeric") {
                return(sorted.data.fram[num, 1])
        } else if (num == "best") {
                return(sorted.data.fram[1,1])
        } else if (num == "worst") {
                return(sorted.data.fram[length(order.vector), 1])
        }
        
        head(sorted.data.fram)

}