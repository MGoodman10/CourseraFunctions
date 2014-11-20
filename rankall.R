rankall <- function(outcome, num="best") {
        path <- paste(getwd(),"/", "hospdata", sep="")
        directory <- file.path(path, "outcome-of-care-measures.csv")
        raw.data <- read.csv(directory, header=TRUE)
        
        outcome.lst <- c('heart attack', 'heart failure', 'pneumonia')
        outcome.idx <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
                         'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                         'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
        ## outcome.idx <- c('from.Heart.Attack',
        ##                 'from.Heart.Failure',
        ##                 'from.Pneumonia')
        outcome.df <- data.frame(outcome.lst, outcome.idx)
        col.idx <- as.character(outcome.df$outcome.idx[
                                outcome.df$outcome.lst==outcome])
        
        if(!is.element(outcome, outcome.lst)) {stop("invalid outcome")}
        
        subset.df <- subset(raw.data, select=c("Hospital.Name", "State", col.idx))
        names(subset.df) <- c("hosp.name","state.col", "mortality")
                
        output.frame <- NULL
        output.frame <- data.frame(hospital=character(),state=character())
        
        ## state.azz <- c("AK", "AL", "AZ")
        state.list <- unique(raw.data$State)
        for (i in state.list) { 
                state <- i
                
                state.data <- subset(subset.df, state.col==state)
                       
                num.state <- nrow(state.data)
                if(num.state==0) { bst.hospital <- NA
                                  return(bst.hospital)}
                               
                state.data$mortality[state.data$mortality=="Not Available"] <- NA
                bad <- is.na(state.data$mortality)
                good.data <- state.data[!bad, ]
                good.data$mortality <- as.numeric(as.character(good.data$mortality))
                               
                order.pop <- order(good.data$mortality, good.data$hosp.name)
                result.data <- good.data[order.pop, ]
                               
                if(num=='best') {hosp.rank <- 1}
                else if(num=='worst') {hosp.rank <- nrow(good.data)}
                else {hosp.rank <- num}
                               
                if(hosp.rank > nrow(good.data)) { bst.hospital <- NA }
                else {bst.hospital <- 
                              as.character(result.data$hosp.name[hosp.rank])}
                               
                new.frame <- data.frame(hospital=bst.hospital, state=i)
                output.frame <- rbind(output.frame, new.frame)
        }
        
        output.frame$hospital <- as.character(output.frame$hospital)
        output.frame$state <- as.character(output.frame$state)
        order.output <- order(output.frame$state)
        final.output <- output.frame[order.output, ]
        rownames(final.output) <- final.output[,2]
        ##print(output.frame)
        final.output        
}