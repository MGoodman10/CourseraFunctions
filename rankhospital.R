best <- function(state, outcome) {
        path <- paste(getwd(),"/", "hospdata", sep="")
        directory <- file.path(path, "outcome-of-care-measures.csv")
        raw.data <- read.csv(directory, header=TRUE)
        
        outcome.lst <- c('heart attack', 'heart failure', 'pneumonia')
        outcome.idx <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
                         'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                         'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
        ##outcome.idx <- c('from.Heart.Attack',
        ##                 'from.Heart.Failure',
        ##                 'from.Pneumonia')
        outcome.df <- data.frame(outcome.lst, outcome.idx)
        col.idx <- as.character(outcome.df$outcome.idx[
                                outcome.df$outcome.lst==outcome])
        
        if(!is.element(outcome, outcome.lst)) {stop("invalid outcome")}
        if(!is.element(state, state.abb)) { stop("invalid state")}
        
        subset.df <- subset(raw.data, select=c("Hospital.Name", "State", col.idx))
        names(subset.df) <- c("hosp.name","state.col", "mortality")
        state.data <- subset(subset.df, state.col==state)
        
        num <- nrow(state.data)
        if(num==0) { bst.hospital <- NULL
                      return(bst.hospital)}
                
        state.data$mortality[state.data$mortality=="Not Available"] <- NA
        bad <- is.na(state.data$mortality)
        good.data <- state.data[!bad, ]
        good.data$mortality <- as.numeric(as.character(good.data$mortality))
                
        order.pop <- order(good.data$mortality, good.data$hosp.name)
        result.data <- good.data[order.pop, ]
                
        bst.hospital <- as.character(result.data$hosp.name[1])
        bst.hospital
        
                
}