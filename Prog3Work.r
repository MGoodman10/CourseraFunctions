prog3 <- function(state, outcome) {

data(airquality)
print(airquality)

outcome.lst <- c('heart attack', 'heart failure', 'pneumonia')
outcome.idx <- c('Solar.R', 'Wind', 'Temp')
outcome.df <- data.frame(outcome.lst, outcome.idx)
col.idx <- as.character(outcome.df$outcome.idx[outcome.df$outcome.lst==outcome])

subset.df <- subset(airquality, select=c("Ozone", "Month", col.idx))
names(subset.df) <- c("hosp.name","state.col", "mortality")

state.data <- subset(subset.df, state.col==state)
print(state.data)
bad <- is.na(state.data$mortality)
good.data <- state.data[!bad, ]

order.pop <- order(good.data$mortality)
result.data <- good.data[order.pop, ]
print(result.data)

bst.hospital <- as.character(result.data$hosp.name[1])
print(bst.hospital)
class(bst.hospital)

}