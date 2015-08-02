best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  min <- NULL
  
  help_function <- function(x) {
    data_state <- subset(data, State == state)
    data_state[data_state == "Not Available"] = NA_character_
    data_state[, x] <- as.numeric(data_state[, x])
    good <- complete.cases(data_state[, x])
    data_state <- data_state[good, ]
    min <- data_state$Hospital.Name[which.min(data_state[, x])]
    min
  }
  
  ## Check that state and outcome are valid
  if(!is.element(state, data$State)) {
    stop("invalid state")
  } else if(outcome == "heart attack") {
    help_function(11)
  } else if(outcome == "heart failure") {
    help_function(17)
  } else if(outcome == "pneumonia") {
    help_function(23)
  } else {
    stop("invalid outcome")
  }
}