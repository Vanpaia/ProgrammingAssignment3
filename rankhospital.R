rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  help_function <- function(x) {
    data_state <- subset(data, State == state)
    data_state[data_state == "Not Available"] = NA_character_
    data_state[, x] <- as.numeric(data_state[, x])
    good <- complete.cases(data_state[, x])
    data_state <- data_state[good, ]
    ranking <- data_state[order(data_state[, x], data_state[, 2]), ]
    if(num == "best") {
      answer <- ranking[1, 2]
    } else if(num == "worst") {
      last <- nrow(ranking)
      answer <- ranking[last, 2]
    } else if(num > nrow(ranking)) {
      answer <- NA_character_
    } else {
      answer <- ranking[num, 2]
    }
    answer
  }
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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