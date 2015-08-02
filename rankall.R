rankall <- function(outcome, num = "best") {
  ## Read outcome data
  raw_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- raw_data[, c(2, 7, 11, 17, 23)]
  
  hospital <- character()
  state <- character()
  
  
  help_function <- function(x) {
    split_data <- split(data, data$State)
    for(i in 1:54) {
      split_data[[i]][split_data[[i]] == "Not Available"] = NA_character_
      split_data[[i]][, x] <- as.numeric(split_data[[i]][, x])
      good <- complete.cases(split_data[[i]][, x])
      split_data[[i]] <- split_data[[i]][good, ]
      ranking <- split_data[[i]][order(split_data[[i]][, x], split_data[[i]][, 1]), ]
      if(num == "best") {
        hospital_name <- ranking[1, 1]
      } else if(num == "worst") {
        last <- nrow(ranking)
        hospital_name <- ranking[last, 1]
      } else if(num > nrow(ranking)) {
        hospital_name <- NA_character_
      } else {
        hospital_name <- ranking[num, 1]
      }
      hospital <- c(hospital, hospital_name)
      state <- c(state, split_data[[i]][1,2])
    }
    final <- data.frame(hospital, state)
    print(final)
  }
  
  if(outcome == "heart attack"){
    help_function(3)
  } else if(outcome == "heart failure") {
    help_function(4)
  } else if(outcome == "pneumonia") {
    help_function(5)
  } else {
    stop("invalid outcome")
  }
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}