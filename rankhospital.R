rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
  ## Read outcome data
  relevant <- data[,c(2, 7, 11, 17, 23)]
  names(relevant) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  outcomes <- relevant[, c(3, 4, 5)]
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  if(!(state %in% relevant$state))
    stop("Invalid State")
  if(!(outcome %in% names(outcomes)))
    stop("Invalid Outcome")
  relevant <-relevant[relevant$state == state, ]
  col_outcome <-  which(names(outcomes) == outcome) +2 #heart_attack=first of names(outcomes) and third of all col (data)
  relevant <- relevant[,c(1, 2, col_outcome)] 
  relevant <- na.omit(relevant)
  names(relevant) <- c("hospital", "state", "outcomek")
  result <- arrange(relevant, outcomek, hospital)
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(num == "best")
  rankhospital <- result[1, "hospital"]
 if(num == "worst")
    rankhospital <- result[nrow(result), "hospital"]
else
  rankhospital <- result[num, "hospital"]
  rankhospital
  }
