best <- function(state, outcome) {
  ## Read outcome data, all characters, converted to NA
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
  ## extract relevant data
  relevant <- data[,c(2, 7, 11, 17, 23)]
  names(relevant) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  outcomes <- relevant[, c(3, 4, 5)]
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are  valid
  if(!(state %in% relevant$state))
    stop("Invalid State")
  if(!(outcome %in% names(outcomes)))
    stop("Invalid Outcome")
  
  relevant <- relevant[relevant$state == state, ] #subsetting by rows based on the state arguent
  #relevantout <- relevant[[outcome]] 
  #hosp_name <- relevant[, "hospital"]
  col_outcome <-  which(names(outcomes) == outcome) +2 #heart_attack=first of names(outcomes) and third of all col (data)
  relevant <- relevant[,c(1,col_outcome)]
  names(relevant) <- c("hospital", "outcomek")
  result <- arrange(relevant, outcomek)
  ## Return hospital name in that state with lowest 30-day death
  #result <- relevant[order(relevant$outcome, relevant$hospital, na.last = TRUE)]
                 
                     best <- result[1,"hospital"]
                     best
  best
                     
  ## rate
}