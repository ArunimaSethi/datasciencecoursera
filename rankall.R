rankall <- function(outcome, num="best"){
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
  ## Read outcome data
  relevant <- data[,c(2, 7, 11, 17, 23)]
  names(relevant) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  outcomes <- relevant[, c(3, 4, 5)]
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% names(outcomes)))
    stop("Invalid Outcome")
  col_outcome <-  which(names(outcomes) == outcome) +2 #heart_attack=first of names(outcomes) and third of all col (data)
  relevant <- relevant[,c(1, 2, col_outcome)] 
  relevant <- na.omit(relevant)
  names(relevant) <- c("hospital", "state", "outcomek")
  result <- arrange(relevant, state, outcomek, hospital)
  result <- split(result, result$state)
  
  helper <- function(dat){
 if(num == "best")
   r <- dat[1, "hospital"]
 if(num == "worst")
   r <- dat[nrow(dat), "hospital"]
 else
   r <- dat[num, "hospital"]
  }

  result <- sapply(result, helper, USE.NAMES = TRUE)
   
 result  
 data.frame(result, state=names(result), row.names=names(result))
}
