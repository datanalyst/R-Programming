# Set your working directory
#setwd("")

best <- function(state, outcome) {

  ## Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state is valid
  column <- if (outcome == "heart attack"){
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
          } else if (outcome == "heart failure"){
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
          } else if (outcome == "pneumonia"){
              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
          } else {
              stop("invalid outcome")
  }
  
  ## Check that outcome is valid
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  if (nrow(data_for_state) == 0) {
        stop("invalid state")	
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data_for_state[,2] <- as.numeric(data_for_state[,2])
  ordered_data_for_state <- order(data_for_state[,2], data_for_state$Hospital.Name)
  data_for_state$Hospital.Name[ordered_data_for_state[1]]
}

# Run tests
source("best.R")

best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

best ("TX","heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"

best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
