# Set your working directory
setwd("")

best <- function(state ="AL", outcome ="heart attack") {

  full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
  column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  data_for_state[,2] <- as.numeric(data_for_state[,2])
  ordered_data_for_state <- order(data_for_state[,2], data_for_state$Hospital.Name)
  data_for_state$Hospital.Name[ordered_data_for_state[1]]

}

# Run tests
best ("AL","heart attack")
#[1] "CRESTWOOD MEDICAL CENTER"

best2 <- function(state , outcome ="heart attack") {

  full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
  data_for_state[,2] <- as.numeric(data_for_state[,2])
  ordered_data_for_state <- order(data_for_state[,2], data_for_state$Hospital.Name)
  data_for_state$Hospital.Name[ordered_data_for_state[1]]

}

# Run tests
best2 ("AL","heart attack")
#[1] "CRESTWOOD MEDICAL CENTER"

best3 <- function(state, outcome) {

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
best3 ("AL","heart attack")
#[1] "CRESTWOOD MEDICAL CENTER"

best3 ("TX","heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"

best3 ("NY", "hert attack")
#Error in best3("NY", "hert attack") : invalid outcome

best3 ("BB","heart attack")
#Error in best3("BB", "heart attack") : invalid state
