# Set your working directory
#setwd("")

rankhospital <- function(state, outcome, num = "best") {

		## Read outcome data
		full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

               ## Check that state and outcome are valid
		column <- if (outcome == "heart attack") {
			"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
		} else if (outcome == "heart failure") {
			"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
		} else if (outcome == "pneumonia") {
			"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		} else {
			stop("invalid outcome")
		}

		data_for_state <- full_data[full_data$State == state, c("Hospital.Name", column)]
		if (nrow(data_for_state) == 0) {
		stop("invalid state")	
		}

                ## Return hospital name in that state with the given rank 30-day death rate
		data_for_state[,2] <- as.numeric(data_for_state[,2])
		ordered_data_for_state <- order(data_for_state[column], data_for_state$Hospital.Name, na.last=NA)

		if (num == "best") {
			as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
		} else if (num == "worst") {
			as.character(data_for_state$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
		} else if (is.numeric(num)) {
			as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
		} else {
			stop("invalid num")
		}
}


# Run tests
source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
#[1] NA
