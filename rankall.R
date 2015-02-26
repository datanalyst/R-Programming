
#setwd("C:/Users/cg07510/Desktop/UTILI")

#rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
#}

rankall <- function(outcome, num = "best") {

      ## Read outcome data
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that state and outcome are valid
	column <- if (outcome == "heart attack") {
			full_data[, 11] <- as.numeric(full_data[, 11])
			"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
		} else if (outcome == "heart failure") {
			full_data[, 17] <- as.numeric(full_data[, 17])
			"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
		} else if (outcome == "pneumonia") {
			full_data[, 23] <- as.numeric(full_data[, 23])
			"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		} else {
			stop("invalid outcome")
	}

	data_by_state <- split(full_data[, c("Hospital.Name", "State", column)], full_data$State)

	rank_hospital <- function(state_data, num) {
		ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)

		if (num == "best") {
			state_data$Hospital.Name[ordered_state_data[1]]
		} else if (num == "worst") {
			state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
		} else if (is.numeric(num)) {
			state_data$Hospital.Name[ordered_state_data[num]]
		} else {
			stop("invalid num")
		}
	}

      ## For each state, find the hospital of the given rank
	pre_result <- lapply(data_by_state, rank_hospital, num)

	## Return a data frame with the hospital names and the (abbreviated) state name
	data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))
}

#source("rankall.R")

#head(rankall("heart attack", 20), 10)

#                              hospital state
#AK                                <NA>    AK
#AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
#AR   ARKANSAS METHODIST MEDICAL CENTER    AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
#CA               SHERMAN OAKS HOSPITAL    CA
#CO            SKY RIDGE MEDICAL CENTER    CO
#CT             MIDSTATE MEDICAL CENTER    CT
#DC                                <NA>    DC
#DE                                <NA>    DE
#FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

#tail(rankall("pneumonia", "worst"), 3)
#                                     hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
#WV                     PLATEAU MEDICAL CENTER    WV
#WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

#tail(rankall("heart failure"), 10)
#                                                            hospital state
#TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
#TX                                        FORT DUNCAN MEDICAL CENTER    TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
#VA                                          SENTARA POTOMAC HOSPITAL    VA
#VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
#VT                                              SPRINGFIELD HOSPITAL    VT
#WA                                         HARBORVIEW MEDICAL CENTER    WA
#WI                                    AURORA ST LUKES MEDICAL CENTER    WI
#WV                                         FAIRMONT GENERAL HOSPITAL    WV
#WY                                        CHEYENNE VA MEDICAL CENTER    WY
