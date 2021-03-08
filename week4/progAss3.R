library(plyr)

fatalities <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

mortalityRates <- function() {
    outcome <- getOutCome()
    hist(as.numeric(outcome[,3]), xlab="30-day Mortality Rate", main="30-day Mortality Rates") 
}

best <- function(state, fatality) {
   outcome <- getOutCome(state, fatality)     
   outcome <- arrange(outcome, Outcome, Hospital)
   outcome[1,"Hospital"]
}

rankhospital <- function(state, fatality, num = "best") {
   outcome <- getOutCome(state, fatality)     
   outcome <- arrange(outcome, Outcome, Hospital)
   
   if(num == "best") return(outcome[1,"Hospital"])
   if(num == "worst") return(outcome[nrow(outcome),"Hospital"])
   if(num > nrow(outcome))return(NA)
   outcome[num,"Hospital"]
}

rankall <- function(fatality, num = "best") {
    outcome <- getOutCome(fatality = fatality)     
    outcome <- arrange(outcome, State, Outcome, Hospital)
    states <- split(outcome, outcome$State)
    
    hospitals <- sapply(states,getRankPerState,num)
    states <- names(hospitals)
    result <- data.frame(hospital=hospitals, state=states,row.names = states)
    result
}

getRankPerState <- function(state, num){
   if(num == "best") return(state[1, "Hospital"])
   if(num == "worst") return(state[nrow(state),"Hospital"])
   state[num,"Hospital"]
}
    
getOutCome <- function(state = "all", fatality = "heart attack") {
    outcome <- read.csv("outcome-of-care-measures.csv"
             , na.strings = "Not Available", stringsAsFactors = FALSE)
    
    if(state != "all") {
        outcome <- outcome[outcome$State == state,]
        
        # Check there are any results and assume an invalid state was 
        # given if not
        if(nrow(outcome) < 1){
            stop("Invalid State given")
        } 
    }
    
    # Check 1 of the 3 fatality rates was chosen
    if(!fatality %in% names(fatalities)) {
        stop("Invalid outcome option chosen")    
    } 
    
    outcome <- outcome[!is.na(outcome[,fatalities[fatality]]),]
    outcome <- outcome[, c(2, 7, fatalities[fatality])] 
    names(outcome) <- c("Hospital", "State", "Outcome") 
    
    outcome
}