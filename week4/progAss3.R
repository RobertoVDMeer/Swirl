library(plyr)
fatalities <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

mortalityRates <- function() {
    outcome <- getOutCome()
    hist(as.numeric(outcome[,3]), xlab="30-day Mortality Rate", main="30-day Mortality Rates") 
}

# plyr for sorting -- arrange(), reference the colnames directly (e.g. without df$yadda)
# move rownames to a column, because plyr::arrange() does not work with rownames
#
# carName <- rownames(mtcars)
# mtcars <- cbind(carName,mtcars)
# rownames(mtcars) <- NULL

# Hospital name 2
# State 7
# 
# heart attack col 11
# heart failure 17
# pneumonia 23

best <- function(state, fatality) {
   outcome <- getOutCome(state, fatality)     
   outcome <- arrange(outcome, Outcome, Hospital)
   outcome[1,"Hospital"]
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