pollutantmean <- function(directory, pollutant, id = 1:332) {
    readings <- c()
    for(monitorId in id) {
        filename <- generateMonitorFilename(directory, monitorId)
        monitor <- read.csv(filename)
        readings <- append(monitor[[pollutant]], readings)
    }
    mean(readings, na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
    complete <- data.frame(matrix(ncol = 2, nrow = 0))
    
    for(monitorId in id) {
        filename <- generateMonitorFilename(directory, monitorId)
        monitor <- read.csv(filename)
        
        completeCases <- sum(complete.cases(monitor))
        
        complete <- rbind.data.frame(complete, c(monitorId, completeCases))
    } 
    colnames(complete) <- c("id", "nobs")
    complete
}

generateMonitorFilename <- function(directory, monitorId) {
    fileName <- formatC(monitorId, width=3,  flag = "0")
    paste(directory, "/", fileName, ".csv", sep = "")
}