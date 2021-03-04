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

corr <- function(directory, threshold = 0) {
    correlations <- c()
    
    allMonitors <- complete(directory)
    validMonitorIds <- allMonitors[allMonitors["nobs"] > threshold,"id"] 
    
    for(monitorId in validMonitorIds) {
        filename <- generateMonitorFilename(directory, monitorId)
        monitor <- read.csv(filename)
        
        completeCases <- monitor[complete.cases(monitor),c("sulfate", "nitrate")]
        correlation <- cor(completeCases$sulfate, completeCases$nitrate)
        correlations <- append(correlation, correlations, after = 0)
    }
    correlations
}

generateMonitorFilename <- function(directory, monitorId) {
    fileName <- formatC(monitorId, width=3,  flag = "0")
    paste(directory, "/", fileName, ".csv", sep = "")
}