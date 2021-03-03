pollutantmean <- function(directory, pollutant, id = 1:332) {
    readings <- c()
    for(monitorId in id) {
        filename <- generateMonitorFilename(directory, monitorId)
        monitor <- read.csv(filename)
        readings <- append(monitor[[pollutant]], readings)
    }
    mean(readings, na.rm = TRUE)
}

generateMonitorFilename <- function(directory, monitorId) {
    fileName <- formatC(monitorId, width=3,  flag = "0")
    paste(directory, "/", fileName, ".csv", sep = "")
}