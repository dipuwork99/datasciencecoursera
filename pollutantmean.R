source("common.R")
pollutantmean <- function(directory  , pollutant , id = 1:332)
{
  means <- c()
  for(monitor in id)
  {
    fileName = getFilePath(directory , monitor)
    readFile <- read.csv(fileName)
    #interestedData <- readFile[ complete.cases(readFile[ , pollutant])  , pollutant]
    interestedData <- readFile[  , pollutant]
    means <- c(means ,interestedData )
  }
  mean(means , na.rm = TRUE)
}
