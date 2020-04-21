#https://statisticsglobe.com/complete-cases-in-r-example/
#https://statisticsglobe.com/complete-cases-in-r-example/
source("common.R")
complete <- function(directory , id = 1:332)
{
  obsMatrix <- NULL
  obsMatrix <- data.frame("id" = integer() , "nobs" = integer())
  for(fileId in id)
  {
    fileName = getFilePath(directory , fileId)
    readFile <- read.csv(fileName)
    readFile <- readFile[complete.cases(readFile) , ]
    size <- dim(readFile)[1]
    obsMatrix <- rbind(obsMatrix , list(fileId , size))
  }
  obsMatrix <- setNames(obsMatrix , c("id" , "nobs"))
  obsMatrix
}
