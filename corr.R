#https://stackoverflow.com/questions/3798998/cor-shows-only-na-or-1-for-correlations-why
#cor(data$price, data$exprice, use = "complete.obs")
source("common.R")
source("complete.R")
corr <- function(directory , threshold = 0){
  allCases <- complete(directory)
  allCases <-  allCases[allCases$nobs >= threshold , ]
  
  results <- numeric(0)
  for(fileId in allCases$id)
  {
    
    fileName = getFilePath(directory ,fileId)
    readFile <- read.csv(fileName)
    readFile <- readFile[complete.cases(readFile) , ]
    results <- c (results ,cor(readFile$sulfate , readFile$nitrate) )
  }
  results  
}