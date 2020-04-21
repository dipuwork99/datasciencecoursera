files <- list.files()
readFile <- NULL
summaryFile <- NULL
count <-1
for(fileName in files)
{
  readFile <- read.csv(fileName)
  print(fileName)
  if(count ==1)
  {
    summaryFile <- readFile[FALSE,]
    count <- count + 1
  }
  size <- dim(readFile)[1]
  print(size)
  for( i in 1:size )
  {
    summaryFile <- rbind(summaryFile , readFile[i , ])
  }
}
#data_2 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("x1", "x2", "x3"))
#https://www.programmingr.com/examples/r-dataframe/create-empty-data-frame/
#Create a place holder data frame using the first frame from the list.


i <- 11
j <- as.character(i)
nchar(j)

cFileName <- function (fileIndex , count = 3 , fill = "0")
{
  cCount <- nchar(as.character(fileIndex))
  fileName = as.character(fileIndex);
  if(cCount  < count)
  {
    loopCount <- count - cCount
   
    for(i in 1:loopCount){
      fileName <- paste(fill,  fileName , sep="")
 
    }    
  }
  fileName
}

pollutantmean <- function(directory  , pollutant , id = 1:332)
{
  print("ko")
  count <-1
  summaryFile <- NULL
  for(fileId in id)
  {
    fileName = paste(directory , "/" , cFileName(fileId , 3) , ".csv" , sep = "")
    readFile <- read.csv(fileName)
    if(count ==1)
    {
      summaryFile <- readFile[FALSE,]
      count <- count + 1
    }
    size <- dim(readFile)[1]
    for( i in 1:size )
    {
      summaryFile <- rbind(summaryFile , readFile[i , ])
    }
  }
  mean(summaryFile[ , pollutant] , na.rm = TRUE)
}


#https://statisticsglobe.com/complete-cases-in-r-example/
#https://statisticsglobe.com/complete-cases-in-r-example/
complete <- function(directory , id = 1:332)
{
  obsMatrix <- NULL
  obsMatrix <- data.frame("id" = integer() , "nobs" = integer())
  for(fileId in id)
  {
    fileName = paste(directory , "/" , cFileName(fileId , 3) , ".csv" , sep = "")
    readFile <- read.csv(fileName)
    readFile <- readFile[complete.cases(readFile) , ]
    size <- dim(readFile)[1]

    obsMatrix <- rbind(obsMatrix , list(fileId , size))
  }
  obsMatrix <- setNames(obsMatrix , c("id" , "nobs"))
  obsMatrix
}


#https://stackoverflow.com/questions/3798998/cor-shows-only-na-or-1-for-correlations-why
#cor(data$price, data$exprice, use = "complete.obs")

corr <- function(directory , threshold = 0){
  allCases <- complete(directory)
  allCases <-  allCases[allCases$nobs > threshold , ]
 
  results <- numeric(0)
  for(fileId in allCases$id)
  {
    
    fileName = paste(directory , "/" , cFileName(fileId , 3) , ".csv" , sep = "")
    readFile <- read.csv(fileName)
    readFile <- readFile[complete.cases(readFile) , ]
    results <- c (results ,cor(readFile$sulfate , readFile$nitrate) )
  }
  results  
}

evaluateResults <- function(...){
  directory <- "/Users/dipuchakrapani/Documents/Learning/R/Samples/specdata"
  presult <- pollutantmean(directory, "sulfate", 1:10)
  print(paste("Question 1 " , presult ))
  presult <- pollutantmean(directory, "nitrate", 70:72)
  print(paste("Question 2 " , presult ))
  presult <- pollutantmean(directory, "sulfate", 34)
  print(paste("Question 3 " , presult ))
  presult <- pollutantmean(directory, "nitrate")
  print(paste("Question 4 " , presult ))

  cc <- complete(directory, c(6, 10, 20, 34, 100, 200, 310))
  print(paste("Question 5 " , cc$nobs ))
  
  cc <- complete(directory, 54)
  print(paste("Question 6 " ,  cc$nobs))
  
  
  RNGversion("3.5.1")  
  set.seed(42)
  cc <- complete(directory, 332:1)
  use <- sample(332, 10)
  print(paste("Question 7 " , cc[use, "nobs"]))
  
  
  cr <- corr(directory)                
  cr <- sort(cr)   
  RNGversion("3.5.1")
  set.seed(868)                
  out <- round(cr[sample(length(cr), 5)], 4)
  print(paste("Question 8 " ,out))
  
  cr <- corr(directory, 129)                
  cr <- sort(cr)                
  n <- length(cr)    
  RNGversion("3.5.1")
  set.seed(197)                
  out <- c(n, round(cr[sample(n, 5)], 4))
  print(paste("Question 9 " ,out))
  

  cr <- corr(directory, 2000)                
  n <- length(cr)                
  cr <- corr("specdata", 1000)                
  cr <- sort(cr)
  print(paste("Question 10 " , c(n, round(cr, 4)) ))
  
}
