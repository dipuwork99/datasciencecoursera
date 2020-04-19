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
  print(id)
  count <-1
  summaryFile <- NULL
  for(fileId in id)
  {
    fileName = paste(directory , "/" , cFileName(fileId , 3) , ".csv" , sep = "")
    print(fileName)
    readFile <- read.csv(fileName)
    if(count ==1)
    {
      summaryFile <- readFile[FALSE,]
      print(dim(summaryFile))
      count <- count + 1
    }
    size <- dim(readFile)[1]
    print(size)
    for( i in 1:size )
    {
      summaryFile <- rbind(summaryFile , readFile[i , ])
    }
  }
  mean(summaryFile[ , pollutant] , na.rm = TRUE)
}


complete <- function(directory , id = 1:332)
{
  obsMatrix <- NULL
  obsMatrix <- data.frame("id" = integer() , "nobs" = integer())
  print(names(obsMatrix))
  print(dim(obsMatrix))
  for(fileId in id)
  {
    fileName = paste(directory , "/" , cFileName(fileId , 3) , ".csv" , sep = "")
    print(fileName)
    readFile <- read.csv(fileName)
    size <- dim(readFile)[1]
    print("dimensions of items")
    print(class(size))
    print(class(fileId))
    print(names(obsMatrix))
    obsMatrix <- rbind(obsMatrix , list(fileId , size))
    print(dim(obsMatrix))
    print(names(obsMatrix))
    
  }
  obsMatrix <- setNames(obsMatrix , c("id" , "nobs"))
  print(obsMatrix)
  obsMatrix
}
