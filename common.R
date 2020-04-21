getFilePath <- function (directory , fileIndex ,  ext=".csv")
{
   paste(getDataDir() , "/" , directory , "/" , sprintf("%03d" , fileIndex) , ".csv" , sep = "")
}

getDataDir <- function(){
  "/Users/dipuchakrapani/Documents/Learning/R/Samples"
}