setwd("/Users/dipuchakrapani/Documents/Coding/RProjects/datasciencecoursera")
source("common.R")
source("pollutantmean.R")

source("complete.R")
source("corr.R")

directory <- "specdata"

presult <- pollutantmean(directory, "sulfate", 1:10)
print(paste("Question 1 " , presult ))
presult <- pollutantmean(directory, "nitrate", 70:72)
print(paste("Question 2 " , presult ))
presult <- pollutantmean(directory, "sulfate", 34)
print(paste("Question 3 " , presult ))
presult <- pollutantmean(directory, "nitrate")
print(paste("Question 4 " , presult ))

cc <- complete(directory, c(6, 10, 20, 34, 100, 200, 310))
print("Question 5 ")
print( cc$nobs )

cc <- complete(directory, 54)
print("Question 6 ")
print( cc$nobs)


RNGversion("3.5.1")  
set.seed(42)
cc <- complete(directory, 332:1)
use <- sample(332, 10)
print("Question 7 ")
print( cc[use, "nobs"])


cr <- corr(directory)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print("Question 8 ")
print(out)

cr <- corr(directory, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print("Question 9 ")
print(out)


cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print("Question 10 ")
print(c(n, round(cr, 4)) )