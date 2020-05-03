## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
  setMatrix <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix , setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  #args <- list(...)
  #orgMatrix <- args[["orgmatrix"]]
  #print(i)
  #print(orgMatrix)
  #if(!is.null(i) && identical( x$getMatrix(), orgMatrix)) {
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  invmatrix <- solve(data, ...)
  #x$setMatrix(orgMatrix)
  x$setInverse(invmatrix)
  invmatrix
}

#http://adv-r.had.co.nz/Functions.html
#http://adv-r.had.co.nz/Functional-programming.html#closures
#http://adv-r.had.co.nz/Functional-programming.html#closures
makeVector <- function(x = numeric()) {
  print("hello")
  print(ls())
  m <- NULL
  set <- function(y) {
    print(ls())
    x <<- y
    print(y)
 
    m <<- mean(y)
    print(m)
  }
  get <- function(){
    x
  } 
  setmean <- function(mean){
    print(ls)
    m <<- mean
  } 
  getmean <- function(){
    m
  } 
  print(ls())
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  identical(org ,x$get() )
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#  args <- list(...)
#place <- args[["place"]]