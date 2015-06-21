## Put comments here that give an overall description of what your
## functions do

## Function to make a cache matrix of its inverse

makeCacheMatrix <- function(m = matrix()) {

  i <- NULL
  
  ## To set the value of matrix
  set <- function(x) {
    m <<- x;
    i <<- NULL;
  }
  
  ## To get value of matrix
  get <- function() {
    return(m)
  }
  
  ## To set the value of inverse
  setinv <- function(inv) {
    i <<- inv
  }
  
  ## To get the value of inverse
  getinv <- function() {
    return(i)
  }
  
  ## The return statement
  return(list(set = set, get = get,
        setinv = setinv, 
        getinv = getinv))
}


## Function the returns inverse of the matrix given if it is not yet set

cacheSolve <- function(m, ...) {

  ## Calling function getinv() to the i
  i <- m$getinv()
  
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  
  ## Give the value of matrix to data
  data <- m$get()
  i <- solve(data, ...)
  m$setinv(i)
  return(i)
}

