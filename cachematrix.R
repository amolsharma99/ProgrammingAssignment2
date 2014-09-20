## Cache Potential time-consuming computations.

##Sample Usage
## > mat <- matrix(rnorm(16), nrow = 4)
## > spclmat <- makeCacheMatrix(mat)
## > cacheSolve(spclmat)  
## > cacheSolve(cx)  ##gets from cache

## this function creates a special matrix, which is a list of functions -
## set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function calculates inverse of the matrix created using above function. 
## It checks cache first to check if it's already been calculated.

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
