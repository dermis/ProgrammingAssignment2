## This to functions to demonstrate how to cache the value for
## time consuming operation by taking advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside
## of an R object.

## This makeCacheMatrix function creates a special "matrix" to be used
## with cacheSolve function to cache its inversed value.

makeCacheMatrix <- function(myMatrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    myMatrix <<- y
    inverse <<- NULL
  }
  get <- function() myMatrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This cacheSolve function calculates inversed values for special "matrix"
## created using makeCacheMatrix function. If inversed values already calculated, 
## the returns values will be retrieved from the cache rather than recalculate it

cacheSolve <- function(myMatrix, ...) {
  inverse <- myMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting inversed value from cached")
    return(inverse)
  }
  data <- myMatrix$get()
  inverse <- solve(data, ...)
  myMatrix$setInverse(inverse)
  inverse
}
