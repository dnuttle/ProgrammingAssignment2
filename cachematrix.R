## These functions allow for creating a matrix wrapper that encapsulates a matrix,
## and caches its inverse.  So the inverse is calculated on a lazy basis.
## If it is not calculated until it is requested.  If the set function of 
## the object returned by makeCacheMatrix is called, the internal matrix is updated,
## and the cached inverse is discarded.


## Returns a matrix wrapper.  The only argument should be a matrix.  The object returned
## has these methods:
## get: Returns the matrix stored in the object
## set: Updates the matrix stored in the object
## getInverse: Returns the inverse of the stored matrix.  This is returned from a cache, 
##   if it has already been created
## setInverse: Updates the cached inverse (should not be called externally)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Caches the inverse of the matrix in an object returned by makeCacheMatrix
## The only argument is an object returned by makeCacheMatrix.
## If that object already has a cached inverse, this function returns that cached inverse.
## Otherwise, the inverse is calculated and returned.
## A message is printed if the cached invserse is available.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  inv <- solve(m, ...)
  x$setInverse(inv)
}
