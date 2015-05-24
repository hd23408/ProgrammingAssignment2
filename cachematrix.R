## This file contains a function to create a special matrix object
## that caches its inverse (once its inverse is computed), as well as
## a function to find the inverse of a matrix by either 
## computing it directly or pulling it from the matrix's cache.

## makeCacheMatrix creates a matrix object that is capable of 
## caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  mdat <- x$get()
  inv <- solve(mdat, ...)
  x$setinv(inv)
  inv
}
