## The following functions will compute and cache the inverse of a matrix
## and calls it from the cache if it is pre existing.

## This function creates  "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mat <<- x;
    inverse <<- NULL;
  }
  get <- function() mat
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix
## If the inverse is pre existing and remains unchanged, then
## the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getinv()
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinv(inverse)
  inverse
}