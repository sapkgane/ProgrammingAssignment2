## The main goal of this assignment is to write down some functions that cache the inverse of matrix. 

## makeCacheMatrix creates a "matrix" object,which can cache its inverse for the input. 

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is a function which computes the incerse of the "matrix"  returned by the previous function makeCacheMatrix. 

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
    if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
