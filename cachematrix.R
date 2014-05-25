## makeCacheMatrix creates a special "matrix" object
##  that can cache its inverse.
## cacheSolve computes the inverse of a special
##  "matrix" returned by makeCacheMatrix. If the
##  inverse has already been calculated (and the
##  matrix has not changed) then cacheSolve should
##  retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get function
  get <- function() x
  # set function
  setinv <- function(solve) m <<- mean
  # get function
  getinv <- function() m
  matrix(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # inverse already is cached
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(x)
  x$setinverse(inv)
  inv
}
