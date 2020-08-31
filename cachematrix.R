## The following two functions are responsible for creating a 'special' object that stores a matrix,
## and caches its inverse.


## makeCacheMatrix creates a 'special' matrix, that caches the inverse of said matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) invrs <<- inverse
  getinvrs <- function() invrs
  list(set = set,
       get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix. In the event the inverse
## of this matrix has already been computed then this cached inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invrs <- x$getinvrs()
  
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinvrs(invrs)
  invrs
}
