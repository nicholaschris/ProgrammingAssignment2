##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse.x <- NULL
  set <- function(y) {
    x <<- y
    inverse.x <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) {
    inverse.x <<- inv   
  }
  getinverse <- function() inverse.x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse.x <- x$getinverse()
  if(!is.null(inverse.x)) {
    message("Getting cached data")
    return(inverse.x)
  }
  data <- x$get() # Get the matrix to inverse 
  inverse.x <- solve(data, ...)  # Inverse matrix
  x$setinverse(inverse.x)  
  inverse.x
}
