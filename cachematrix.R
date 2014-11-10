## Functions to create a list that stores a matrix and caches its inverse.

## Creates a list for setting and getting of matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
  # Check to see if inverse has already been computed.
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Inverse not computed already, so compute inverse and sets value in the 
  # cache.
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
