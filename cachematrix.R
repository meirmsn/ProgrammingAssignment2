## This file generates functions to handle a special type of matrix: a cached one
## A cache matrix is a matrix of which it's inverse  is only calculated the first
## it's asked for from the code, saved on a cache, and returned upon future requests
## to calculate it again. If the matrix changes, the cache is reset, meaning the next
## time the inverse is requested, it will be calculated (and saved to the cache, and so on)

## makeCacheMatrix generates the special matrix "class", one that can 
## store and retrieve both the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initially set the inverse to NULL since it's unknown
  i <- NULL
  # A set function to update the matrix
  set <- function(y) {
    # Overwrite the current matrix
    x <<- y
    ## If the matrix changes, set the inverse to NULL since it's unknown at this time
    i <<- NULL
  }
  # A get function to retrieve the matrix
  # A setInverse and getInverse functions to set and get the inverse once it's calculated
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  # Return a list with the functions to handle this special matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve return a matrix that is the inverse of 'x', but it is only
## calculated once and stored in a cache. Further calls to this function
## return the cached inverse. If the matrix changes, then the inverse is
## calculated again
cacheSolve <- function(x, ...) {
  ## Try to get the inverse, if existing (e.g.: not null) indicate with print and return it!
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## This code is only run if the cached version doesn't exists: get the matrix, solve for
  ## it's inverse
  data <- x$get()
  i <- solve(data, ...)
  ## Save the newly calculated inverse in the cache and return it
  x$setInverse(i)
  i
}
