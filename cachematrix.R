## Put comments here that give an overall description of what your
## functions do

## Create a cache matrix (makeCacheMatrix) and provide a method do determine
## The iverse of the matrix

## Write a short comment describing this function
## -- Creates a list of functions which can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## --  Determines the inverse of the matrix returned
##   --  by makeCacheMatrix() except when its already in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if ( ! is.null(m)) {
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}