## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL

  get <- function() x
  set <- function(m)  { 
    x <<- m
    ix <<- NULL
  }
  
  inverse <- function() ix
  invert <- function() {
    ix <<- solve(get())
  }
  list(get = get, set = set, inverse = inverse, invert = invert)
}

## hilbert function copied from ?solve
hilbert <- function(n) { 
  i <- 1:n
  1 / outer(i - 1, i, "+") 
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverted <- x$inverse()
  
  if (is.null(inverted)) {
    message("First time, lets cache this")
    x$invert()
    inverted <- x$inverse()
  } else {
    message("Got it from the cache")
  }
  
  inverted
}

