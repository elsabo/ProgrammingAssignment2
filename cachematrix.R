#  Programming Assignment 2: Lexical Scoping
#  Author: Jesus Chavez
## elsabo@gmail.com
## CourseraId: 6529762


## hilbert function 
## copied from ?solve
hilbert <- function(n) { 
  i <- 1:n
  1 / outer(i - 1, i, "+") 
}

## Receives x as matrix, it has to be inversable, no error handling added
## inverse: returns the value of ix, defined to hold the inverse of the instatiated matrix
## invert: Will inverse matrix x and store it in ix
## get|set to retrieve|assign the matrix after first instatiation
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL

  get <- function() x
  set <- function(m)  { 
    ## set can be used to change the matrix
    x <<- m
    ix <<- NULL
  }
  
  inverse <- function() ix
  invert <- function() {
    ## Extremely important to pay attention to the lexical scoping 
    ix <<- solve(get())
  }
  list(get = get, set = set, inverse = inverse, invert = invert)
}

## Receives a special matrix created with makeCacheMatrix
## makeCacheMatrix holds an invertable matrix 
## It will always return the inverse of the matrix stored in the object makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##Check if we already have the inverse
  inverted <- x$inverse()
  
  if (is.null(inverted)) {
    ## We do not
    message("First time, lets cache this")
    x$invert()
    inverted <- x$inverse()
  } else {
    ## We do
    message("Got it from the cache")
  }
  
  inverted
}

