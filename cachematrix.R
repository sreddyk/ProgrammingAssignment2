## Put comments here that give an overall description of what your
## functions do


## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  getInverse <- function() invMatrix
  setInverse <- function(x) {
    invMatrix <<- x
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setInverse(invMatrix)
  invMatrix
}
