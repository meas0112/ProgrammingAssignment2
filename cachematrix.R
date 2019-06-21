## Functions  to cache the inverse of a Matrix using lexical scoping  
##   (Scoping is the mechanism within R that determines how R finds symbols (i.e. programming language elements)
##      to retrieve their values during the execution of an R script)  


## makeCacheMatrix is a function 
##   that creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {   
  s <- NULL                                   
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve is a function 
##   that retrieves the inverse of a matrix from the cached value that is stored in the makeCacheMatrix() object's 
##   environment. It requires an argument that is returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## The following codes are used to test the above two functions

#create an initial 2 x 2 matrix named twobytwo , then a testmatrix
twobytwo = matrix(c(4, 2, 7, 6), nrow=2, ncol=2)
testmatrix = matrix(c(3, 3.5, 3.2, 3.6), nrow=2, ncol=2)

#test that the functions work
aMatrix <- makeCacheMatrix(twobytwo)
aMatrix$get()               # retrieve the value of x
aMatrix$getsolve()          # retrieve the value of s, which should be NULL
aMatrix$set(testmatrix)     # reset value with a new matrix
cacheSolve(aMatrix)         # notice solve calculated is solve of testmatrix, not twobytwo
aMatrix$getsolve()          # retrieve it directly, now that it has been cached