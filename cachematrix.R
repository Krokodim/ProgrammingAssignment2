###############################################################
##                                                           ##
##           cachematrix.R                  15.01.2015       ##
##                                                           ##
##-----------------------------------------------------------##
##                                                           ##
##    This module contains a set of functions to solve       ##
##  square matricies with caching                            ##
##                                                           ##
###############################################################



## makeCacheMatrix
## ---------------
## accepts  an inversible square matrix, stores it and 
## returns an interface to manipulate the data:
##    - set(x)          stores the initial matrix
##    - get()           returns the initial matrix
##    - setsolve(x)     stores the solved matrix
##    - getsolve()      returns the solved matrix (or NULL)
##
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function (solve) m <<- solve
  getsolve <- function () m
  list(get=get, 
       set=set, 
       getsolve=getsolve, 
       setsolve=setsolve)

}


## cacheSolve 
## ---------------
## Accepts the initial matrix in a form returned by 
## makeCacheMatrix function and returns an inversed
## matrix. If the matrix was already inverted, reads
## the result from the cache, without calculations
## 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}