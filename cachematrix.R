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
  
  set <- function (y) { x <<- y; m <<- NULL }
  get <- function()   { x }
  setsolve <- function (solve) { m <<- solve }
  getsolve <- function ()      { m }
  
  list(get=get, 
       set=set, 
       getsolve=getsolve, 
       setsolve=setsolve)
}

## cacheSolve 
## ----------
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

## testCacheSolve
## --------------
## Just a sample function showing how to use cacheSolve 
## and makeCacheMatrix funcions. 
##
testCacheSolve <- function () {
  #size for a test matrix
  dim <-100
  
  #init a test matrix #1
  m1 <- matrix(rnorm(dim * dim), dim)
  
  #init caching
  mx1 <- makeCacheMatrix(m1)
  
  #calc the inverted matrix (the inversion is calculated)
  mi11 <- cacheSolve(mx1)
  
  #do something ...
  # ...
  
  #calc it again (now it is read from a cache)
  mi12 <- cacheSolve(mx1)
  
  #set another matrix
  m2 <- matrix(rnorm(dim * dim), dim)
  mx2 <- makeCacheMatrix(m2)
  
  #calc it - it is calculated
  mi21 <- cacheSolve(mx2)  
  
  #and now it is read from a cache
  mi22 <- cacheSolve(mx2)  
}