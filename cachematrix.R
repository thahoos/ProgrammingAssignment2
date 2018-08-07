## Functions written to fulfill Coursera R-Programming Week 3  programming assignment 

## The   functions are  to produce the inverse of a matrix with taking advantage of 
## caching and thus avoiding repeated computation. The first function creates a list 
## containing a function to set a matrix, get a matrix, set the inverse of a matrix, 
## and get the inverse of a matrix.


## The  second function generates the inverse of the matrix
## If the inverse has already been calculated  and the matrix has not changed then
##  cacheSolve will pull the inverse from the cache
 

## matrix function to cache its inverse.


makeCacheMatrix  <- function(x =  matrix()) {   # default aurgement as matrix
  inv <- NULL                                   ## inv to  hold value of matrix inverse 
  set <- function(y) {                          ## set function to assign value of matrix to parent environment
    x <<- y                                     
    inv <<- NULL                                ## If new matrix, assign null to inv 
  }                                             ## get fucntion - returns value of the matrix 
  get <- function() x
  setinverse  <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse  <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} 




cacheSolve  <- function(x, ...) {
  inv  <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

 
