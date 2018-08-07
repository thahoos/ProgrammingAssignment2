## Functions written to fulfill Coursera R-Programming Week 3  programming assignment 



 

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


## This function generates the inverse of the matrix
## If the inverse has already been calculated  and the matrix has not changed then
##  cacheSolve will pull the inverse from the cache

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

 
