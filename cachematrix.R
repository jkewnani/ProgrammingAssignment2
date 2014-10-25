## R Programming - Assignment 2
## The functions below do the following:
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and matrix hasn't been changed), inverse
## is retrieved from cache.


## makeCacheMatrix
## creates a special "matrix" object with functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve
## calculates inverse matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}