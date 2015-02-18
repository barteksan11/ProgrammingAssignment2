## Functions for creating and using inverted matrices which caching ability
## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #making a placeholder for inverted matrix
  inv <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL # matrix has changed, reassign to NULL
  }
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(solve) inv <<- solve
  
  # get inverse of matrix
  getinverse <- function() inv
  
  # return a list containing all functions defined above
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  # get inverse
  inv <- x$getinverse()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, get matrix
  data<- x$get()
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  # return inverse
  inv
}