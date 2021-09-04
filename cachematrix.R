## The following functions compute the inverse of a matrix. In order to make
## computations faster the functions avoid repeated calculation of inverse 
## for the same matrix and instead of that it caches the inverse.

## The following function creates a special type of matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL ## intialising z
  ## z corresponds to the inverse of matrix x 
  set <- function(y){
    x <<- y
    z <<- NULL
  } ## Function 'set' resets the matrix x to matrix y and set its inverse to NULL. 
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function computes the inverse of the special type of matrix returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }## This if condition checks if the inverse of the matrix x is in the cache memory. 
  data <- x$get() ## if the above if condition is not satisfied then it computes the inverse of matrix x by solve function.
  z <- solve(data,...)
  x$setinverse(z)## It computes the inverse and stores it in the cache memory.
  z
  ## Return a matrix that is the inverse of 'x'
}
