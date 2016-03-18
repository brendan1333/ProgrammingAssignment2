## The two functions (makeCacheMatrix & cacheSolve) are used to create a cache of
## the inverse of a matrix.

## "makeCacheMatrix" is the first function that creates the matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
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


## "cacheSolve" is the function that will compete the inverse of the matrix produced
## by "makeCacheMatrix". If this inverse matrix is not different from the original
## solution (the inverse is the same) then it will retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
