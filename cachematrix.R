## Amit Aggarwal
## These two functions allow one to store the inverse of an invertible matrix, and also
## call it and reset it when necessary.

## This function takes in a matrix and assigns it an associated variable that acts as its
## cached inverse. There are nested functions that allow one to access this information as well.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function uses the makeCacheMatrix() function to check if there is a cached inverse,
## and if there is none, calculates, caches, and returns the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
