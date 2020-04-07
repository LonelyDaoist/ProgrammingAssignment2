## these functions makes a system for caching the inverse of a matrix

## this function sets up the cache for a matrix and exposes some APIs to access it.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    return(x)
  }
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() {
    return(inverse)
  }
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## this function can set/retrieve the inverse of a matrix to/from its cache

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
  inv
}
