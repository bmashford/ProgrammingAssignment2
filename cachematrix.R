# These functions will calculate the inverse of a matrix. They will retrive the answer from the cache if it exists.

# makeCacheMatrix creates a list containing functions that: set, get, set inverse, get inverse

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


# This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If it does, it gets that result.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

