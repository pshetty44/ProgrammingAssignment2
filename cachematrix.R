## The program computes the inverse of a sqare matrix.
## If the inverse is already computed, it gets the inverse from the cache.

## This below function can cache the inverse of the given matrix.
## This has been acheived through R's lexical scoping.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The below function uses the solve function to get the inverse of the 
## given matrix.It checks if the inverse is already present in the cache
## thereby avoiding costly computation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}