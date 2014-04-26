## Due to the high computational costs of calculating the inverse of matricies
## it is useful to cache inverse matrix values as opposed to repeatedly calculating
## them. The following pair of functions work together to cache inverse matrix values.

## makeCacheMatrix produces an abstracted "matrix" object (a list of four functions)
## capable of cacheing its own inverse.

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


## cacheSolve computes the inverse of the special "matrix" created by the above
## function. If the inverse hasn't been solved, it is computed directly and cached; however,
## if it has, the cached value is retrieved

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
