
##assignment 2

makeCacheMatrix <- function(x = matrix())
{
  invr <- NULL
  set <- function(y)
  { x <<- y
  invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  if (!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  my_mat <- x$get()
  invr <- solve(my_mat, ...)
  x$setInverse(invr)
  invr
}
