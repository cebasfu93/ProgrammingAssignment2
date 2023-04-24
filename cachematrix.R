# creates a "special" matrix with methods to set and get 
# the array data and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


# given a "special", fetch its inverse and return it. 
# If the "special" matrix has not cached the inverse yet, compute it, store it,
# and return it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
