## Two functions that can cache the inverse matrix and the retrieve the cache.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInv <- NULL
  set <- function(y) {
    x <<- y
    myInv <<- NULL
  }
  get <- function() x
  setMyInverse <- function(inverse) myInv <<- inverse
  getMyInverse <- function() myInv
  list(set = set,
       get = get,
       setMyInverse = setMyInverse,
       getMyInverse = getMyInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}