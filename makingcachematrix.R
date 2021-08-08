## (Mass) library is used for calculating inverse for nonsquared and squared matrices##
#makeCacheMatrix consists of set,get, setInverse,  getInverse#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  # function to get matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {
    inver<-ginv(x)
    inver%*%x}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## this is used to get the cache data##
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # return inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}







