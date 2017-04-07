## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to cache the inverse of a matrix.

## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      invn <- NULL
      set <- function(y) {
        x <<- y
        invn <<- NULL
      }
      get <- function() x
      setinversion <- function(inversion) invn <<- inversion
      getinversion <- function() invn
      list(set = set, 
           get = get,
           setinversion = setinversion,
           getinversion = getinversion)
}


## The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      invn <- x$getinversion()
      if(!is.null(invn)) {
        message("getting cached data")
        return(invn)
      }
      data <- x$get()
      invn <- solve(data, ...)
      x$setinversion(invn)
      invn
}
