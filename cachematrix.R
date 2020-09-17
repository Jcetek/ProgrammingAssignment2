## These functions cache the computed inverse of a given matrix.
## If the value has already been calculated then the inverse of the matrix
##is retreived from the cache. If not then it is calculated and returned

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then the cachesolve should retrieve the inverse from
##the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

