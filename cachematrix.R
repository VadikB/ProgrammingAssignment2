## The function is prepating environment for using cashSolve just like it was made in the example
## usage :
# x1 <- makeCacheMatrix(x) - to prepare the cache
# cacheSolve(x1)           - to get the inverse matrix, trying to get it from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setSolve <- function(slv)
    m <<- slv
  getSolve <- function()
    m
  list(
    set = set, get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )
}

## The function tryies to get inverse matrix from cache first, uses solve function if inverse matrix is still not in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
