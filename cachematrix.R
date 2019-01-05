## This assignment is to invert a matrix and cache the result and retrieve from cache instead of
## recalculating the inverse


## Creating a cache for a matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inv)
    im <<- inv
  getinverse <- function()
    im
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculating matrix inverse and retrieving it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}

