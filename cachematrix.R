## Create and retrieve the cache of an matrix inverse operation
## The matrix inverse computation is costly. These functions allow
## the data to be store (cached) locally using makeCacheMatrix and
## retrieve using cacheSolve.  

## makeCacheMatrix checks if the contents have changed. If so the
## contents are recalculated and cached locally

makeCacheMatrix <- function(x = matrix()) {

}


## cacheSolve checks if the contents have already been calculated
## and cached. If so the cached content is returned. If the content
## is not found then the matrix inverse operation is performed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


