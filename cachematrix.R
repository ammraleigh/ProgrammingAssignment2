## Create and retrieve the cache of an matrix inverse operation
## The matrix inverse computation is costly. These functions allow
## the data to be store (cached) locally using makeCacheMatrix and
## retrieve using cacheSolve.  

## makeCacheMatrix checks if the matrix inverse operation has already
## been performed and cached. If not the operation is performed and
## the result is cached. If a prior cached result is found the cached 
## result is returned.

## Test Cases / Results
## Test 2 - 2x2 matrix
## > a = matrix(c(1,-1,1,2),nrow=2,ncol=2)
## > a
##       [,1] [,2]
## [1,]    1    1
## [2,]   -1    2
##
##  > myMatrix <- makeCacheMatrix(a)    ## create special matrix
##  > cacheSolve(myMatrix)
##
##          [,1]       [,2]
##  [1,] 0.6666667 -0.3333333
##  [2,] 0.3333333  0.3333333
##
## Test 2 - 3x3 matrix
## > a = matrix(c(1,2,-4,3,-2,1,0,1,-1),nrow=3,ncol=3)
## > a
##        [,1] [,2] [,3]
##   [1,]    1    3    0
##   [2,]    2   -2    1
##   [3,]   -4    1   -1
##
##  > myMatrix <- makeCacheMatrix(a)    ## create special matrix
##  > cacheSolve(myMatrix)
##
##       [,1] [,2] [,3]
##  [1,] -0.2 -0.6 -0.6
##  [2,]  0.4  0.2  0.2
##  [3,]  1.2  2.6  1.6

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve checks if the contents have already been calculated
## and cached. If so the cached content is returned. If the content
## is not found then the matrix inverse operation is performed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
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


