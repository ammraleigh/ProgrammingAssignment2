## Store input/output from the resource intensive matrix inverse 
## operation in a local cache to speed up processing times.

## makeCacheMatrix creates a special vector which is a list 
## containing functions to:
## 
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the solve(matrix)
##  - get the value of the solve(matrix)

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

## cacheSolve takes as input the special vector created by makeCacheMatrix
## The cacheSolve function then :
##    - checks if the solve result for the given matrix has already been cached
##    - if so then the cached result is output on the console & the function returns
##    - if not then the new input matrix is retrieved
##    - and the solve operator is applied to the new matrix and a result is cached
##    - Lastly, the solve result is output to the console

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

## Test Cases / Results
##
## Test 1 - 2x2 matrix
## > a = matrix(c(2,2,3,2),nrow=2, ncol=2)
## > a
##       [,1] [,2]
## [1,]    2    3
## [2,]    2    2
##
## > myMatrix <- makeCacheMatrix(a)
## > cacheSolve(myMatrix)
##       [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
##
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
## Test 3 - 3x3 matrix
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



