## makeCacheMatrix creates a special "matrix" object that can cache its inverse.  
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

## mat is the matrix that will have its inverse calculated if it hasn't been computed anymore
## How to use:
## > X <- matrix(c(1,2,2,3,4,5,1,2,1), nrow = 3)
## > x <- makeCacheMatrix(X)
## > x$get()
## [,1] [,2] [,3]
## [1,]    1    3    1
## [2,]    2    4    2
## [3,]    2    5    1
## > inv <- cacheSolve(x)
## > inv
## [,1] [,2] [,3]
## [1,]   -3  1.0    1
## [2,]    1 -0.5    0
## [3,]    1  0.5   -1
## > solve(X)
## [,1] [,2] [,3]
## [1,]   -3  1.0    1
## [2,]    1 -0.5    0
## [3,]    1  0.5   -1 
makeCacheMatrix <- function(mat = matrix()) {
        invM <- NULL
        set <- function(y) {
                mat <<- y
                invM <<- NULL   #resets the inverse to null because the matrix changed.
        }
        get <- function() mat
        setinverse <- function(solve) invM <<- solve
        getinverse <- function() invM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calls the functions defined in makeCacheMatrix in order to compute the inverse of a matrix 
cacheSolve <- function(m, ...) {
  invM <- m$getinverse()
  if(!is.null(invM)) {
    # If the matrix is null, the inverse has not been computed yet.
    message("getting cached inverse")
    return(invM)
  }
  mat <- m$get()
  invM <- solve(mat, ...)
  m$setinverse(invM)
  invM
  ## Return a matrix that is the inverse of 'm'
}
