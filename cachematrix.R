## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function (x = matrix(), ...) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function (i = matrix(), ...) m <<- i
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix
## returned by makeCacheMatrix. Retrieving the inverse from the
## cache, if previously calculated

cacheSolve <- function(x, ...) {
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

## Assumption: matrix supplied is always invertible.
## Call example: 
## a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))

