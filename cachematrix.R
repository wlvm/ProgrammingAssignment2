## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## In this assignment I write a pair of functions that cache the inverse of an
## invertible matrix.

## 'makeCacheMatrix' function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    ## if matrix is assumed invertible we can use solve to get its inverse
    # set the inverse of the matrix
    setinverse <- function(inverse) m <<- solve
    # get the inverse of the matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve' function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'   
    ## We can assume here that:
    ## matrix is invertible 
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
