## Matrix inversion is usually a costly computation and there can be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The following pair of functions, makeCacheMatrix and cacheSolve, is able
## to cache the inverse of a matrix and return the value.

## We assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    ## solve(data) calculates the inverse of the matrix in data
    ## We assume that the matrix supplied in data is always invertible.
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}