## makeCacheMatrix creates a new type of cached matrix. This new type is actually a list of 
## some functions taking advantage of lexical scoping
## The new type declares 4 methods:
## get() to retrieve the matrix
## set(mat) to set the matrix
## getInverse() to retrieve the cached inverse, this can be NULL
## setInverse(inverse) to cache the inverse matrix

## To use this cache matrix use cacheSolve which calculates the inverse of the cache matrix
## if the inverse was calculated before and if the matrix has not changed, then the cacheSolve function
## will return the cached inverse otherwise it will calculate and store it

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    mat <- x$get()
    inverse <- solve(mat, ... = ...)
    x$setInverse(inverse)
    inverse
}
