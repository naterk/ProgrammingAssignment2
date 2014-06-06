## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setMatrix <- function(mtx) {
        x <<- mtx
        inverse <<- NULL
    }
    
    getMatrix <- function() {
        x
    }
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(setMatrix  = setMatrix,
         getMatrix  = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting inverse from cached data")
        return(inv)
    }
    mtx <- x$getMatrix()
    inv <- solve(mtx)
    x$setInverse(inv)
    inv
}
