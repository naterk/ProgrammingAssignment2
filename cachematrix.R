## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {          ## Store an empty matrix unless caller provides one
    inverse <- NULL                                  ## Set cahced index to NULL to force a recalculation
    
    setMatrix  <- function(mtx) {                    ## Function caches the caller supplied matrix
        x       <<- mtx                              ## storing the matrix in function variable x
        inverse <<- NULL                             ## and resetting the cached inverse to NULL
    }                                                ## in order to force a recalculation of the inverse
    getMatrix  <- function()    { x               }  ## Function returns the cached matrix
    setInverse <- function(inv) { inverse <<- inv }  ## Function caches the caller supplied inverse matrix
    getInverse <- function()    { inverse         }  ## Function returns the cached inverse matrix
    
    list(setMatrix  = setMatrix,                     ## Return a list of matrix functions to the caller.
         getMatrix  = getMatrix,                     ## Each function is assigned a name in the list
         setInverse = setInverse,                    ## that matches the function name.  They are called
         getInverse = getInverse)                    ## using <user var>$<function name>()
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                     ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()                            ## Retrieve the cached inverse from x
    if (!is.null(inv)) {                             ## If it is not NULL, then it has already been calculated
        message("getting inverse from cached data")  ## Display a message indicating cached data is being used.
        return(inv)                                  ## Return the cached inverse and exit the function.
    }
    mtx <- x$getMatrix()                             ## The inverse has to be computed, retrieve the cached matrix
    inv <- solve(mtx)                                ## Compute the inverse matrix
    x$setInverse(inv)                                ## Cache the inverse in x
    inv                                              ## Return the inverse to the caller
}
