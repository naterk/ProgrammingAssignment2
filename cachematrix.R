##
## This project defines two functions for creating and caching a matrix and its inverse.
##
## The first function 'makeCacheMatrix()' provides persistent storage for a caller-supplied
## square, invertible matrix and its inverse.  It returns a list of functions that can
## be used to set and get the matrix and set and get its inverse.
##
## The second function 'cacheSolve()' computes the inverse of the matrix created with
## 'makeCacheMatrix()'.  It first checks to see if the inverse has already been 
## computed, and it if has, returns the saved version to the caller.  Otherwise, it
## computes the inverse of the matrix and stores the value using the setInverse()
## function defined in 'makeCachMatrix()'.
##
## Sample usage:
##
## t1 <- matrix(c(3,5,2,6,4,2,7,3,6),nrow=3,ncol=3)  ## Create a square invertible matrix
## m <- makeCacheMatrix(t)                           ## Create a new cached matrix initialized to 't1'
## cacheSolve(m)                                     ## Compute the inverse and cache the result
## t2 <- matrix(c(5,2,3,1,6,3,8,8,4),nrow=3,ncol=3)  ## Create a second matrix
## m$setMatrix(t2)                                   ## Replace the original matrix with a new one
## cacheSolve(m)                                     ## Compute the inverse of the second matrix


##
## makeCacheMatrix()
##
## Purpose
##    Stores a caller supplied matrix and its inverse.  Implements functions allowing the
##    matrix and inverse to be stored and retrieved.
##
## Parameters
##    'x'              : A caller supplied square, invertible matrix (optional), defaults to an empty matrix
##
## Persistent Storage:
##    'x'              : Stores the caller supplied square, invertible matrix
##    'inverse'        : Stores the inverse of 'x'
##
## Functions
##    'setMatrix()'    : Replaces the cached matrix with a new caller-supplied matrix
##    'getMatrix()'    : Retrieves the previously cached matrix from persistent memory
##    'setInverse()'   : Stores the caller supplied inverse matrix
##    'getInverse()'   : Retrieves the previously cached inverse.  Returns NULL if no inverse exists.
##
## Returns
##    A list of the above functions
##

makeCacheMatrix <- function(x = matrix()) {          ## Store an empty matrix unless caller provides one
    inverse <- NULL                                  ## Set cached inverse to NULL to force a recalculation
    
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


##
## cacheSolve()
##
## Purpose
##    Computes the inverse of the matrix stored in the above function and caches it for future
##    use.  Retrieves a previously cached inverse when available.
##
## Parameters
##    'x'              : A "cached matrix" created by the 'makeCacheMatrix()' function
##
## Returns
##    The inverse of the matrix cached in 'x'.
##

cacheSolve <- function(x, ...) {                     ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()                            ## Retrieve the cached inverse from x
    if (!is.null(inv)) {                             ## If it is not NULL, then it has already been calculated
        message("getting inverse from cached data")  ## Display a message indicating cached data is being used.
        return(inv)                                  ## Return the cached inverse and exit the function.
    }
    mtx <- x$getMatrix()                             ## The inverse has to be computed, retrieve the cached matrix
    inv <- solve(mtx)                                ## Compute the inverse matrix
    x$setInverse(inv)                                ## Cache the inverse for future use
    inv                                              ## Return the inverse to the caller
}
