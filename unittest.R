##
## Unit Test Script for makeCacheMatrix() and cacheSolve()
##
## Execute with 'source("unittest.R", echo=TRUE, max.deparse.length=500)

##
## 1. Create a new cache matrix using the defaults.  Verify that the cahced
##    matrix is empty (1x1 matrix with NA value) and that the cached inverse
##    is NULL.
##

t <- makeCacheMatrix()
t$getMatrix()
t$getInverse()

##
## 2. Create a new cache matrix initialized to a 3x3 matrix.  Verify that the
##    cached matrix is identical to the source and that the cached inverse is
##    NULL.
##

m1 <- matrix(c(1,5,2,6,8,3,6,4,3),3,3)
t <- makeCacheMatrix(m1)
identical(t$getMatrix(),m1)
t$getInverse()

##
## 3. Compute the inverse of the cached matrix stored in t.  Attempt to compute
##    the matrix a second time and verify that the cached version of the inverse
##    is used.
##

cacheSolve(t)
cacheSolve(t)

##
## 4.  Verify that the cached inverse is in fact the inverse of the original matrix m
##     by multiplying the inverse times the original.  The result should be the 3x3
##     identity matrix.
##

round(m1 %*% t$getInverse(),3)

##
## 5.  Replace the cached matrix in t with a new 3x3 matrix.  Verify that the
##     new matrix is stored correctly and that the cached inverse has been set
##     back to null.

m2 <- matrix(c(5,2,6,2,7,4,1,3,5),3,3)
t$setMatrix(m2)
identical(m2,t$getMatrix())
t$getInverse()

##
## 6.  Compute the inverse of the new cached matrix.  Recompute it and verify that the
##     cached version is used the second time.  Demonstrate that this is the inverse of
##     m2.

cacheSolve(t)
cacheSolve(t)
round(m2 %*% t$getInverse(),3)