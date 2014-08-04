## Matrix inversion is an expensive computation, especially when dealing
## with large matrices!  The following two functions 'makeCacheMatrix'
## and 'cacheSolve' will save a copy of the computed inverse of a matrix X
## in the memory cache for later use.  This cuts down on expensive 
## computations in the case of repeated matrix inversions of X.
##
##
## EXAMPLE:
## After sourcing the script 'cachematrix.R' you may try the following 
## small matrix example on the command line, just to make sure you believe
## these functions deal with a matrix and its inverse:
##
##> mat <- matrix(rnorm(16), c(4,4))
##> mCM <- makeCacheMatrix(mat)
##> mat_inv <- cacheSolve(mCM)
##
## Then verify that the following matrix multiplication returns 
## the identity matrix:
##
##> mat %*% mat_inv
##
## The result of the multiplication should show only 1's along the diagonal.
## Don't worry if you see numbers like *e-17, these numbers are regarded as
## zero due to machine precision.
##
##
## Created by: Cory R Robinson on 07/23/2014
##
## Based on a script for caching the mean of a vector by
## Roger Peng, Ph.D. -- Johns Hopkins University
## https://github.com/rdpeng/ProgrammingAssignment2
##
##-----------------------------------------------------------------------------


## The function 'makeCacheMatrix' creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y  # reassigns 'x' to the value 'y'
        inv <<- NULL
    }
    get <- function() x  # 'get' is a function that returns the value 'x'
    setinverse <- function(inverse) inv <<- inverse  # 'setinverse' is a 
                                    # function that takes an argument 'inverse'
                                    # and returns the value 'inv' which is
                                    # reassigned to 'inverse'
    getinverse <- function() inv  # 'getinverse' is a function that returns
                                  # the value 'inv'
    
    # This frunction returns a list of the arguments below, which may be used 
    # when 'cacheSolve' calls the name assigned to 'makeCacheMatrix'
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}




## The function 'cacheSolve' computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # if the inverse is not empty, then we retrieve it from the cache
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # otherwise, we need to compute the inverse of the matrix and cache it.
    data <- x$get()  # get the matrix to compute its inverse
    inv <- solve(data, ...)  # solve() computes the inverse of a matrix
    x$setinverse(inv)  
    inv
}
